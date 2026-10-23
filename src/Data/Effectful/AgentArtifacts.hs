module Data.Effectful.AgentArtifacts (AgentArtifacts (..), Asset (..), ArtifactFailure (..), RetainedFix, retainedFix, MediaBytes, verifiedBytes, PublicationArchive, archiveBytes, configuredBucket, assetFile, retainFixMedia, readFixMedia, packageFixMedia, publicationDescription, runAgentArtifactsReal, runAgentArtifactsTest) where

import Conduit (runConduit, (.|))
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isHexDigit)
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.CodeReview qualified as Agent
import Data.Effectful.CodeReview.Artifacts qualified as Media
import Data.Effectful.CodeReview.Types (Asset (..), assetFile, revisionText)
import Data.Effectful.GitPublication.Native qualified as Publication
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Network.Minio qualified as Minio
import Network.URI (URI (..), URIAuth (..), parseURI)
import Pkg.Minio qualified as Storage
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (nullStream, proc, readProcessStdout_, setStderr, setStdin)
import UnliftIO.Exception (tryAny)
import UnliftIO.Timeout qualified as Timeout
import "cryptonite" Crypto.Hash (Digest, SHA256, hashlazy)


data ArtifactFailure = StorageUnavailable | ArtifactUnavailable | InvalidArtifact | PublicationUnavailable
  deriving stock (Eq, Show)


newtype RetainedFix = RetainedFix Agent.ExplainedFix


retainedFix :: RetainedFix -> Agent.ExplainedFix
retainedFix (RetainedFix explained) = explained


newtype MediaBytes = MediaBytes LBS.ByteString


verifiedBytes :: MediaBytes -> LBS.ByteString
verifiedBytes (MediaBytes bytes) = bytes


newtype PublicationArchive = PublicationArchive LBS.ByteString


archiveBytes :: PublicationArchive -> LBS.ByteString
archiveBytes (PublicationArchive bytes) = bytes


configuredBucket :: EnvConfig -> Either ArtifactFailure Text
configuredBucket config = case config.impactReviewArtifactBucket of
  Just bucket | not (T.null $ T.strip bucket) && not (T.null $ T.strip config.s3Endpoint) -> Right bucket
  _ -> Left StorageUnavailable


data AgentArtifacts :: Effect where
  RetainFixMedia :: Agent.ExplainedFix -> AgentArtifacts m (Either ArtifactFailure RetainedFix)
  ReadFixMedia :: Agent.Explanation -> Asset -> AgentArtifacts m (Either ArtifactFailure MediaBytes)
  PackageFixMedia :: Agent.FixResult -> Agent.Explanation -> Text -> AgentArtifacts m (Either ArtifactFailure PublicationArchive)


type instance DispatchOf AgentArtifacts = 'Dynamic


makeEffect ''AgentArtifacts


runAgentArtifactsReal :: IOE :> es => AuthContext -> Eff (AgentArtifacts ': es) a -> Eff es a
runAgentArtifactsReal ctx = interpret_ \case
  RetainFixMedia explained -> liftIO $ perform \bucket connection -> do
    let explanation = Agent.fixExplanation explained
    sizes <- forM [minBound .. maxBound] \asset -> do
      key <- hoistEither $ objectKey explanation asset
      bytes <- ExceptT $ load explanation asset
      void $ storage connection $ Minio.putObject bucket key (CC.sourceLazy $ verifiedBytes bytes) (Just $ LBS.length $ verifiedBytes bytes) Minio.defaultPutObjectOptions
      pure $ LBS.length $ verifiedBytes bytes
    unless (fromIntegral (sum sizes) == explanation.mediaBytes) $ hoistEither $ Left InvalidArtifact
    pure $ RetainedFix explained
  ReadFixMedia explanation asset -> liftIO $ perform \bucket connection -> readObject bucket connection explanation asset
  PackageFixMedia result explanation origin -> liftIO $ perform \bucket connection -> do
    files <- hoistEither $ publicationFiles result explanation origin
    ExceptT $ withSystemTempDirectory "monoscope-publication" \directory -> runExceptT do
      liftIO $ for_ files \(name, bytes) -> writeFileLBS (directory </> name) bytes
      sizes <- forM [minBound .. maxBound] \asset -> do
        bytes <- verifiedBytes <$> readObject bucket connection explanation asset
        liftIO $ writeFileLBS (directory </> toString (assetFile asset)) bytes
        pure $ LBS.length bytes
      unless (fromIntegral (sum sizes) == explanation.mediaBytes) $ hoistEither $ Left InvalidArtifact
      outcome <- liftIO $ tryAny $ readProcessStdout_ $ setStdin nullStream $ setStderr nullStream $ proc "tar" $ ["-cf", "-", "-C", directory, "--"] <> map fst files <> map (toString . assetFile) [minBound .. maxBound]
      either (const $ hoistEither $ Left PublicationUnavailable) (pure . PublicationArchive) outcome
  where
    readObject bucket connection explanation asset = do
      key <- hoistEither $ objectKey explanation asset
      bytes <- storage connection do
        response <- Minio.getObject bucket key Minio.defaultGetObjectOptions
        runConduit $ Minio.gorObjectStream response .| CC.takeE (32 * 1024 * 1024 + 1) .| CC.sinkLazy
      hoistEither $ checkedBytes explanation asset bytes
    perform :: (Minio.Bucket -> Minio.ConnectInfo -> ExceptT ArtifactFailure IO a) -> IO (Either ArtifactFailure a)
    perform action = do
      result <- tryAny $ Timeout.timeout (120 * 1000000) $ runExceptT do
        bucket <- hoistEither $ configuredBucket ctx.config
        let credentials = Minio.CredentialValue (fromString $ toString ctx.config.s3AccessKey) (fromString $ toString ctx.config.s3SecretKey) Nothing
            connection = Minio.setCreds credentials $ Minio.setRegion ctx.config.s3Region $ fromString $ toString ctx.config.s3Endpoint
        action bucket connection
      pure $ case result of Right (Just value) -> value; _ -> Left StorageUnavailable
    storage :: Minio.ConnectInfo -> Minio.Minio a -> ExceptT ArtifactFailure IO a
    storage connection action = ExceptT $ first (const StorageUnavailable) <$> Storage.runMinio ctx.s3HttpManager connection action
    load explanation asset = case ctx.config.impactReviewUsageDir of
      Nothing -> pure $ Left ArtifactUnavailable
      Just directory -> do
        bytes <- Media.readExplanationMedia directory explanation (assetFile asset)
        pure $ first (const ArtifactUnavailable) bytes >>= checkedBytes explanation asset . toLazy

    publicationFiles :: Agent.FixResult -> Agent.Explanation -> Text -> Either ArtifactFailure [(FilePath, LBS.ByteString)]
    publicationFiles result explanation origin = do
      description <- publicationDescription result explanation origin
      pure
        [ ("description.md", encodeUtf8 description)
        , ("evidence.json", AE.encode $ AE.object ["result" AE..= result, "explanation" AE..= explanation])
        , ("PUBLISHING.md", "This private package is prepared for review; it has not been published.\n\nOnly the retained regression comparison is verified here. Repository review and required checks still apply.\n\nNative uploads need a GitHub CLI with --attach and a supported upload credential. Most GitHub App tokens are unsupported. With the verified candidate already on the PR branch, use:\n\n    gh pr edit PR_NUMBER --repo OWNER/REPO --body-file description.md --attach explanation.mp4 --attach cause.png\n\nThe video and diagram will be copied to GitHub. The captions and original SVG remain in this package and the authenticated player.\n")
        ]


publicationDescription :: Agent.FixResult -> Agent.Explanation -> Text -> Either ArtifactFailure Text
publicationDescription result explanation origin = do
  unless ((result.projectId, result.runId, result.issueId, result.candidateId, result.head, result.checkId, result.regressionSha256) == (explanation.projectId, explanation.runId, explanation.issueId, explanation.candidateId, explanation.head, explanation.checkId, explanation.regressionSha256)) $ Left InvalidArtifact
  case parseURI $ toString origin of
    Just uri | uri.uriScheme `elem` ["http:", "https:"], Just authority <- uri.uriAuthority, null authority.uriUserInfo, not (null authority.uriRegName), null uri.uriQuery, null uri.uriFragment -> pass
    _ -> Left PublicationUnavailable
  let project = T.dropWhileEnd (== '/') origin <> "/p/" <> result.projectId.toText
      description =
        unlines
          [ "[Issue](<" <> project <> "/issues/" <> result.issueId.toText <> ">) · [Verification evidence and captions](<" <> project <> "/issue-fixes/" <> result.runId.toText <> ">)"
          , ""
          , "**Cause (inferred):** " <> code result.rootCause
          , ""
          , "**Change:** " <> code result.summary
          , ""
          , "**Verification:** Same retained regression fails on base " <> code (revisionText result.base) <> " and passes on candidate " <> code (revisionText result.head) <> "."
          , ""
          , Publication.attachmentReference Publication.Video
          , ""
          , Publication.attachmentReference Publication.Diagram
          , ""
          , Publication.publicationMarker result explanation
          ]
  pure description
  where
    code value =
      let text = unwords $ words value
          ticks = T.replicate (1 + foldl' max 0 [T.length part | part <- T.group text, T.isPrefixOf "`" part]) "`"
       in ticks <> " " <> text <> " " <> ticks


runAgentArtifactsTest :: (Agent.Explanation -> Either ArtifactFailure ()) -> (Agent.Explanation -> Asset -> Either ArtifactFailure LBS.ByteString) -> Eff (AgentArtifacts ': es) a -> Eff es a
runAgentArtifactsTest retain read = interpret_ \case
  RetainFixMedia explained -> pure $ RetainedFix explained <$ retain (Agent.fixExplanation explained)
  ReadFixMedia explanation asset -> pure $ read explanation asset >>= checkedBytes explanation asset
  PackageFixMedia{} -> pure $ Left PublicationUnavailable


checkedBytes :: Agent.Explanation -> Asset -> LBS.ByteString -> Either ArtifactFailure MediaBytes
checkedBytes explanation asset bytes = do
  unless (not (LBS.null bytes) && LBS.length bytes <= 32 * 1024 * 1024 && Map.lookup (assetFile asset) explanation.files == Just (show (hashlazy bytes :: Digest SHA256)) && (asset /= Video || fromIntegral (LBS.length bytes) == explanation.videoBytes)) $ Left InvalidArtifact
  pure $ MediaBytes bytes


objectKey :: Agent.Explanation -> Asset -> Either ArtifactFailure Text
objectKey explanation asset = do
  digest <- maybeToRight InvalidArtifact $ Map.lookup (assetFile asset) explanation.files
  unless (all (\value -> T.length value == 64 && T.all isHexDigit value) ([explanation.source, digest] :: [Text])) $ Left InvalidArtifact
  pure $ T.intercalate "/" ["agent-explanations", "v1", explanation.projectId.toText, explanation.runId.toText, explanation.candidateId.toText, explanation.source, digest, assetFile asset]
