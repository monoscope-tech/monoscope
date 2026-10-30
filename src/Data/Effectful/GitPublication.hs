module Data.Effectful.GitPublication (GitPublication (..), Request, requestFor, UploadCredential, uploadCredential, Visibility (..), Stage (..), Failure (..), Authorization (..), MediaUrls (..), PullState (..), Receipt (..), PublishedFix, publicationReceipt, publishFix, runGitPublicationReal, runGitPublicationTest) where

import Data.Aeson qualified as AE
import Data.Char (isAlphaNum, isAscii)
import Data.Effectful.AgentArtifacts qualified as Artifacts
import Data.Effectful.CodeReview qualified as Agent
import Data.Effectful.CodeReview.Artifacts (ArtifactStore)
import Data.Effectful.CodeReview.Types (revisionText)
import Data.Effectful.GitPublication.Native qualified as Native
import Data.Effectful.GitPublication.Types
import Data.Effectful.Wreq qualified as HTTP
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.IssueFixes (CompletedFix, completedFixContents)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Pkg.Git qualified as Git
import Relude
import UnliftIO.Exception (tryAny)
import UnliftIO.Timeout qualified as Timeout
import "cryptonite" Crypto.Hash (Digest, SHA256, hashlazy)
import Prelude (until)


-- No Generic/FromJSON or exported fields: only an authorized completed DB read
-- can supply the capability needed to prepare a publication request.
data Request = Request Agent.ArtifactRequest Agent.Explanation Text Visibility Text Text


instance AE.ToJSON Request where
  toJSON (Request artifacts explanation target visibility description _) = AE.object ["artifacts" AE..= artifacts, "explanation" AE..= explanation, "targetBranch" AE..= target, "visibility" AE..= visibility, "description" AE..= description]


requestFor :: CompletedFix -> Text -> Visibility -> Text -> Either Failure Request
requestFor completed target visibility origin = do
  let (artifacts, explanation) = completedFixContents completed
  unless (not (T.null $ T.strip target) && T.length target <= 200) $ Left InvalidRequest
  repo <- maybeToRight InvalidRequest $ T.stripPrefix "https://github.com/" artifacts.repository >>= T.stripSuffix ".git"
  description <- first (const InvalidRequest) $ Artifacts.publicationDescription artifacts.result explanation origin
  case T.splitOn "/" repo of
    [owner, name] | all (\part -> not (T.null part) && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ['-', '_', '.'])) part) [owner, name] -> pure $ Request artifacts explanation target visibility description repo
    _ -> Left InvalidRequest


newtype UploadCredential = UploadCredential Text


uploadCredential :: GitSync.GitCreds -> Either Failure UploadCredential
uploadCredential = \case
  GitSync.PersonalToken token | not (T.null $ T.strip token) -> Right $ UploadCredential token
  _ -> Left NativeUnavailable


newtype PublishedFix = PublishedFix Receipt


publicationReceipt :: PublishedFix -> Receipt
publicationReceipt (PublishedFix receipt) = receipt


data GitPublication :: Effect where
  PublishFix :: Request -> UploadCredential -> (Stage -> m (Either Failure ())) -> (Receipt -> m (Either Failure ())) -> GitPublication m (Either Failure PublishedFix)


type instance DispatchOf GitPublication = 'Dynamic


makeEffect ''GitPublication


runGitPublicationReal :: (HTTP.HTTP :> es, IOE :> es) => Maybe ArtifactStore -> Maybe FilePath -> Eff (GitPublication ': es) a -> Eff es a
runGitPublicationReal store githubCli = interpret \env -> \case
  PublishFix request@(Request artifacts explanation _ _ _ _) (UploadCredential token) authorize validate -> localSeqUnlift env \unlift -> withSeqEffToIO \runInIO -> case (store, githubCli) of
    (Just configured, Just github) -> do
      outcome <- tryAny $ Timeout.timeout (480 * 1000000) $ do
        published <- Native.publish configured github token artifacts explanation (contextFor request Prepare) (runInIO . unlift . authorize)
        case published of
          Left failure -> pure $ Left failure
          Right receipt | validReceipt request receipt -> runInIO $ runExceptT do
            ExceptT $ confirmRemote request token receipt
            ExceptT $ unlift $ validate receipt
            pure $ PublishedFix receipt
          _ -> pure $ Left InvalidReceipt
      pure $ case outcome of Left _ -> Left ProtocolFailure; Right Nothing -> Left DeadlineExceeded; Right (Just result) -> result
    _ -> pure $ Left NotConfigured
  where
    confirmRemote :: HTTP.HTTP :> es => Request -> Text -> Receipt -> Eff es (Either Failure ())
    confirmRemote request@(Request artifacts explanation target visibility _ slug) token receipt = runExceptT do
      conn <- hoistEither $ first (const InvalidRequest) $ Git.mkGitConn Git.GitHub Nothing token
      repo <- hoistEither $ case T.splitOn "/" slug of
        [owner, name] -> Right $ Git.RepoRef owner name target
        _ -> Left InvalidRequest
      access <- ExceptT $ first (const GitHubFailure) <$> Git.getRepositoryAccess conn repo
      unless (access.visibility == visibility) $ hoistEither $ Left VisibilityChanged
      unless access.permissions.push $ hoistEither $ Left AccessRevoked
      remote <- ExceptT $ first (const GitHubFailure) <$> Git.getPublicationPullRequest conn repo receipt.number
      let context = contextFor request Complete
          sameRepo = (== T.toCaseFold slug) . T.toCaseFold
      unless
        ( all sameRepo [access.fullName, remote.head.repo.fullName, remote.base.repo.fullName]
            && remote.number
            == receipt.number
            && T.toCaseFold remote.htmlUrl
            == T.toCaseFold receipt.url
            && remote.head.ref
            == context.branch
            && remote.base.ref
            == target
            && remote.state
            == receipt.state
            && remote.draft
            == receipt.draft
            && (remote.state == Open || remote.merged)
        )
        $ hoistEither
        $ Left PullRequestChanged
      unless
        ( remote.head.sha
            == revisionText artifacts.result.head
            && (remote.merged || remote.base.sha == revisionText artifacts.result.base)
        )
        $ hoistEither
        $ Left SourceChanged
      blocks <- forM ([("video", "explanation.mp4", receipt.assets.video), ("diagram", "cause.png", "![Reproduction, inferred cause, and verified fix](" <> receipt.assets.diagram <> ")")] :: [(Text, Text, Text)]) \(kind, file, line) -> do
        digest <- hoistEither $ maybeToRight InvalidReceipt $ Map.lookup file explanation.files
        pure $ "<!-- monoscope-fix-media:" <> kind <> ":" <> digest <> " -->\n" <> line
      unless (T.strip (T.replace "\r\n" "\n" $ fromMaybe "" remote.body) == context.description <> "\n\n" <> T.intercalate "\n\n" blocks) $ hoistEither $ Left InvalidReceipt


runGitPublicationTest :: (Request -> Either Failure Receipt) -> Eff (GitPublication ': es) a -> Eff es a
runGitPublicationTest respond = interpret \env -> \case
  PublishFix request _ authorize validate -> localSeqUnlift env \unlift -> runExceptT do
    receipt <- hoistEither $ respond request
    ExceptT $ unlift $ authorize Prepare
    unless (validReceipt request receipt) $ hoistEither $ Left InvalidReceipt
    ExceptT $ unlift $ authorize Complete
    ExceptT $ unlift $ validate receipt
    pure $ PublishedFix receipt


contextFor :: Request -> Stage -> Authorization
contextFor (Request artifacts explanation target visibility description repo) stage =
  let result = artifacts.result
      cleaned = unlines $ filter (`notElem` (["![](explanation.mp4)", "![Reproduction, inferred cause, and verified fix](cause.png)"] :: [Text])) $ lines $ T.replace "\r\n" "\n" $ T.strip description
   in Authorization result.projectId result.runId result.head explanation.source (show (hashlazy (AE.encode $ Map.toAscList explanation.files) :: Digest SHA256)) ("monoscope/fix/" <> result.projectId.toText <> "/" <> result.runId.toText <> "/" <> result.candidateId.toText) target repo visibility (T.strip $ until (not . T.isInfixOf "\n\n\n") (T.replace "\n\n\n" "\n\n") cleaned) stage


validReceipt :: Request -> Receipt -> Bool
validReceipt request receipt =
  let context = contextFor request Complete
   in receipt.number
        > 0
        && receipt.head
        == context.head
        && receipt.source
        == context.source
        && receipt.mediaSha256
        == context.mediaSha256
        && T.toLower receipt.url
        == T.toLower ("https://github.com/" <> context.repo <> "/pull/" <> show receipt.number)
        && receipt.assets.video
        /= receipt.assets.diagram
        && all assetUrl [receipt.assets.video, receipt.assets.diagram]
  where
    assetUrl value = case parseURI $ toString value of
      Just uri
        | uri.uriScheme == "https:"
        , Just authority <- uri.uriAuthority
        , authority.uriRegName == "github.com"
        , null authority.uriUserInfo
        , null authority.uriPort
        , null uri.uriQuery
        , null uri.uriFragment ->
            isJust $ T.stripPrefix "/user-attachments/assets/" (toText uri.uriPath) >>= UUID.fromText
      _ -> False
