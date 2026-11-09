module Data.Effectful.CodeReview.Explanation (renderExplanation, readExplanationMedia, proofScript, proofCast, proofFailure, renderProof) where

import Conduit (runConduit, (.|))
import Data.Aeson qualified as AE
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isControl, isDigit)
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), readPrivateArtifact, sha256, verifyArtifacts)
import Data.Effectful.CodeReview.Types hiding (sha256)
import Data.FileEmbed (embedFile)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing, removePathForcibly, renameDirectory)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath ((</>))
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Unistd (fileSynchronise)
import System.Process.Typed (ExitCode (..), createPipe, getStderr, getStdout, nullStream, proc, setEnv, setStderr, setStdin, setStdout, setWorkingDir, waitExitCode, withProcessTerm)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (bracket, throwIO, tryAny)
import UnliftIO.Timeout qualified as Timeout
import "base64" Data.ByteString.Base64 qualified as Base64


newtype Assertion = Assertion {failureMessage :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Stream = Stream {codec_name :: Text, width :: Word16, height :: Word16}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


newtype Format = Format {duration :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Probe = Probe {streams :: [Stream], format :: Format}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Scene = Scene {name :: Text, title :: Text, heading :: Text, detail :: Text, note :: Text, accent :: Text}


renderExplanation :: ArtifactStore -> ArtifactRequest -> IO (Either Text Explanation)
renderExplanation store request = withinBudget $ do
  either (const invalid) pure =<< verifyArtifacts store request
  root <- canonicalizePath store.usageDir
  let result = request.result
      checks = root </> "fixes" </> toString result.runId.toText </> "checks"
      captured = checks </> "candidates" </> toString result.candidateId.toText
  assertion <- decode @Assertion =<< readPrivateArtifact (checks </> toString result.checkId.toText) "report.json" 65536
  let cause = result.rootCause
      change = result.summary
      before = assertion.failureMessage
      bounded limit value = not (T.null $ T.strip value) && T.length value <= limit && not (T.any (\c -> isControl c && c /= '\n' && c /= '\t') value)
  ensure $ bounded 240 cause && bounded 180 change && bounded 360 before
  let renderer = sha256 $ $(embedFile "src/Data/Effectful/CodeReview/Explanation.hs") <> $(embedFile "src/Data/Effectful/CodeReview/Artifacts.hs") <> $(embedFile "src/Data/Effectful/CodeReview/Types.hs")
      source = sha256 $ toStrict $ AE.encode $ AE.object ["version" AE..= (1 :: Int), "rendererSha256" AE..= renderer, "result" AE..= result, "executions" AE..= (request.baseline.id, request.candidate.id, request.baseline.imageBuildId), "presentation" AE..= (cause, change, before)]
      parent = captured </> "explanations"
      directory = parent </> toString source
  createDirectoryIfMissing True parent
  canonicalizePath parent >>= ensure . (== parent)
  Posix.setFileMode parent 0o700
  withFileLock (parent </> ".native-render.lock") Exclusive \_ -> do
    let matches (manifest :: Explanation) = (manifest.source, manifest.rendererSha256, manifest.projectId, manifest.runId, manifest.issueId, manifest.candidateId, manifest.head, manifest.checkId, manifest.regressionSha256, manifest.version, manifest.durationMs, manifest.width, manifest.height) == (source, renderer, result.projectId, result.runId, result.issueId, result.candidateId, result.head, result.checkId, result.regressionSha256, 1, 36000, 1280, 720)
    cached <- tryAny do
      manifest <- decode @Explanation =<< readPrivateArtifact directory "manifest.json" 65536
      ensure $ matches manifest && validExplanation result manifest
      sizes <- forM (Map.toList manifest.files) \(name, digest) -> do
        bytes <- readPrivateArtifact directory (toString name) (32 * 1048576)
        ensure $ sha256 bytes == digest
        pure (name, fromIntegral $ BS.length bytes :: Word64)
      ensure $ sum (map snd sizes) == manifest.mediaBytes && lookup "explanation.mp4" sizes == Just manifest.videoBytes
      pure manifest
    case cached of
      Right manifest -> pure manifest
      Left _ -> withTempDirectory parent ".native-render-" \temporary -> do
        started <- getMonotonicTimeNSec
        environment <- filter ((== "PATH") . fst) <$> getEnvironment
        let command program args = withProcessTerm (setStdin nullStream $ setStdout createPipe $ setStderr createPipe $ setEnv (environment <> [("LC_ALL", "C")]) $ setWorkingDir temporary $ proc program args) \process -> do
              let capture handle = do
                    bytes <- runConduit $ CC.sourceHandle handle .| CC.takeE (1048576 + 1) .| CC.sinkLazy
                    ensure $ BS.length (toStrict bytes) <= 1048576
                    pure $ decodeUtf8 @Text bytes
              output <- concurrently (capture $ getStdout process) (capture $ getStderr process)
              waitExitCode process >>= ensure . (== ExitSuccess)
              pure output
            write :: FilePath -> Text -> IO ()
            write name value = writeFileBS (temporary </> name) $ encodeUtf8 value
            wrap width = T.intercalate "\n" . concatMap splitLine . T.splitOn "\n"
              where
                splitLine text
                  | T.length text <= width = [text]
                  | otherwise =
                      let (prefix, suffix) = T.splitAt width text
                          (wholeWords, lastWord) = T.breakOnEnd " " prefix
                          (part, rest) = if T.null wholeWords then (prefix, suffix) else (T.dropEnd 1 wholeWords, lastWord <> suffix)
                       in part : splitLine rest
            quoted = decodeUtf8 . AE.encode @Text
            graph body = "digraph G { graph [bgcolor=\"#0f172a\", pad=\"0.65\", nodesep=\"0.5\", ranksep=\"0.5\"]; node [fontname=\"Helvetica\", fontsize=26, fontcolor=\"#f8fafc\", color=\"#334155\", style=\"rounded,filled\", fillcolor=\"#172338\", margin=\"0.3,0.22\", shape=box]; edge [color=\"#94a3b8\", arrowsize=0.9]; " <> body <> " }"
            short = T.take 10 . revisionText
            scenes =
              [ Scene "before" "01  /  REPRODUCE" "The regression fails on base" before ("Recorded assertion · exit " <> maybe "unknown" show request.baseline.exitCode <> " · " <> short result.base) "#fb7185"
              , Scene "cause" "02  /  ROOT CAUSE" "Why the behavior failed" cause "Agent explanation · inference from the investigated code" "#a5b4fc"
              , Scene "change" "03  /  THE FIX" "The candidate keeps the regression" change "Same test · same image · fresh verifier VMs" "#a5b4fc"
              , Scene "after" "04  /  VERIFY" "The unchanged test passes on candidate" "Verified exit code: 0." ("Recorded exit status · exit 0 · " <> short result.head <> "\nCounterfactual: the same test still fails on base") "#34d399"
              ]
        write "diagram.dot" $ graph $ "rankdir=LR; base [label=" <> quoted ("Base fails\n" <> short result.base) <> ", color=\"#fb7185\"]; cause [label=" <> quoted ("Inferred cause\n" <> wrap 30 cause) <> "]; change [label=" <> quoted ("Change\n" <> wrap 30 change) <> "]; fixed [label=" <> quoted ("Same test passes\n" <> short result.head) <> ", color=\"#34d399\"]; base -> cause -> change -> fixed;"
        void $ command "dot" ["-Tsvg", "diagram.dot", "-o", "diagram.svg"]
        for_ scenes \scene ->
          if scene.name == "cause"
            then void $ command "dot" ["-Tpng", "diagram.dot", "-o", "cause.png"]
            else do
              write (toString scene.name <> ".dot") $ graph $ "a [label=" <> quoted scene.title <> ", shape=plain, fontsize=20, fontcolor=" <> quoted scene.accent <> "]; b [label=" <> quoted scene.heading <> ", shape=plain, fontsize=32]; c [label=" <> quoted (wrap 60 scene.detail) <> ", fontname=" <> quoted (if scene.name == "before" || scene.name == "after" then "Courier" else "Helvetica") <> ", color=" <> quoted scene.accent <> "]; d [label=" <> quoted scene.note <> ", shape=plain, fontsize=18, fontcolor=\"#cbd5e1\"]; e [label=" <> quoted ("Monoscope · retained verification evidence · " <> T.take 8 result.checkId.toText) <> ", shape=plain, fontsize=14, fontcolor=\"#94a3b8\"]; a -> b -> c -> d -> e [style=invis];"
              void $ command "dot" ["-Tpng", toString scene.name <> ".dot", "-o", toString scene.name <> ".png"]
        let stamp :: Int -> Text
            stamp seconds = "00:00:" <> T.justifyRight 2 '0' (show seconds) <> ".000"
            escaped = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;" . T.intercalate "\n" . filter (not . T.null . T.strip) . lines
        write "captions.vtt" $ "WEBVTT\n\n" <> T.intercalate "\n" [stamp (index * 9) <> " --> " <> stamp ((index + 1) * 9) <> "\n" <> scene.heading <> "\n" <> escaped scene.detail <> "\n" | (index, scene) <- zip [0 ..] scenes]
        let inputs = concatMap (\scene -> ["-loop", "1", "-t", "9", "-i", toString scene.name <> ".png"]) scenes
            filters = T.intercalate ";" ["[" <> show index <> ":v]scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2:color=0x0f172a,setsar=1,fps=24[v" <> show index <> "]" | index <- [0 .. 3 :: Int]] <> ";[v0][v1][v2][v3]concat=n=4:v=1:a=0[out]"
        void $ command "ffmpeg" $ ["-hide_banner", "-loglevel", "error"] <> inputs <> ["-filter_complex", toString filters, "-map", "[out]", "-an", "-c:v", "libx264", "-preset", "veryfast", "-crf", "24", "-pix_fmt", "yuv420p", "-movflags", "+faststart", "-t", "36", "explanation.mp4"]
        probe <- decode @Probe . encodeUtf8 . fst =<< command "ffprobe" ["-v", "error", "-show_streams", "-show_format", "-of", "json", "explanation.mp4"]
        ensure $ case probe.streams of [stream] -> (stream.codec_name, stream.width, stream.height) == ("h264", 1280, 720) && readMaybe @Double (toString probe.format.duration) == Just 36; _ -> False
        ffmpeg <- maybe invalid pure . viaNonEmpty Relude.head . lines . fst =<< command "ffmpeg" ["-version"]
        graphviz <- T.strip . snd <$> command "dot" ["-V"]
        files <- forM mediaFiles \name -> do
          bytes <- readPrivateArtifact temporary (toString name) (32 * 1048576)
          pure (name, (sha256 bytes, fromIntegral $ BS.length bytes :: Word64))
        elapsed <- subtract started <$> getMonotonicTimeNSec
        videoBytes <- maybe invalid (pure . snd) $ lookup "explanation.mp4" files
        let manifest = Explanation{version = 1, source, rendererSha256 = renderer, projectId = result.projectId, runId = result.runId, issueId = result.issueId, candidateId = result.candidateId, head = result.head, checkId = result.checkId, regressionSha256 = result.regressionSha256, durationMs = 36000, width = 1280, height = 720, renderMs = max 1 $ (elapsed + 999999) `div` 1000000, mediaBytes = sum $ map (snd . snd) files, videoBytes, resourceCostStatus = Unpriced, tools = Map.fromList [("ffmpeg", ffmpeg), ("graphviz", graphviz)], files = Map.fromList $ map (second fst) files}
        writeFileLBS (temporary </> "manifest.json") $ AE.encode manifest
        for_ ("manifest.json" : map toString mediaFiles) $ sync . (temporary </>)
        sync temporary
        removePathForcibly directory
        renameDirectory temporary directory
        sync parent
        pure manifest


-- | The frozen check behind a proof: @proof.sh@ with the command as its base64 argument.
-- The base64 alphabet has no shell metacharacters, so a double-quoted literal is a safe quote.
--
-- >>> map (`T.isInfixOf` proofScript "echo hi") ["set -- \"ZWNobyBoaQ==\"", "echo hi", castBegin, castEnd, proofFailure]
-- [True,False,True,True,True]
proofScript :: Text -> Text
proofScript command = "set -- " <> show @Text (extractBase64 $ Base64.encodeBase64 $ encodeUtf8 command) <> "\n" <> decodeUtf8 $(embedFile "src/Data/Effectful/CodeReview/proof.sh")


castBegin :: Text
castBegin = "::monoscope-cast::"


castEnd :: Text
castEnd = "::monoscope-cast-end::"


proofFailure :: Text
proofFailure = "monoscope-proof-failed"


-- | A cast's size limit; @proof.sh@ prints nothing larger, and the host does not trust that.
castLimit :: Int
castLimit = 524288


-- | The recorded cast in a proof check's log, and the terminal output it shows.
--
-- >>> let cast = "{\"version\": 2, \"width\": 80, \"height\": 24}\n[0.1, \"o\", \"hello\\r\\n\"]\n"
-- >>> let logged encoded = "noise\n::monoscope-cast::\n" <> encoded <> "\n::monoscope-cast-end::\n"
-- >>> fmap snd $ proofCast $ logged $ extractBase64 $ Base64.encodeBase64' cast
-- Right "hello\r\n"
-- >>> proofCast "asciinema: no tty"
-- Left "The proof produced no recording. Log tail: asciinema: no tty"
-- >>> proofCast $ logged $ extractBase64 $ Base64.encodeBase64' "not a cast"
-- Left "The proof recording is not an asciinema cast"
-- >>> either ("not valid base64" `T.isInfixOf`) (const False) $ proofCast $ logged "%%%"
-- True
-- >>> proofCast $ logged $ BS.replicate 800000 65
-- Left "The proof recording exceeds its size limit"
proofCast :: ByteString -> Either Text (ByteString, Text)
proofCast logBytes = do
  encoded <- maybeToRight ("The proof produced no recording. Log tail: " <> decodeUtf8 (BS.takeEnd 600 logBytes)) $ find (not . BS.null) $ drop 1 $ BS8.lines $ snd $ BS.breakSubstring (encodeUtf8 castBegin) logBytes
  when (BS.length encoded > castLimit * 4 `div` 3 + 4) $ Left "The proof recording exceeds its size limit"
  cast <- first ("The proof recording is not valid base64: " <>) $ Base64.decodeBase64Untyped encoded
  case BS8.lines cast of
    header : events
      | Just (_ :: AE.Object) <- AE.decodeStrict header ->
          Right (cast, T.concat [output | Just (AE.Array (toList -> [_, AE.String "o", AE.String output])) <- map AE.decodeStrict events])
    _ -> Left "The proof recording is not an asciinema cast"


-- | An animated preview (GitHub inlines GIFs) and a video. Both tools read guest-recorded bytes,
-- so they run unprivileged with bounded time, input duration and output size.
renderProof :: ByteString -> IO (Either Text (ByteString, ByteString))
renderProof cast = do
  outcome <- tryAny $ Timeout.timeout (120 * 1000000) $ withSystemTempDirectory "monoscope-proof" \directory -> do
    environment <- filter ((== "PATH") . fst) <$> getEnvironment
    writeFileBS (directory </> "proof.cast") cast
    let run program args = withProcessTerm (setStdin nullStream $ setStdout nullStream $ setStderr nullStream $ setEnv environment $ setWorkingDir directory $ proc program args) waitExitCode >>= \code -> unless (code == ExitSuccess) $ throwIO $ userError $ program <> " exited with " <> show code
    run "agg" ["--idle-time-limit", "2", "--fps-cap", "12", "--font-size", "16", "--theme", "github-dark", "proof.cast", "proof.gif"]
    run "ffmpeg" ["-hide_banner", "-loglevel", "error", "-nostdin", "-y", "-t", "120", "-i", "proof.gif", "-movflags", "+faststart", "-pix_fmt", "yuv420p", "-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2", "-c:v", "libx264", "-crf", "26", "-fs", show mediaLimit, "proof.mp4"]
    join bitraverse (\name -> readPrivateArtifact directory name mediaLimit) ("proof.gif", "proof.mp4")
  pure $ case outcome of
    Right (Just media) -> Right media
    Right Nothing -> Left "Rendering the proof recording timed out"
    Left failure -> Left $ "The proof recording could not be rendered: " <> toText (displayException failure)
  where
    mediaLimit = 8 * 1048576


readExplanationMedia :: FilePath -> Explanation -> Text -> IO (Either Text ByteString)
readExplanationMedia usageDir explanation asset = withinBudget do
  ensure $ asset `elem` mediaFiles && T.length explanation.source == 64 && T.all (\c -> isDigit c || c >= 'a' && c <= 'f') explanation.source
  root <- canonicalizePath usageDir
  let directory = root </> "fixes" </> toString explanation.runId.toText </> "checks" </> "candidates" </> toString explanation.candidateId.toText </> "explanations" </> toString explanation.source
  retained <- decode @Explanation =<< readPrivateArtifact directory "manifest.json" 65536
  ensure $ retained == explanation
  bytes <- readPrivateArtifact directory (toString asset) (32 * 1048576)
  ensure $ Map.lookup asset explanation.files == Just (sha256 bytes)
  pure bytes


mediaFiles :: [Text]
mediaFiles = ["explanation.mp4", "diagram.svg", "captions.vtt", "before.png", "cause.png", "change.png", "after.png"]


withinBudget :: IO a -> IO (Either Text a)
withinBudget action = do
  outcome <- tryAny $ Timeout.timeout (180 * 1000000) action
  pure $ case outcome of Right (Just result) -> Right result; _ -> Left "Verified explanation media could not be prepared"


decode :: AE.FromJSON a => ByteString -> IO a
decode = either (const invalid) pure . AE.eitherDecodeStrict


ensure :: Bool -> IO ()
ensure condition = unless condition invalid


invalid :: IO a
invalid = throwIO $ userError "Invalid retained explanation"


sync :: FilePath -> IO ()
sync path = bracket (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.cloexec = True}) Posix.closeFd fileSynchronise
