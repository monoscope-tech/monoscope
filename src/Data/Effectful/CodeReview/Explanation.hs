module Data.Effectful.CodeReview.Explanation (renderExplanation, readExplanationMedia, proofScript, proofCast, proofFailure, Window (..), clipCast, renderProof, BrowserMedia (..), browserMedia, renderVideo) where

import Conduit (runConduit, (.|))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isControl, isDigit)
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), readPrivateArtifact, sha256, verifyArtifacts)
import Data.Effectful.CodeReview.Types hiding (sha256)
import Data.FileEmbed (embedFile)
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Scientific (toRealFloat)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing, getFileSize, removePathForcibly, renameDirectory)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath ((</>))
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Unistd (fileSynchronise)
import System.Process.Typed (ExitCode (..), createPipe, getStderr, getStdout, nullStream, proc, readProcessStdout_, setEnv, setStderr, setStdin, setStdout, setWorkingDir, waitExitCode, withProcessTerm)
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


-- | The frozen check behind a proof: @proof.sh@ with the command and optional setup as base64
-- arguments. The base64 alphabet has no shell metacharacters, so double-quoted literals are safe.
--
-- >>> map (`T.isInfixOf` proofScript Nothing "echo hi") ["set -- \"ZWNobyBoaQ==\" \"\"", "echo hi", "::monoscope-%s::%s", "::monoscope-%s-end::", "emit cast", proofFailure]
-- [True,False,True,True,True,True]
-- >>> "set -- \"ZWNobyBoaQ==\" \"c2VlZA==\"" `T.isInfixOf` proofScript (Just "seed") "echo hi"
-- True
proofScript :: Maybe Text -> Text -> Text
proofScript setup command = "set -- " <> unwords (map (show @Text . extractBase64 . Base64.encodeBase64 . encodeUtf8) [command, fold setup]) <> "\n" <> decodeUtf8 $(embedFile "src/Data/Effectful/CodeReview/proof.sh")


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
  encoded <- maybeToRight ("The proof produced no recording. Log tail: " <> decodeUtf8 (BS.takeEnd 600 logBytes)) $ find (not . BS.null) $ map snd $ logSections "cast" logBytes
  when (BS.length encoded > castLimit * 4 `div` 3 + 4) $ Left "The proof recording exceeds its size limit"
  cast <- first ("The proof recording is not valid base64: " <>) $ Base64.decodeBase64Untyped encoded
  case BS8.lines cast of
    header : events
      | Just (_ :: AE.Object) <- AE.decodeStrict header ->
          Right (cast, T.concat [output | Just (AE.Array (toList -> [_, AE.String "o", AE.String output])) <- map AE.decodeStrict events])
    _ -> Left "The proof recording is not an asciinema cast"


-- | Where a proof's clip sits in the whole recording, in seconds.
data Window = Window {start :: Double, end :: Double, total :: Double}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.ToJSON)


-- | The section of a recording the comment plays, at most eight seconds: from just before the
-- first output containing @highlight@, or the end of the recording when that never prints.
-- Earlier output replays at once, so the clip opens on the full screen. A row under the terminal
-- is a timeline of the whole recording: the clip's section is blue, and a dot marks the moment.
--
-- >>> let cast = "{\"version\":2,\"width\":40,\"height\":2}\n[0.5,\"o\",\"boot\\r\\n\"]\n[12,\"o\",\"== ke\"]\n[12.5,\"o\",\"y\\r\\n\"]\n[30,\"o\",\"done\"]"
-- >>> snd <$> clipCast (Just "key") cast
-- Right (Window {start = 11.0, end = 19.0, total = 30.0})
-- >>> snd <$> clipCast Nothing cast
-- Right (Window {start = 22.0, end = 30.0, total = 30.0})
-- >>> map (fmap (\(t, _ :: Text, _ :: Text) -> t) . AE.decodeStrict @(Double, Text, Text)) . drop 1 . BS8.lines . fst <$> clipCast (Just "key") cast
-- Right [Just 0.0,Just 0.0,Just 0.0,Just 1.0,Just 1.5,Just 8.0]
-- >>> clipCast Nothing "not a cast"
-- Left "The proof recording is not an asciinema cast"
clipCast :: Maybe Text -> ByteString -> Either Text (ByteString, Window)
clipCast highlight cast = case BS8.lines cast of
  header : rest
    | Just meta <- AE.decodeStrict @AE.Object header
    , Just (AE.Number (toRealFloat @Double -> columns)) <- KM.lookup "width" meta
    , Just (AE.Number (toRealFloat @Double -> rows)) <- KM.lookup "height" meta -> do
        let outputs = [(t, text) | Just (AE.Array (toList -> [AE.Number (toRealFloat -> t), AE.String "o", AE.String text])) <- map AE.decodeStrict rest]
            total = maybe 0 (fst . last) (nonEmpty outputs)
            printed = T.concat (map snd outputs)
            -- The time of the output that holds the highlight's first character.
            moment = do
              needle <- mfilter (not . T.null) highlight
              let at = T.length $ fst $ T.breakOn needle printed
              guard (at < T.length printed)
              fst <$> find ((> at) . snd) (zip (map fst outputs) (scanl1 (+) (map (T.length . snd) outputs)))
            window
              | total <= 8 = Window 0 total total
              | Just m <- moment = let from = max 0 (min (m - 1) (total - 8)) in Window from (from + 8) total
              | otherwise = Window (total - 8) total total
            width = floor columns :: Int
            row = floor rows + 1 :: Int
            clock seconds = let whole = floor seconds :: Int in show (whole `div` 60) <> ":" <> T.justifyRight 2 '0' (show (whole `mod` 60))
            timeline now =
              let cells = max 10 (width - 2 - T.length (clock now) - T.length (clock total))
                  cell at = min (cells - 1) $ floor (at / max 1e-9 total * fromIntegral cells)
                  paint i
                    | i == cell now = ("\ESC[97m", "●")
                    | i >= cell window.start && i <= cell window.end = ("\ESC[38;2;59;130;246m", "━")
                    | otherwise = ("\ESC[38;2;100;116;139m", "─")
                  track = foldMap (\run@((color, _) :| _) -> color <> foldMap snd run) $ NE.groupWith fst $ map paint [0 .. cells - 1]
               in "\ESC7\ESC[" <> show row <> ";1H\ESC[2K\ESC[0m" <> clock now <> " " <> track <> "\ESC[0m " <> clock total <> "\ESC8"
            event at text = decodeUtf8 @Text (AE.encode (at, "o" :: Text, text))
            events =
              [event (0 :: Double) ("\ESC[1;" <> show (row - 1) <> "r")]
                <> [event 0 text | (t, text) <- outputs, t < window.start]
                <> [event 0 (timeline window.start)]
                <> [event (t - window.start) (text <> timeline t) | (t, text) <- outputs, t >= window.start, t <= window.end]
                <> [event (window.end - window.start) (timeline window.end)]
        Right (encodeUtf8 $ unlines $ decodeUtf8 (AE.encode $ KM.insert "height" (AE.toJSON row) meta) : events, window)
  _ -> Left "The proof recording is not an asciinema cast"


-- | The clip ('clipCast') as an animated preview (GitHub inlines GIFs) and the whole recording as a
-- video. Both tools read guest-recorded bytes, so they run unprivileged with bounded time, input
-- duration and output size.
renderProof :: ByteString -> ByteString -> IO (Either Text (ByteString, ByteString))
renderProof clip cast = renderMedia cast \directory run -> do
  writeFileBS (directory </> "clip.cast") clip
  for_ [("proof.input", "full.gif"), ("clip.cast", "proof.gif")] \(input, output) -> run "agg" ["--idle-time-limit", "2", "--fps-cap", "12", "--font-size", "16", "--theme", "github-dark", input, output]
  run "ffmpeg" (quiet <> ["-t", "120", "-i", "full.gif", "-movflags", "+faststart", "-pix_fmt", "yuv420p", "-vf", "scale=trunc(iw/2)*2:trunc(ih/2)*2", "-c:v", "libx264", "-crf", "26", "-fs", show mediaLimit, "proof.mp4"])


-- | A browser recording's full video and a preview that plays inline at full comment width, both
-- inside a drawn browser window showing the page's title and URL. The preview is the first 30
-- seconds as an animated WebP (a GIF of a scrolling UI runs to several megabytes), at the sharpest
-- setting that fits under GitHub's image proxy limit. Like a player, it shows a timeline of the
-- whole video, the part it covers, its own position, and the full length to play.
renderVideo :: Maybe (Text, Text) -> ByteString -> IO (Either Text (ByteString, ByteString))
renderVideo page video = renderMedia video \directory run -> do
  -- Text goes to drawtext as UTF-8 files the worker's locale need not encode; expansion=none keeps a URL's % literal.
  for_ [("proof.title", maybe "" fst page), ("proof.url", maybe "" snd page), ("proof.nav", "←    →    ↻"), ("proof.dot", "●")] \(name, text) -> writeFileBS (directory </> name) (encodeUtf8 text)
  let text = ("drawtext=fontfile=/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf:expansion=none:" <>)
      window =
        intercalate
          ","
          [ "drawbox=x=24:y=24:w=1280:h=38:color=0x1F2023:t=fill"
          , "drawbox=x=24:y=62:w=1280:h=40:color=0x35363A:t=fill"
          , text "textfile=proof.dot:fontcolor=0xFF5F57:fontsize=14:x=42:y=36"
          , text "textfile=proof.dot:fontcolor=0xFEBC2E:fontsize=14:x=62:y=36"
          , text "textfile=proof.dot:fontcolor=0x28C840:fontsize=14:x=82:y=36"
          , "drawbox=x=112:y=30:w=260:h=32:color=0x35363A:t=fill"
          , text "textfile=proof.title:fontcolor=white@0.9:fontsize=13:x=128:y=40"
          , text "textfile=proof.nav:fontcolor=white@0.55:fontsize=16:x=42:y=73"
          , "drawbox=x=134:y=68:w=1156:h=28:color=0x202124:t=fill"
          , text "textfile=proof.url:fontcolor=white@0.85:fontsize=14:x=150:y=75"
          ]
  run "ffmpeg" (quiet <> ["-t", "120", "-i", "proof.input", "-f", "lavfi", "-i", "color=c=0xDCE1EA:s=1328x926", "-filter_complex", "[0:v]scale=1280:800:force_original_aspect_ratio=decrease,pad=1280:800:(ow-iw)/2:(oh-ih)/2[page];[1:v]" <> window <> "[frame];[frame][page]overlay=24:102:shortest=1", "-an", "-movflags", "+faststart", "-pix_fmt", "yuv420p", "-c:v", "libx264", "-crf", "24", "-fs", show mediaLimit, "proof.mp4"])
  probed <- readProcessStdout_ (proc "ffprobe" ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", directory </> "proof.mp4"])
  let total = max 1 $ fromMaybe 30 $ readMaybe @Double $ toString $ T.strip $ decodeUtf8 probed
      shown = min 30 total
  let clock t = let n = round t :: Int in show (n `div` 60) <> ":" <> T.justifyRight 2 '0' (show (n `mod` 60))
  for_ [("proof.start", "0:00"), ("proof.end", clock shown), ("proof.label", "▶  Watch full video  ·  " <> clock total)] \(name, label) -> writeFileBS (directory </> name) (encodeUtf8 label)
  let preview (fps, width, quality) = do
        -- A player bar: the clip's start and end times either side of a track of the whole video, with
        -- the clip bracketed in blue and a playhead moving through it.
        let clip = "(iw-420)*" <> show (shown / total)
            timeline =
              intercalate
                ","
                [ "drawbox=x=0:y=ih-46:w=iw:h=46:color=0x0B1220@0.82:t=fill"
                , text "textfile=proof.start:fontcolor=white:fontsize=18:x=16:y=h-33"
                , "drawbox=x=72:y=ih-27:w=iw-420:h=8:color=white@0.22:t=fill"
                , "drawbox=x=72:y=ih-27:w=" <> clip <> ":h=8:color=0x2F6FED@0.55:t=fill"
                , "drawbox=x=69:y=ih-35:w=3:h=24:color=0x2F6FED:t=fill"
                , "drawbox=x=72+" <> clip <> ":y=ih-35:w=3:h=24:color=0x2F6FED:t=fill"
                , text "textfile=proof.end:fontcolor=white:fontsize=18:x=w-334:y=h-33"
                , text "textfile=proof.label:fontcolor=white:fontsize=17:box=1:boxcolor=white@0.16:boxborderw=8:x=w-tw-18:y=h-32"
                ]
        run "ffmpeg" (quiet <> ["-t", "30", "-i", "proof.mp4", "-f", "lavfi", "-i", "color=c=white:s=4x18:r=" <> fps, "-filter_complex", "[0:v]fps=" <> fps <> ",scale='min(" <> width <> ",iw)':-2:flags=lanczos," <> timeline <> "[v];[v][1:v]overlay=x=72+(W-420)*t/" <> show total <> ":y=H-32:shortest=1", "-c:v", "libwebp_anim", "-quality", quality, "-loop", "0", "-f", "webp", "proof.gif"])
        (<= 4718592) <$> getFileSize (directory </> "proof.gif")
  unlessM (anyM preview [("8", "1200", "60"), ("6", "960", "50"), ("4", "800", "40")]) $ throwIO $ userError "the preview exceeds GitHub's image size limit"


renderMedia :: ByteString -> (FilePath -> (String -> [String] -> IO ()) -> IO ()) -> IO (Either Text (ByteString, ByteString))
renderMedia input steps = do
  outcome <- tryAny $ Timeout.timeout (120 * 1000000) $ withSystemTempDirectory "monoscope-proof" \directory -> do
    environment <- filter ((== "PATH") . fst) <$> getEnvironment
    writeFileBS (directory </> "proof.input") input
    steps directory \program args -> withProcessTerm (setStdin nullStream $ setStdout nullStream $ setStderr nullStream $ setEnv environment $ setWorkingDir directory $ proc "nice" (["-n", "19", program] <> args)) waitExitCode >>= \code -> unless (code == ExitSuccess) $ throwIO $ userError $ program <> " exited with " <> show code
    join bitraverse (\name -> readPrivateArtifact directory name mediaLimit) ("proof.gif", "proof.mp4")
  pure $ case outcome of
    Right (Just media) -> Right media
    Right Nothing -> Left "Rendering the proof recording timed out"
    Left failure -> Left $ "The proof recording could not be rendered: " <> toText (displayException failure)


quiet :: [String]
quiet = ["-hide_banner", "-loglevel", "error", "-nostdin", "-y"]


mediaLimit :: Int
mediaLimit = 8 * 1048576


-- | What a UI proof wrote to @$PROOF_MEDIA@: its browser recording, PNG screenshots in the order
-- taken, chapters (seconds and label) that link into the video, and the page's title and URL for
-- the browser frame drawn around it.
data BrowserMedia = BrowserMedia {video :: Maybe ByteString, shots :: [(Text, ByteString)], chapters :: [(Double, Text)], page :: Maybe (Text, Text)}
  deriving stock (Eq, Show)


-- |
-- >>> let section name param bytes = "\n::monoscope-" <> name <> "::" <> param <> "\n" <> extractBase64 (Base64.encodeBase64' bytes) <> "\n::monoscope-" <> name <> "-end::\n"
-- >>> let logged = "noise" <> section "shot" "Menu open" "\137PNG1" <> section "shot" "Not an image" "GIF89a" <> section "video" "webm" "webm-bytes" <> section "chapters" "" "0\tOpen the menu\n7.5\tEnd the survey\nbad\tline\n" <> section "page" "" "Explorer\t127.0.0.1:8080/p/x\n"
-- >>> browserMedia logged
-- BrowserMedia {video = Just "webm-bytes", shots = [("Menu open","\137PNG1")], chapters = [(0.0,"Open the menu"),(7.5,"End the survey")], page = Just ("Explorer","127.0.0.1:8080/p/x")}
-- >>> browserMedia "a terminal proof's log"
-- BrowserMedia {video = Nothing, shots = [], chapters = [], page = Nothing}
browserMedia :: ByteString -> BrowserMedia
browserMedia logBytes =
  BrowserMedia
    { video = snd <$> listToMaybe (sections "video")
    , shots = take 4 [(T.strip label, png) | (label, png) <- sections "shot", "\137PNG" `BS.isPrefixOf` png]
    , -- Chapters only point into a video.
      chapters = take 12 [(at, T.take 80 label) | (_, tsv) <- take 1 (sections "chapters" <* sections "video"), line <- lines (decodeUtf8 tsv), let (at', label') = T.breakOn "\t" line, Just at <- [readMaybe @Double (toString at')], at >= 0, let label = T.strip (T.drop 1 label'), not (T.null label)]
    , page = listToMaybe [bimap (T.take 34 . T.strip) (T.take 140 . T.strip . T.drop 1) (T.breakOn "\t" line) | (_, tsv) <- sections "page", line <- take 1 (lines (decodeUtf8 tsv))]
    }
  where
    sections name = [(param, decoded) | (param, encoded) <- logSections name logBytes, Right decoded <- [Base64.decodeBase64Untyped encoded]]


-- | The sections @proof.sh@ prints as @::monoscope-NAME::PARAM@, a base64 payload line and
-- @::monoscope-NAME-end::@, in order and still encoded.
logSections :: ByteString -> ByteString -> [(Text, ByteString)]
logSections name = go
  where
    begin = "::monoscope-" <> name <> "::"
    end = "::monoscope-" <> name <> "-end::"
    go bytes = case BS.breakSubstring begin bytes of
      (_, rest)
        | BS.null rest -> []
        | otherwise ->
            let (param, body) = BS8.break (== '\n') (BS.drop (BS.length begin) rest)
                (encoded, remaining) = BS.breakSubstring end (BS.drop 1 body)
             in (decodeUtf8 param, BS8.strip encoded) : go remaining


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
