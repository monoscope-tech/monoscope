module Pkg.ErrorFingerprint (
  StackFrame (..),
  ErrorHashes (..),
  Runtime (..),
  parseRuntime,
  parseStackTrace,
  normalizeStackTrace,
  normalizeMessage,
  computeErrorFingerprint,
  computeErrorHashes,
  ErrorMask (..),
  deriveErrorMask,
  applyErrorMasks,
  shapeWithMasks,
  maskMinAffixLen,
  isFrameworkTransportError,
)
where

import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Display (Display)
import Pkg.Deriving (WrappedEnumSC (..))
import Relude
import Utils (replaceAllFormats, toXXHash)
import Web.HttpApiData (FromHttpApiData (..))


-- =============================================================================
-- Error Fingerprinting
-- =============================================================================
-- Fingerprinting priority:
-- 1. Stack trace (if available and has in-app frames)
-- 2. Exception type + normalized message
-- 3. Normalized message only (fallback)
--
-- Stack trace normalization extracts the module/package name and the function name
-- (cleaned per platform) of the last in-app frame.
--
-- Message normalization:
-- - Limits to first 2 non-empty lines
-- - Replaces UUIDs, IPs, emails, timestamps, numbers with placeholders

-- | Represents a parsed stack frame
data StackFrame = StackFrame
  { filePath :: Text
  , moduleName :: Maybe Text
  , functionName :: Text
  , lineNumber :: Maybe Int
  , contextLine :: Maybe Text
  , isInApp :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | SDK language (@telemetry.sdk.language@) picking the stack-frame parser.
-- The attribute is foreign data, so parsing is total: anything unrecognised is 'RGeneric'.
--
-- >>> map parseRuntime ["go", "nodejs", "webjs", "python", "java", "php", "dotnet"]
-- [RGo,RNodejs,RWebjs,RPython,RJava,RPhp,RDotnet]
-- >>> map parseRuntime ["ruby", "", "node.js"]
-- [RGeneric,RGeneric,RGeneric]
data Runtime = RGo | RNodejs | RWebjs | RPython | RJava | RPhp | RDotnet | RGeneric
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (Display, FromHttpApiData) via WrappedEnumSC 'Nothing "R" Runtime


parseRuntime :: Text -> Runtime
parseRuntime = fromMaybe RGeneric . rightToMaybe . parseUrlPiece


-- | Parse a stack trace into a list of stack frames based on runtime
--
-- >>> length $ parseStackTrace RNodejs "at processTicksAndRejections (node:internal/process/task_queues:95:5)\nat handleRequest (/app/src/server.js:42:15)"
-- 2
--
-- >>> map (.functionName) $ parseStackTrace RPython "File \"/app/main.py\", line 10, in main\nFile \"/app/utils.py\", line 5, in helper"
-- ["main","helper"]
--
-- >>> map (.isInApp) $ parseStackTrace RNodejs "at handleRequest (/app/src/server.js:42:15)\nat processTicksAndRejections (node:internal/process/task_queues:95:5)"
-- [True,False]
--
-- >>> map (.functionName) $ parseStackTrace RJava "at com.example.MyClass.doWork(MyClass.java:25)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1067)"
-- ["doWork","doDispatch"]
--
-- Go: parses function call lines, skips goroutine headers and file-path lines
-- >>> map (.functionName) $ parseStackTrace RGo "goroutine 1 [running]:\nmain.handleRequest(0xc0000b4000)\n\t/app/main.go:42 +0x1f\nmyapp/handler.ProcessData(0xc0000b2000)"
-- ["handleRequest","ProcessData"]
--
-- >>> map (.isInApp) $ parseStackTrace RGo "main.handleRequest(0xc0000b4000)\nruntime.goexit()\nnet/http.ListenAndServe(...)"
-- [True,False,False]
--
-- PHP: parses #N /path(line): Class->method() format
-- >>> map (.functionName) $ parseStackTrace RPhp "#0 /app/src/UserController.php(42): App\\UserController->getUser()\n#1 /app/vendor/slim/slim/Slim/Handlers/Strategies/RequestResponse.php(43): closure()"
-- ["getUser","closure"]
--
-- >>> map (.isInApp) $ parseStackTrace RPhp "#0 /app/src/Handler.php(10): Handler->run()\n#1 /app/vendor/lib/Router.php(50): Router->dispatch()"
-- [True,False]
--
-- .NET: parses at Namespace.Class.Method() in /path:line N format
-- >>> map (.functionName) $ parseStackTrace RDotnet "at MyApp.Controllers.UserController.GetUser(Int32 id) in /app/Controllers/UserController.cs:line 42\nat Microsoft.AspNetCore.Mvc.Infrastructure.ActionMethodExecutor.Execute(IActionResultTypeMapper mapper)"
-- ["GetUser","Execute"]
--
-- >>> map (.isInApp) $ parseStackTrace RDotnet "at MyApp.Services.DbService.Query() in /app/Services/DbService.cs:line 15\nat System.Runtime.CompilerServices.TaskAwaiter.HandleNonSuccessAndDebuggerNotification(Task task)"
-- [True,False]
parseStackTrace :: Runtime -> Text -> [StackFrame]
parseStackTrace runtime = mapMaybe parseFrame . filter (not . T.null) . map T.strip . lines
  where
    parseFrame = case runtime of
      RGo -> parseGoFrame
      RNodejs -> parseJsFrame
      RWebjs -> parseJsFrame
      RPython -> parsePythonFrame
      RJava -> parseJavaFrame
      RPhp -> parsePhpFrame
      RDotnet -> parseDotNetFrame
      RGeneric -> parseGenericFrame


-- | Every parser leaves 'contextLine' empty.
mkFrame :: Text -> Maybe Text -> Text -> Maybe Int -> Bool -> StackFrame
mkFrame filePath moduleName functionName lineNumber isInApp = StackFrame{contextLine = Nothing, ..}


readText :: Read a => Text -> Maybe a
readText = readMaybe . toString


-- | Split a dot-qualified name into (module, function)
splitDotted :: Text -> (Text, Text)
splitDotted q = case T.breakOnEnd "." q of
  ("", _) -> ("", q)
  (modDot, fn) -> (T.dropEnd 1 modDot, fn)


-- | Basename of a path with the first matching extension dropped. Shared by the
-- JS/Python/PHP frame parsers so all three agree on what a module name is.
moduleFromPath :: [Text] -> Text -> Text
moduleFromPath exts path =
  let baseName = T.takeWhileEnd (/= '/') path
   in fromMaybe baseName $ asum $ map (`T.stripSuffix` baseName) exts


-- | O(log n) prefix check via S.lookupLE: the largest entry <= the input
-- is the only possible prefix match in a sorted set of prefixes.
isGoStdlib :: Text -> Bool
isGoStdlib txt = any (`T.isPrefixOf` txt) $ S.lookupLE txt goStdlibPrefixes


goStdlibPrefixes :: S.Set Text
goStdlibPrefixes = S.fromList $ words "archive/ bufio. bytes. cmp. compress/ context. crypto. crypto/ database/ debug/ embed. encoding. encoding/ errors. expvar. flag. fmt. go/ hash. hash/ html. html/ image. image/ index/ internal/ io. io/ iter. log. log/ maps. math. math/ mime. mime/ net. net/ os. path. path/ plugin. reflect. regexp. runtime. slices. sort. strconv. strings. sync. sync/ syscall. testing. time. unicode. unicode/ unique. unsafe."
{-# NOINLINE goStdlibPrefixes #-}


-- | Parse Go stack frame: "goroutine 1 [running]:" or "main.foo(0x1234)".
-- Goroutine headers and @/path/file.go:123 +0x1f@ lines carry no function name, so
-- they are skipped as fingerprint noise.
parseGoFrame :: Text -> Maybe StackFrame
parseGoFrame line
  | "goroutine" `T.isPrefixOf` line || ".go:" `T.isInfixOf` line = Nothing
  | "(" `T.isInfixOf` line =
      let funcPart = fst $ T.breakOn "(" line
          (modName, fnName) = splitDotted funcPart
       in Just $ mkFrame "" (guarded (not . T.null) modName) fnName Nothing (not $ isGoStdlib funcPart)
  | otherwise = Nothing


-- | Parse JavaScript stack frame
-- Formats:
--   at functionName (filePath:line:col)
--   at filePath:line:col
--   at async functionName (filePath:line:col)
parseJsFrame :: Text -> Maybe StackFrame
parseJsFrame line = do
  content <- T.strip <$> T.stripPrefix "at " line
  let content' = fromMaybe content $ T.stripPrefix "async " content
      (funcPart, rest) = T.breakOn " (" content'
      (fn, loc)
        | "(" `T.isInfixOf` content' = (T.takeWhileEnd (/= '.') funcPart, T.dropAround (`elem` ("()" :: String)) rest)
        | otherwise = ("<anonymous>", content')
      -- Split from the right to handle paths with colons (Windows)
      parts = T.splitOn ":" loc
      n = length parts
      (filePath, lineNum)
        | n >= 3 = (T.intercalate ":" $ take (n - 2) parts, viaNonEmpty head (drop (n - 2) parts) >>= readText)
        | otherwise = (loc, Nothing)
  pure $ mkFrame filePath (Just $ T.toLower $ moduleFromPath [".js", ".ts", ".mjs", ".cjs"] filePath) fn lineNum (not $ any (`T.isInfixOf` filePath) ["node_modules/", "<anonymous>", "internal/", "node:"])


-- | Parse Python stack frame
-- Format: File "path/to/file.py", line 123, in function_name
parsePythonFrame :: Text -> Maybe StackFrame
parsePythonFrame line = do
  content <- T.stripPrefix "File \"" line
  let (filePath, rest) = T.breakOn "\"" content
      parts = T.splitOn ", " $ T.drop 2 rest -- Skip '",'
      field pfx = T.drop (T.length pfx) <$> find (pfx `T.isPrefixOf`) parts
      funcName = foldr (uncurry T.replace) (fromMaybe "<module>" $ field "in ") ([("<lambda>", "lambda"), ("<listcomp>", "listcomp"), ("<dictcomp>", "dictcomp")] :: [(Text, Text)])
  pure $ mkFrame filePath (Just $ moduleFromPath [".py"] filePath) funcName (readText =<< field "line ") (not $ any (`T.isInfixOf` filePath) ["site-packages/", "dist-packages/", "/lib/python", "<frozen"])


-- | Parse Java stack frame
-- Format: at com.example.Class.method(File.java:123)
parseJavaFrame :: Text -> Maybe StackFrame
parseJavaFrame line = do
  content <- T.stripPrefix "at " line
  let (qualifiedMethod, rest) = T.breakOn "(" content
      (fileName, lineStr) = T.breakOn ":" $ T.dropAround (`elem` ("()" :: String)) rest
      (moduleName, funcName) = splitDotted qualifiedMethod
  -- takeWhile drops generics: method<T> -> method
  pure $ mkFrame fileName (Just moduleName) (T.takeWhile (/= '<') funcName) (readText $ T.drop 1 lineStr) (not $ any (`T.isPrefixOf` qualifiedMethod) ["java.", "javax.", "sun.", "com.sun.", "jdk.", "org.springframework."])


-- | Parse PHP stack frame
-- Format: #0 /path/to/file.php(123): ClassName->methodName()
parsePhpFrame :: Text -> Maybe StackFrame
parsePhpFrame line = do
  guard $ "#" `T.isPrefixOf` line
  let (pathPart, funcPart) = T.breakOn ": " $ T.drop 1 $ T.dropWhile (/= ' ') line -- Skip "#N "
      (filePath, lineStr) = T.breakOn "(" pathPart
      func = T.takeWhile (/= '(') $ T.drop 2 funcPart
      splitLast sep = viaNonEmpty last $ drop 1 $ T.splitOn sep func
  pure $ mkFrame filePath (Just $ moduleFromPath [".php"] filePath) (T.replace "{closure}" "closure" $ fromMaybe func $ splitLast "->" <|> splitLast "::") (readText $ T.takeWhile (/= ')') $ T.drop 1 lineStr) (not $ any (`T.isInfixOf` filePath) ["/vendor/", "/phar://"])


-- | Parse .NET stack frame
-- Format: at Namespace.Class.Method(params) in /path/to/file.cs:line 123
parseDotNetFrame :: Text -> Maybe StackFrame
parseDotNetFrame line = do
  content <- T.stripPrefix "at " line
  let (methodPart, locationPart) = T.breakOn " in " content
      qualifiedMethod = T.takeWhile (/= '(') methodPart
      (moduleName, funcName) = splitDotted qualifiedMethod
      (filePath, lineStr) = T.breakOn ":line " $ T.drop 4 locationPart
  -- takeWhile drops generic arity: Method`1 -> Method
  pure $ mkFrame filePath (Just moduleName) (T.takeWhile (/= '`') funcName) (readText $ T.drop 6 lineStr) (not $ any (`T.isPrefixOf` qualifiedMethod) ["System.", "Microsoft.", "Newtonsoft."])


-- | Generic stack frame parser for unknown formats. The function is the first
-- function-like word, falling back to the full line to avoid collisions; frames are
-- assumed in-app.
parseGenericFrame :: Text -> Maybe StackFrame
parseGenericFrame line = do
  guard $ not ("..." `T.isPrefixOf` line)
  let fn = maybe line (T.takeWhile (/= '(')) $ find (\p -> "(" `T.isInfixOf` p || "." `T.isInfixOf` p) $ words line
  pure $ mkFrame line Nothing fn (listToMaybe $ mapMaybe readText $ drop 1 $ T.splitOn ":" line) True


-- | Normalize a stack trace for fingerprinting
-- Returns a list of normalized frame strings suitable for hashing
--
-- >>> normalizeStackTrace RNodejs "at handleRequest (/app/src/server.js:42:15)\nat processTicksAndRejections (node:internal/process/task_queues:95:5)"
-- "server|handleRequest"
--
-- >>> normalizeStackTrace RPython "File \"/app/main.py\", line 10, in main\nFile \"/app/utils.py\", line 5, in helper"
-- "utils|helper"
--
-- >>> normalizeStackTrace RJava "at com.example.MyClass.doWork(MyClass.java:25)\nat com.example.Service.process(Service.java:50)"
-- "com.example.Service|process"
--
-- >>> normalizeStackTrace RGeneric "some random frame info"
-- "some random frame info"
normalizeStackTrace :: Runtime -> Text -> Text
normalizeStackTrace runtime stackText =
  let frames = parseStackTrace runtime stackText
      inAppFrames = filter (.isInApp) frames
   in maybe "" normalizeFrame $ viaNonEmpty last $ bool inAppFrames frames (null inAppFrames)
  where
    -- Strip memory addresses (0x...), generic type parameters and parameter lists.
    normalizeFunction = T.strip . T.takeWhile (`notElem` ("<(" :: String)) . unwords . filter (not . ("0x" `T.isPrefixOf`)) . words
    normalizeFrame frame = T.intercalate "|" $ filter (not . T.null) [fold frame.moduleName, normalizeFunction frame.functionName]


-- | Normalize an error message for fingerprinting
-- Limits to first 2 non-empty lines and replaces variable content
--
-- >>> normalizeMessage "Connection refused to 192.168.1.100:5432"
-- "Connection refused to {ipv4}{port}"
--
-- >>> normalizeMessage "User c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd not found"
-- "User {uuid} not found"
--
-- >>> normalizeMessage "Error 404: Resource 12345 not available\n\nDetails here\nMore info"
-- "Error {integer}: Resource {integer} not available Details here"
--
-- >>> normalizeMessage "   Trimmed message   "
-- "Trimmed message"
--
-- A serialised context object is collapsed. htmx logs its whole event, whose
-- listener arrays and request fields differ on every occurrence, so each one
-- minted its own pattern (611 hashes for 611 Talstack AbortErrors):
--
-- >>> let dump n = "{\"elt\":{\"listeners\":[" <> T.intercalate "," (replicate n "{\"runCount\":7}") <> "]},\"pad\":\"" <> T.replicate 512 "x" <> "\"}"
-- >>> normalizeMessage ("htmx:error: The user aborted a request. " <> dump 1) == normalizeMessage ("htmx:error: The user aborted a request. " <> dump 3)
-- True
--
-- A short JSON body is kept: it is usually what tells two failures apart.
--
-- >>> normalizeMessage "Apaylo request failed: {\"StatusCode\":400,\"Message\":\"No data found\"}" == normalizeMessage "Apaylo request failed: {\"StatusCode\":500,\"Message\":\"A technical error occurred\"}"
-- False
normalizeMessage :: Text -> Text
normalizeMessage msg =
  let lns = take 2 $ filter (not . T.null . T.strip) $ T.splitOn "\n" $ T.replace "\r\n" "\n" msg
      combined = T.intercalate " " $ map T.strip lns
      -- Every discriminating JSON body seen in 90 days of prod patterns is under 512
      -- chars; everything over it is a dump (htmx events, ethers transactions).
      (prose, json) = T.breakOn "{\"" combined
      collapsed = if T.compareLength json 512 == GT then prose <> "{json}" else combined
   in T.strip $ replaceAllFormats collapsed


-- | Compute the error fingerprint hash using stack-trace-first prioritization
-- Priority:
-- 1. Stack trace (if has meaningful in-app frames)
-- 2. Exception type + message
-- 3. Message only
--
-- With stack trace - uses projectId, service, exceptionType, and normalized stack:
-- >>> computeErrorFingerprint "proj1" (Just "svc") (Just "span") RNodejs "TypeError" "msg" "at handler (/app/index.js:10:5)"
-- "c3500d04"
--
-- Without stack trace but with exception type - uses projectId, service, span, type, and message:
-- >>> computeErrorFingerprint "proj1" (Just "user-service") (Just "/api/users") RNodejs "ValidationError" "Invalid email format" ""
-- "af0aa163"
--
-- Without stack trace or exception type - uses projectId, service, span, and message only:
-- >>> computeErrorFingerprint "proj1" (Just "api") (Just "/health") RNodejs "" "Connection refused to 192.168.1.1:5432" ""
-- "44e50418"
--
-- Same error from different IPs produces same fingerprint (IP normalized):
-- >>> computeErrorFingerprint "proj1" (Just "db") Nothing RPython "" "Connection refused to 10.0.0.1:5432" "" == computeErrorFingerprint "proj1" (Just "db") Nothing RPython "" "Connection refused to 172.16.0.50:5432" ""
-- True
--
-- Same stack trace produces same fingerprint regardless of message:
-- >>> computeErrorFingerprint "proj1" Nothing Nothing RNodejs "Error" "message 1" "at handler (/app/index.js:10:5)" == computeErrorFingerprint "proj1" Nothing Nothing RNodejs "Error" "different message" "at handler (/app/index.js:10:5)"
-- True
--
-- Python: same in-app frame with different line numbers → same fingerprint (line numbers excluded from hash):
-- >>> computeErrorFingerprint "proj1" Nothing Nothing RPython "ValueError" "invalid literal" "File \"/app/main.py\", line 10, in handler\nFile \"/usr/lib/python3.11/json/decoder.py\", line 355, in raw_decode" == computeErrorFingerprint "proj1" Nothing Nothing RPython "ValueError" "invalid literal" "File \"/app/main.py\", line 99, in handler\nFile \"/usr/lib/python3.11/json/decoder.py\", line 400, in raw_decode"
-- True
--
-- Java: same in-app method at different lines → same fingerprint:
-- >>> computeErrorFingerprint "proj1" Nothing Nothing RJava "NullPointerException" "msg1" "at com.example.App.process(App.java:42)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1067)" == computeErrorFingerprint "proj1" Nothing Nothing RJava "NullPointerException" "msg2" "at com.example.App.process(App.java:99)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:2000)"
-- True
computeErrorFingerprint :: Text -> Maybe Text -> Maybe Text -> Runtime -> Text -> Text -> Text -> Text
computeErrorFingerprint p s sn rt et msg st = (computeErrorHashes p s sn rt et msg st).narrow


-- | Pair of fingerprints: the narrow one (includes spanName) for per-route identity,
-- and the broad one (excludes spanName) for cross-route rollup of span-agnostic errors.
data ErrorHashes = ErrorHashes
  { narrow :: Text
  , broad :: Text
  , shape :: Text
  -- ^ Type plus normalised message, and nothing else — no service, no span name,
  -- no stack. This is the key errors are /grouped for review/ under, deliberately
  -- coarser than 'narrow' and stable across the things that split it.
  --
  -- It is not a fingerprint and must never be used as one: same shape with
  -- different stack traces is two bugs by 'narrow''s own definition. It exists so
  -- that a review, and the merge a review authorises, survive a change to the
  -- fingerprint — which is otherwise a re-key that orphans both.
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Compute both narrow (spanName-aware) and broad (spanName-agnostic) hashes.
--
-- Two different spanNames produce the same broad hash:
-- >>> (computeErrorHashes "p" (Just "s") (Just "GET /a") RNodejs "" "boom" "").broad == (computeErrorHashes "p" (Just "s") (Just "GET /b") RNodejs "" "boom" "").broad
-- True
--
-- But different narrow hashes:
-- >>> (computeErrorHashes "p" (Just "s") (Just "GET /a") RNodejs "" "boom" "").narrow == (computeErrorHashes "p" (Just "s") (Just "GET /b") RNodejs "" "boom" "").narrow
-- False
--
-- Transport errors collapse across routes and stack traces (same type+message → same hash):
-- >>> let a = computeErrorHashes "p" (Just "web") (Just "GET /a") RGeneric "InvalidRequest" "Warp: Client closed connection prematurely" "at Network.Wai.Handler.Warp.Run:123"
-- >>> let b = computeErrorHashes "p" (Just "web") (Just "POST /b") RGeneric "InvalidRequest" "Warp: Client closed connection prematurely" "at Network.Wai.Handler.Warp.Recv:45"
-- >>> a.narrow == b.narrow && a.broad == b.broad
-- True
computeErrorHashes :: Text -> Maybe Text -> Maybe Text -> Runtime -> Text -> Text -> Text -> ErrorHashes
computeErrorHashes projectIdText mService spanName runtime exceptionType message stackTrace
  -- Framework transport noise is route/stack-agnostic: collapse every occurrence of
  -- the same type+message into one pattern so it does not fan out into hundreds of issues.
  | isFrameworkTransportError typ msg = let h = hashParts [projectIdText, "transport", typ, msg] in ErrorHashes{narrow = h, broad = h, shape}
  | otherwise = ErrorHashes{narrow = go spanName, broad = go Nothing, shape}
  where
    msg = normalizeMessage message
    typ = T.strip exceptionType
    shape = shapeWithMasks [] projectIdText exceptionType message
    stack = normalizeStackTrace runtime stackTrace
    -- 'hashParts' drops empty parts, so an empty type falls back to message only.
    go sn = hashParts $ projectIdText : fromMaybe "" mService : bool [fromMaybe "" sn, typ, msg] [typ, stack] (not $ T.null stack)


hashParts :: [Text] -> Text
hashParts = toXXHash . T.intercalate "|" . filter (not . T.null)


-- | Detect framework/transport-layer errors that happen independently of the
-- application's span identity. These should be rolled up into a single issue
-- even when they occur across many different routes or handlers.
--
-- Classification is conservative to avoid silently merging user errors:
-- 1. An allowlisted exception type (case-insensitive) matches unconditionally.
-- 2. Message-pattern matching only applies when the exception type is empty or
--    a generic IO/runtime type — prevents `ValidationError: "cannot have broken
--    pipe in filename"` from being misclassified.
--
-- >>> isFrameworkTransportError "InvalidRequest" "Warp: Client closed connection prematurely"
-- True
--
-- Case-insensitive type match:
-- >>> isFrameworkTransportError "econnreset" ""
-- True
--
-- Java SocketException: message fallback under a generic type:
-- >>> isFrameworkTransportError "SocketException" "connection reset by peer"
-- True
--
-- Generic IOError with broken-pipe message:
-- >>> isFrameworkTransportError "IOError" "broken pipe"
-- True
--
-- Non-generic type with a substring false-positive is NOT flagged:
-- >>> isFrameworkTransportError "ValidationError" "cannot use broken pipe in filename"
-- False
--
-- >>> isFrameworkTransportError "ValidationError" "Invalid email"
-- False
--
-- >>> isFrameworkTransportError "NullPointerException" "foo was null"
-- False
isFrameworkTransportError :: Text -> Text -> Bool
isFrameworkTransportError exceptionType normalizedMessage =
  S.member t knownTypes || (S.member t genericTypes && any (`T.isInfixOf` T.toLower normalizedMessage) messagePatterns)
  where
    t = T.toLower $ T.strip exceptionType
    knownTypes =
      S.fromList
        $ foldMap
          words
          [ "invalidrequest connectionclosedbypeer endofstream" -- Haskell / Warp
          , "clientabortexception brokenpipeerror connectionreseterror connectionabortederror connectionerror remotedisconnected http2.connectionerror h2.exceptions.streamclosederror h11._util.remoteprotocolerror" -- Python
          , "econnreset epipe econnaborted econnrefused etimedout ehostunreach" -- Node.js errno codes
          , "*net.operror net.operror syscall.errno io.erruneexpectedeof io.errclosedpipe" -- Go
          , "system.io.ioexception system.net.sockets.socketexception system.net.http.httprequestexception system.operationcanceledexception" -- .NET
          , "java.net.socketexception java.io.eofexception org.apache.catalina.connector.clientabortexception" -- Java
          , "errno::epipe errno::econnreset errno::econnaborted" -- Ruby
          , "std::io::error" -- Rust
          ]
    -- Types where a message-pattern fallback is trustworthy (generic / IO layer)
    genericTypes = S.fromList $ "" : words "ioerror ioexception oserror socketexception runtimeerror error systemerror exception"
    messagePatterns = ["client closed", "broken pipe", "connection reset", "connection aborted", "premature eof", "client disconnected"]


-- | A literal prefix and suffix around a part that varies, learned from a group the
-- review loop merged.
data ErrorMask = ErrorMask
  { prefix :: Text
  , suffix :: Text
  , minVarLen :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Smallest literal on either side of a mask.
--
-- Two characters is not a mask, it is a wildcard with extra steps. The endpoint side
-- learned this the same way: 'sharedIdPrefix' refuses a prefix under two characters
-- and is checked against known route words on top.
maskMinAffixLen :: Int
maskMinAffixLen = 3


-- | Derive a mask from the messages of a group that was merged.
--
-- The messages agree everywhere except the values that made them look different, so
-- the common prefix and suffix are the literal and what is left is the variable.
--
-- >>> deriveErrorMask ["SB-3FDBA2D0A0A2 not processing", "SB-90770E7BA9FD not processing"]
-- Just (ErrorMask {prefix = "SB-", suffix = " not processing", minVarLen = 12})
--
-- Refused when either literal is too short to anchor — this would otherwise be
-- @{anything} declined@, which swallows every error ending in that word:
--
-- >>> deriveErrorMask ["a declined", "b declined"]
-- Nothing
--
-- Refused when the messages are identical, which leaves nothing varying and would
-- produce a mask over the empty string:
--
-- >>> deriveErrorMask ["same message", "same message"]
-- Nothing
--
-- Refused below two members: one message shares all of itself with itself.
--
-- >>> deriveErrorMask ["SB-3FDBA2D0A0A2 not processing"]
-- Nothing
deriveErrorMask :: [Text] -> Maybe ErrorMask
deriveErrorMask msgs = do
  let distinct = ordNub msgs
  guard $ length distinct >= 2
  firstMsg <- viaNonEmpty head distinct
  let pfx = foldl' commonPrefix firstMsg distinct
      sfx = foldl' commonSuffix firstMsg distinct
      varLens = [T.length m - T.length pfx - T.length sfx | m <- distinct]
  -- The affixes must not overlap: on near-identical messages the common prefix and
  -- the common suffix can both run past the middle and describe a negative variable.
  guard $ all (> 0) varLens
  guard $ T.length pfx >= maskMinAffixLen && T.length sfx >= maskMinAffixLen
  pure ErrorMask{prefix = pfx, suffix = sfx, minVarLen = foldl' min (T.length firstMsg) varLens}
  where
    commonPrefix a b = maybe "" (\(c, _, _) -> c) $ T.commonPrefixes a b
    commonSuffix a b = T.reverse $ maybe "" (\(c, _, _) -> c) $ T.commonPrefixes (T.reverse a) (T.reverse b)


-- | Replace the variable part of every mask that matches.
--
-- >>> applyErrorMasks [ErrorMask "SB-" " not processing" 12] "SB-90770E7BA9FD not processing"
-- "SB-{masked} not processing"
--
-- A variable shorter than the group the mask came from is left alone, so a mask over
-- ids does not also swallow a one-character flag between the same literals:
--
-- >>> applyErrorMasks [ErrorMask "SB-" " not processing" 12] "SB-7 not processing"
-- "SB-7 not processing"
--
-- Non-matching text is returned unchanged:
--
-- >>> applyErrorMasks [ErrorMask "SB-" " not processing" 12] "unrelated failure"
-- "unrelated failure"
applyErrorMasks :: [ErrorMask] -> Text -> Text
applyErrorMasks masks msg = foldl' applyOne msg masks
  where
    applyOne m mask = fromMaybe m do
      afterPfx <- T.stripPrefix mask.prefix m
      var <- T.stripSuffix mask.suffix afterPfx
      guard $ T.length var >= mask.minVarLen
      pure $ mask.prefix <> "{masked}" <> mask.suffix


-- | The shape, with a project's learned masks applied to the message first.
--
-- With no masks this MUST agree with 'computeErrorHashes'. Every @shape_hash@ already
-- stored was written by one or the other, and a divergence would not fail — it would
-- quietly stop the ingest gate matching anything:
--
-- >>> shapeWithMasks [] "p" "TypeError" "boom" == (computeErrorHashes "p" Nothing Nothing RGeneric "TypeError" "boom" "").shape
-- True
--
-- Masks move the SHAPE and never the fingerprint. That is the whole reason this is a
-- separate function rather than a parameter on 'computeErrorHashes': a mask that
-- changed @narrow@ would re-key every pattern it touches, minting new issues for
-- errors we already know about — the same "a rename is a new hash" trap the endpoint
-- work hit. Moving only the shape is additive: grouping gets coarser, issue identity
-- does not move.
shapeWithMasks :: [ErrorMask] -> Text -> Text -> Text -> Text
shapeWithMasks masks projectIdText exceptionType message =
  hashParts [projectIdText, "shape", T.strip exceptionType, applyErrorMasks masks (normalizeMessage message)]
