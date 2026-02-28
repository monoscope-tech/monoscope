module Pkg.ErrorFingerprint (
  StackFrame (..),
  parseStackTrace,
  normalizeStackTrace,
  normalizeMessage,
  computeErrorFingerprint,
)
where

import Data.Text qualified as T
import Relude
import Utils (replaceAllFormats, toXXHash)


-- =============================================================================
-- Error Fingerprinting
-- =============================================================================
-- Fingerprinting priority:
-- 1. Stack trace (if available and has in-app frames)
-- 2. Exception type + normalized message
-- 3. Normalized message only (fallback)
--
-- Stack trace normalization extracts:
-- - Module/package name
-- - Function name (cleaned per platform)
-- - Context line (whitespace normalized, max 120 chars)
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
  , columnNumber :: Maybe Int
  , contextLine :: Maybe Text
  , isInApp :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Parse a stack trace into a list of stack frames based on runtime
--
-- >>> length $ parseStackTrace "nodejs" "at processTicksAndRejections (node:internal/process/task_queues:95:5)\nat handleRequest (/app/src/server.js:42:15)"
-- 2
--
-- >>> map (.functionName) $ parseStackTrace "python" "File \"/app/main.py\", line 10, in main\nFile \"/app/utils.py\", line 5, in helper"
-- ["main","helper"]
--
-- >>> map (.isInApp) $ parseStackTrace "nodejs" "at handleRequest (/app/src/server.js:42:15)\nat processTicksAndRejections (node:internal/process/task_queues:95:5)"
-- [True,False]
--
-- >>> map (.functionName) $ parseStackTrace "java" "at com.example.MyClass.doWork(MyClass.java:25)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1067)"
-- ["doWork","doDispatch"]
--
-- Go: parses function call lines, skips goroutine headers and file-path lines
-- >>> map (.functionName) $ parseStackTrace "go" "goroutine 1 [running]:\nmain.handleRequest(0xc0000b4000)\n\t/app/main.go:42 +0x1f\nmyapp/handler.ProcessData(0xc0000b2000)"
-- ["handleRequest","ProcessData"]
--
-- >>> map (.isInApp) $ parseStackTrace "go" "main.handleRequest(0xc0000b4000)\nruntime.goexit()\nnet/http.ListenAndServe(...)"
-- [True,False,False]
--
-- PHP: parses #N /path(line): Class->method() format
-- >>> map (.functionName) $ parseStackTrace "php" "#0 /app/src/UserController.php(42): App\\UserController->getUser()\n#1 /app/vendor/slim/slim/Slim/Handlers/Strategies/RequestResponse.php(43): closure()"
-- ["getUser","closure"]
--
-- >>> map (.isInApp) $ parseStackTrace "php" "#0 /app/src/Handler.php(10): Handler->run()\n#1 /app/vendor/lib/Router.php(50): Router->dispatch()"
-- [True,False]
--
-- .NET: parses at Namespace.Class.Method() in /path:line N format
-- >>> map (.functionName) $ parseStackTrace "dotnet" "at MyApp.Controllers.UserController.GetUser(Int32 id) in /app/Controllers/UserController.cs:line 42\nat Microsoft.AspNetCore.Mvc.Infrastructure.ActionMethodExecutor.Execute(IActionResultTypeMapper mapper)"
-- ["GetUser","Execute"]
--
-- >>> map (.isInApp) $ parseStackTrace "dotnet" "at MyApp.Services.DbService.Query() in /app/Services/DbService.cs:line 15\nat System.Runtime.CompilerServices.TaskAwaiter.HandleNonSuccessAndDebuggerNotification(Task task)"
-- [True,False]
parseStackTrace :: Text -> Text -> [StackFrame]
parseStackTrace runtime stackText =
  let lns = filter (not . T.null . T.strip) $ lines stackText
   in mapMaybe (parseStackFrame runtime) lns


-- | Parse a single stack frame line based on SDK type
parseStackFrame :: Text -> Text -> Maybe StackFrame
parseStackFrame runtime line =
  let trimmed = T.strip line
   in if
        | runtime == "go" -> parseGoFrame trimmed
        | runtime `elem` ["nodejs", "webjs"] -> parseJsFrame trimmed
        | runtime == "python" -> parsePythonFrame trimmed
        | runtime == "java" -> parseJavaFrame trimmed
        | runtime == "php" -> parsePhpFrame trimmed
        | runtime == "dotnet" -> parseDotNetFrame trimmed
        | otherwise -> parseGenericFrame trimmed


readText :: Read a => Text -> Maybe a
readText = readMaybe . toString


-- | Split a dot-qualified name into (module, function)
splitDotted :: Text -> (Text, Text)
splitDotted q = case T.breakOnEnd "." q of
  ("", _) -> ("", q)
  (modDot, fn) -> (T.dropEnd 1 modDot, fn)


-- | True when none of the needles appear as infixes in the haystack
noneInfix :: [Text] -> Text -> Bool
noneInfix needles haystack = not $ any (`T.isInfixOf` haystack) needles


-- | True when none of the needles appear as prefixes of the text
nonePrefix :: [Text] -> Text -> Bool
nonePrefix needles txt = not $ any (`T.isPrefixOf` txt) needles


-- | Parse Go stack frame: "goroutine 1 [running]:" or "main.foo(0x1234)"
-- Format: package.function(args) or /path/to/file.go:123 +0x1f
parseGoFrame :: Text -> Maybe StackFrame
parseGoFrame line
  | "goroutine" `T.isPrefixOf` line = Nothing -- Skip goroutine headers
  | ".go:" `T.isInfixOf` line =
      -- File path line: /path/to/file.go:123 +0x1f — skip since functionName is empty (noise in fingerprints)
      Nothing
  | "(" `T.isInfixOf` line =
      let (funcPart, _) = T.breakOn "(" line
          (modName, fnName) = splitDotted funcPart
       in Just
            StackFrame
              { filePath = ""
              , moduleName = if T.null modName then Nothing else Just modName
              , functionName = fnName
              , lineNumber = Nothing
              , columnNumber = Nothing
              , contextLine = Nothing
              , isInApp = nonePrefix goStdlibPrefixes funcPart
              }
  | otherwise = Nothing


goStdlibPrefixes :: [Text]
goStdlibPrefixes =
  [ "runtime."
  , "syscall."
  , "net."
  , "net/"
  , "reflect."
  , "os."
  , "io."
  , "io/"
  , "fmt."
  , "log."
  , "log/"
  , "strings."
  , "strconv."
  , "sync."
  , "sync/"
  , "sort."
  , "bytes."
  , "encoding."
  , "encoding/"
  , "crypto."
  , "crypto/"
  , "math."
  , "math/"
  , "testing."
  , "context."
  , "time."
  , "path."
  , "path/"
  , "regexp."
  , "bufio."
  , "archive/"
  , "compress/"
  , "database/"
  , "debug/"
  , "embed."
  , "errors."
  , "expvar."
  , "flag."
  , "go/"
  , "hash."
  , "hash/"
  , "html."
  , "html/"
  , "image."
  , "image/"
  , "index/"
  , "internal/"
  , "maps."
  , "mime."
  , "mime/"
  , "plugin."
  , "slices."
  , "unicode."
  , "unicode/"
  , "unsafe."
  , "cmp."
  , "iter."
  , "unique."
  ]
{-# NOINLINE goStdlibPrefixes #-}


-- | Parse JavaScript stack frame
-- Formats:
--   at functionName (filePath:line:col)
--   at filePath:line:col
--   at async functionName (filePath:line:col)
parseJsFrame :: Text -> Maybe StackFrame
parseJsFrame line
  | "at " `T.isPrefixOf` T.strip line =
      let content = T.strip $ T.drop 3 $ T.strip line
          -- Handle "at async ..."
          content' =
            if "async " `T.isPrefixOf` content
              then T.drop 6 content
              else content
       in if "(" `T.isInfixOf` content'
            then parseJsWithParens content'
            else parseJsWithoutParens content'
  | otherwise = Nothing
  where
    parseJsWithParens txt =
      let (funcPart, rest) = T.breakOn " (" txt
          locationPart = T.dropAround (`elem` ("()" :: String)) rest
          (filePath, lineCol) = parseJsLocation locationPart
          (lineNum, colNum) = parseLineCol lineCol
       in Just
            StackFrame
              { filePath = filePath
              , moduleName = extractJsModule filePath
              , functionName = cleanJsFunction funcPart
              , lineNumber = lineNum
              , columnNumber = colNum
              , contextLine = Nothing
              , isInApp = isJsInApp filePath
              }

    parseJsWithoutParens txt =
      let (filePath, lineCol) = parseJsLocation txt
          (lineNum, colNum) = parseLineCol lineCol
       in Just
            StackFrame
              { filePath = filePath
              , moduleName = extractJsModule filePath
              , functionName = "<anonymous>"
              , lineNumber = lineNum
              , columnNumber = colNum
              , contextLine = Nothing
              , isInApp = isJsInApp filePath
              }

    parseJsLocation loc =
      -- Split from the right to handle paths with colons (Windows)
      let parts = T.splitOn ":" loc
          n = length parts
       in if n >= 3
            then (T.intercalate ":" $ take (n - 2) parts, T.intercalate ":" $ drop (n - 2) parts)
            else (loc, "")

    parseLineCol lc =
      let parts = T.splitOn ":" lc
       in case parts of
            [l, c] -> (readText l, readText c)
            [l] -> (readText l, Nothing)
            _ -> (Nothing, Nothing)

    extractJsModule path =
      let baseName = fromMaybe path $ viaNonEmpty last $ T.splitOn "/" path
       in Just
            $ T.toLower
            $ fromMaybe baseName
            $ T.stripSuffix ".js" baseName
            <|> T.stripSuffix ".ts" baseName
            <|> T.stripSuffix ".mjs" baseName
            <|> T.stripSuffix ".cjs" baseName

    cleanJsFunction func =
      -- Remove namespacing: Object.foo.bar -> bar
      let parts = T.splitOn "." func
       in fromMaybe func $ viaNonEmpty last parts

    isJsInApp = noneInfix ["node_modules/", "<anonymous>", "internal/", "node:"]


-- | Parse Python stack frame
-- Format: File "path/to/file.py", line 123, in function_name
parsePythonFrame :: Text -> Maybe StackFrame
parsePythonFrame line
  | "File \"" `T.isPrefixOf` T.strip line =
      let content = T.drop 6 $ T.strip line -- Remove 'File "'
          (filePath, rest) = T.breakOn "\"" content
          -- Parse ", line 123, in func_name"
          parts = T.splitOn ", " $ T.drop 2 rest -- Skip '",'
          lineNum = case find ("line " `T.isPrefixOf`) parts of
            Just p -> readText $ T.drop 5 p
            Nothing -> Nothing
          funcName = case find ("in " `T.isPrefixOf`) parts of
            Just p -> T.drop 3 p
            Nothing -> "<module>"
       in Just
            StackFrame
              { filePath = filePath
              , moduleName = extractPythonModule filePath
              , functionName = cleanPythonFunction funcName
              , lineNumber = lineNum
              , columnNumber = Nothing
              , contextLine = Nothing
              , isInApp = isPythonInApp filePath
              }
  | otherwise = Nothing
  where
    extractPythonModule path =
      let baseName = fromMaybe path $ viaNonEmpty last $ T.splitOn "/" path
          moduleName = fromMaybe baseName $ T.stripSuffix ".py" baseName
       in Just moduleName

    cleanPythonFunction func =
      -- Remove lambda indicators
      T.replace "<lambda>" "lambda"
        $ T.replace "<listcomp>" "listcomp"
        $ T.replace "<dictcomp>" "dictcomp" func

    isPythonInApp = noneInfix ["site-packages/", "dist-packages/", "/lib/python", "<frozen"]


-- | Parse Java stack frame
-- Format: at com.example.Class.method(File.java:123)
parseJavaFrame :: Text -> Maybe StackFrame
parseJavaFrame line
  | "at " `T.isPrefixOf` T.strip line =
      let content = T.drop 3 $ T.strip line
          (qualifiedMethod, rest) = T.breakOn "(" content
          locationPart = T.dropAround (`elem` ("()" :: String)) rest
          (fileName, lineNum) = parseJavaLocation locationPart
          (moduleName, funcName) = splitDotted qualifiedMethod
       in Just
            StackFrame
              { filePath = fileName
              , moduleName = Just moduleName
              , functionName = cleanJavaFunction funcName
              , lineNumber = lineNum
              , columnNumber = Nothing
              , contextLine = Nothing
              , isInApp = isJavaInApp qualifiedMethod
              }
  | otherwise = Nothing
  where
    parseJavaLocation loc =
      let (file, lineStr) = T.breakOn ":" loc
       in (file, readText $ T.drop 1 lineStr)

    cleanJavaFunction =
      -- Remove generics: method<T> -> method
      T.takeWhile (/= '<')

    isJavaInApp = nonePrefix ["java.", "javax.", "sun.", "com.sun.", "jdk.", "org.springframework."]


-- | Parse PHP stack frame
-- Format: #0 /path/to/file.php(123): ClassName->methodName()
parsePhpFrame :: Text -> Maybe StackFrame
parsePhpFrame line
  | "#" `T.isPrefixOf` T.strip line =
      let content = T.drop 1 $ T.dropWhile (/= ' ') $ T.strip line -- Skip "#N "
          (pathPart, funcPart) = T.breakOn ": " content
          (filePath, lineNum) = parsePhpPath pathPart
          funcName = T.takeWhile (/= '(') $ T.drop 2 funcPart
       in Just
            StackFrame
              { filePath = filePath
              , moduleName = extractPhpModule filePath
              , functionName = cleanPhpFunction funcName
              , lineNumber = lineNum
              , columnNumber = Nothing
              , contextLine = Nothing
              , isInApp = isPhpInApp filePath
              }
  | otherwise = Nothing
  where
    parsePhpPath path =
      let (file, lineStr) = T.breakOn "(" path
          lineNum = readText $ T.takeWhile (/= ')') $ T.drop 1 lineStr
       in (file, lineNum)

    extractPhpModule path =
      let baseName = fromMaybe path $ viaNonEmpty last $ T.splitOn "/" path
       in Just $ fromMaybe baseName $ T.stripSuffix ".php" baseName

    cleanPhpFunction func =
      T.replace "{closure}" "closure" $ fromMaybe func $ splitLast "->" <|> splitLast "::"
      where
        splitLast sep = viaNonEmpty last $ drop 1 $ T.splitOn sep func

    isPhpInApp = noneInfix ["/vendor/", "/phar://"]


-- | Parse .NET stack frame
-- Format: at Namespace.Class.Method(params) in /path/to/file.cs:line 123
parseDotNetFrame :: Text -> Maybe StackFrame
parseDotNetFrame line
  | "at " `T.isPrefixOf` T.strip line =
      let content = T.drop 3 $ T.strip line
          (methodPart, locationPart) = T.breakOn " in " content
          qualifiedMethod = T.takeWhile (/= '(') methodPart
          (moduleName, funcName) = splitDotted qualifiedMethod
          (filePath, lineNum) = parseDotNetLocation $ T.drop 4 locationPart
       in Just
            StackFrame
              { filePath = filePath
              , moduleName = Just moduleName
              , functionName = cleanDotNetFunction funcName
              , lineNumber = lineNum
              , columnNumber = Nothing
              , contextLine = Nothing
              , isInApp = isDotNetInApp qualifiedMethod
              }
  | otherwise = Nothing
  where
    parseDotNetLocation loc =
      let (path, lineStr) = T.breakOn ":line " loc
       in (path, readText $ T.drop 6 lineStr)

    cleanDotNetFunction =
      -- Remove generic arity: Method`1 -> Method
      T.takeWhile (/= '`')

    isDotNetInApp = nonePrefix ["System.", "Microsoft.", "Newtonsoft."]


-- | Generic stack frame parser for unknown formats
parseGenericFrame :: Text -> Maybe StackFrame
parseGenericFrame line =
  let trimmed = T.strip line
   in if T.null trimmed || "..." `T.isPrefixOf` trimmed
        then Nothing
        else
          Just
            StackFrame
              { filePath = trimmed
              , moduleName = Nothing
              , functionName = extractGenericFunction trimmed
              , lineNumber = extractGenericLineNumber trimmed
              , columnNumber = Nothing
              , contextLine = Nothing
              , isInApp = True -- Assume in-app by default
              }
  where
    extractGenericFunction txt =
      -- Try to find function-like patterns; fall back to full line to avoid collisions
      let parts = words txt
       in case find (\p -> "(" `T.isInfixOf` p || "." `T.isInfixOf` p) parts of
            Just p -> T.takeWhile (/= '(') p
            Nothing -> txt

    extractGenericLineNumber txt =
      -- Look for :NUMBER or line NUMBER patterns
      let parts = T.splitOn ":" txt
       in case parts of
            _ : rest -> listToMaybe $ mapMaybe readText rest
            _ -> Nothing


-- | Normalize a stack trace for fingerprinting
-- Returns a list of normalized frame strings suitable for hashing
--
-- >>> normalizeStackTrace "nodejs" "at handleRequest (/app/src/server.js:42:15)\nat processTicksAndRejections (node:internal/process/task_queues:95:5)"
-- "server|handleRequest"
--
-- >>> normalizeStackTrace "python" "File \"/app/main.py\", line 10, in main\nFile \"/app/utils.py\", line 5, in helper"
-- "main|main\nutils|helper"
--
-- >>> normalizeStackTrace "java" "at com.example.MyClass.doWork(MyClass.java:25)\nat com.example.Service.process(Service.java:50)"
-- "com.example.MyClass|doWork\ncom.example.Service|process"
--
-- >>> normalizeStackTrace "unknown" "some random frame info"
-- "some random frame info"
normalizeStackTrace :: Text -> Text -> Text
normalizeStackTrace runtime stackText =
  let frames = parseStackTrace runtime stackText
      inAppFrames = filter (.isInApp) frames
      framesToUse = if null inAppFrames then frames else inAppFrames
      normalizedFrames = map normalizeFrame framesToUse
   in T.intercalate "\n" normalizedFrames
  where
    normalizeFrame :: StackFrame -> Text
    normalizeFrame frame =
      let modulePart = fromMaybe "" frame.moduleName
          funcPart = normalizeFunction frame.functionName
          -- Context line: normalize whitespace, truncate if > 120 chars
          contextPart = maybe "" normalizeContextLine frame.contextLine
       in T.intercalate "|" $ filter (not . T.null) [modulePart, funcPart, contextPart]

    normalizeFunction :: Text -> Text
    normalizeFunction func =
      -- Common normalizations across platforms:
      -- 1. Remove memory addresses (0x...)
      -- 2. Remove generic type parameters
      -- 3. Remove parameter types
      -- 4. Normalize anonymous/lambda markers
      let noAddr = unwords $ filter (not . ("0x" `T.isPrefixOf`)) $ words func
          noGenerics = T.takeWhile (/= '<') noAddr
          noParams = T.takeWhile (/= '(') noGenerics
       in T.strip noParams

    normalizeContextLine :: Text -> Text
    normalizeContextLine ctx =
      let normalized = unwords $ words ctx
       in if T.length normalized > 120
            then "" -- Skip overly long context lines (like Sentry does)
            else normalized


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
normalizeMessage :: Text -> Text
normalizeMessage msg =
  let lns = take 2 $ filter (not . T.null . T.strip) $ T.splitOn "\n" $ T.replace "\r\n" "\n" msg
      combined = T.intercalate " " $ map T.strip lns
      -- Replace variable patterns with placeholders
      normalized = replaceAllFormats combined
   in T.strip normalized


-- | Compute the error fingerprint hash using Sentry-style prioritization
-- Priority:
-- 1. Stack trace (if has meaningful in-app frames)
-- 2. Exception type + message
-- 3. Message only
--
-- With stack trace - uses projectId, exceptionType, and normalized stack:
-- >>> computeErrorFingerprint "proj1" (Just "svc") (Just "span") "nodejs" "TypeError" "msg" "at handler (/app/index.js:10:5)"
-- "269748a1"
--
-- Without stack trace but with exception type - uses projectId, service, span, type, and message:
-- >>> computeErrorFingerprint "proj1" (Just "user-service") (Just "/api/users") "nodejs" "ValidationError" "Invalid email format" ""
-- "af0aa163"
--
-- Without stack trace or exception type - uses projectId, service, span, and message only:
-- >>> computeErrorFingerprint "proj1" (Just "api") (Just "/health") "nodejs" "" "Connection refused to 192.168.1.1:5432" ""
-- "44e50418"
--
-- Same error from different IPs produces same fingerprint (IP normalized):
-- >>> computeErrorFingerprint "proj1" (Just "db") Nothing "python" "" "Connection refused to 10.0.0.1:5432" "" == computeErrorFingerprint "proj1" (Just "db") Nothing "python" "" "Connection refused to 172.16.0.50:5432" ""
-- True
--
-- Same stack trace produces same fingerprint regardless of message:
-- >>> computeErrorFingerprint "proj1" Nothing Nothing "nodejs" "Error" "message 1" "at handler (/app/index.js:10:5)" == computeErrorFingerprint "proj1" Nothing Nothing "nodejs" "Error" "different message" "at handler (/app/index.js:10:5)"
-- True
--
-- Python: same in-app frame with different line numbers → same fingerprint (line numbers excluded from hash):
-- >>> computeErrorFingerprint "proj1" Nothing Nothing "python" "ValueError" "invalid literal" "File \"/app/main.py\", line 10, in handler\nFile \"/usr/lib/python3.11/json/decoder.py\", line 355, in raw_decode" == computeErrorFingerprint "proj1" Nothing Nothing "python" "ValueError" "invalid literal" "File \"/app/main.py\", line 99, in handler\nFile \"/usr/lib/python3.11/json/decoder.py\", line 400, in raw_decode"
-- True
--
-- Java: same in-app method at different lines → same fingerprint:
-- >>> computeErrorFingerprint "proj1" Nothing Nothing "java" "NullPointerException" "msg1" "at com.example.App.process(App.java:42)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1067)" == computeErrorFingerprint "proj1" Nothing Nothing "java" "NullPointerException" "msg2" "at com.example.App.process(App.java:99)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:2000)"
-- True
computeErrorFingerprint :: Text -> Maybe Text -> Maybe Text -> Text -> Text -> Text -> Text -> Text
computeErrorFingerprint projectIdText mService spanName runtime exceptionType message stackTrace =
  let
    -- Normalize components
    normalizedStack = normalizeStackTrace runtime stackTrace
    normalizedMsg = normalizeMessage message
    normalizedType = T.strip exceptionType

    -- Build fingerprint components based on priority
    fingerprintComponents =
      if
        | not (T.null normalizedStack) -> [projectIdText, normalizedType, normalizedStack]
        | not (T.null normalizedType) -> [projectIdText, fromMaybe "" mService, fromMaybe "" spanName, normalizedType, normalizedMsg]
        | otherwise -> [projectIdText, fromMaybe "" mService, fromMaybe "" spanName, normalizedMsg]

    -- Combine and hash
    combined = T.intercalate "|" $ filter (not . T.null) fingerprintComponents
   in
    toXXHash combined
