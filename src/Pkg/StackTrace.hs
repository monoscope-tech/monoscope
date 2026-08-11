-- | Turning a stack trace from an opaque blob into frames.
--
-- Every runtime prints its stack differently and none of them are structured, so an
-- exception reaches us as one string. Rendering that string in a @\<pre\>@ is what we did
-- before, and it is why the error panel could never say "the failure is line 88 of
-- @checkout.py@, here is the code" the way Sentry and Datadog do — nothing downstream knew
-- where one frame ended and the next began.
--
-- The parse is deliberately total and lossy-nowhere: a line that matches no known shape
-- becomes a 'Frame' carrying only its text, so the rendered trace is always the whole trace.
-- Recognising a frame is an upgrade (it gets a file, a line, a source snippet), never a
-- filter.
module Pkg.StackTrace (Frame (..), parseStackTrace, frameFromAttributes, framesFor, isInApp) where

import Data.Char (isDigit)
import Data.Text qualified as T
import Relude


-- | One line of a stack trace. @file@\/@line@ are 'Nothing' for a line whose shape we do not
-- recognise (and for the runtimes that genuinely omit them), which is why they are optional
-- rather than defaulted — @line 0@ would be a lie a snippet fetcher would act on.
data Frame = Frame
  { raw :: Text
  -- ^ The line exactly as the runtime printed it. Always rendered, so nothing is lost.
  , function :: Maybe Text
  , file :: Maybe Text
  , line :: Maybe Int
  }
  deriving stock (Eq, Generic, Show)


-- | Does this frame look like the user's own code, rather than a runtime or a dependency?
--
-- Sentry calls this @in_app@ and expands the first such frame by default, which is almost
-- always the one you want during an incident: the top of a stack is usually five frames of
-- framework before anything you wrote.
--
-- A path test, because that is all a stack trace gives us. It is a heuristic and it is
-- allowed to be wrong — being wrong costs one extra click, and the alternative (a
-- per-project configuration nobody fills in) costs the feature.
--
-- >>> isInApp Frame{raw = "", function = Nothing, file = Just "app/services/checkout.py", line = Just 1}
-- True
--
-- >>> let fileFrame p = Frame{raw = "", function = Nothing, file = Just p, line = Nothing}
-- >>> map (isInApp . fileFrame) ["/usr/lib/python3.11/json/decoder.py", "node_modules/express/lib/router.js", "vendor/bundle/gems/rack.rb", "C:\\Program Files\\dotnet\\Sys.dll"]
-- [False,False,False,False]
--
-- A frame with no file cannot be judged, and is treated as not-in-app so it never steals the
-- default expansion from a frame that does have a file:
--
-- >>> isInApp Frame{raw = "???", function = Nothing, file = Nothing, line = Nothing}
-- False
isInApp :: Frame -> Bool
isInApp f = case f.file of
  Nothing -> False
  Just p -> not $ any (`T.isInfixOf` normalized p) vendorMarkers
  where
    normalized = T.toLower . T.replace "\\" "/"
    vendorMarkers =
      [ "node_modules/"
      , "site-packages/"
      , "dist-packages/"
      , "/vendor/"
      , "vendor/bundle"
      , "/gems/"
      , "/usr/lib/"
      , "/usr/local/lib/"
      , "program files"
      , "/.cargo/"
      , "/go/pkg/mod/"
      , "runtime/"
      , "<anonymous>"
      , "<frozen "
      ]


-- | Parse a raw stack trace into frames, one per non-blank line.
--
-- One parser per runtime family, chosen by the line's shape rather than by a declared
-- language: the language is not on the span, and a polyglot trace (a Node service reporting a
-- Python subprocess's failure) is a real thing.
--
-- JavaScript\/Node — @at fn (file:line:col)@, with and without the function:
--
-- >>> parseStackTrace "    at checkout (/srv/app/checkout.js:88:15)\n    at /srv/app/index.js:12:3"
-- [Frame {raw = "    at checkout (/srv/app/checkout.js:88:15)", function = Just "checkout", file = Just "/srv/app/checkout.js", line = Just 88},Frame {raw = "    at /srv/app/index.js:12:3", function = Nothing, file = Just "/srv/app/index.js", line = Just 12}]
--
-- Python — the file line and the source line the traceback echoes underneath it:
--
-- >>> map (.file) $ parseStackTrace "Traceback (most recent call last):\n  File \"app/checkout.py\", line 88, in charge\n    raise ValueError(x)"
-- [Nothing,Just "app/checkout.py",Nothing]
--
-- >>> [(f.function, f.line) | f <- parseStackTrace "  File \"app/checkout.py\", line 88, in charge"]
-- [(Just "charge",Just 88)]
--
-- Java\/Kotlin, Ruby, Go and PHP:
--
-- >>> [(f.file, f.line) | f <- parseStackTrace "\tat com.acme.Checkout.charge(Checkout.java:88)"]
-- [(Just "Checkout.java",Just 88)]
--
-- >>> [(f.file, f.line, f.function) | f <- parseStackTrace "app/models/order.rb:88:in `charge'"]
-- [(Just "app/models/order.rb",Just 88,Just "charge")]
--
-- >>> [(f.file, f.line) | f <- parseStackTrace "\t/srv/app/checkout.go:88 +0x1f5"]
-- [(Just "/srv/app/checkout.go",Just 88)]
--
-- >>> [(f.file, f.line, f.function) | f <- parseStackTrace "#0 /srv/app/Checkout.php(88): Acme\\Checkout->charge()"]
-- [(Just "/srv/app/Checkout.php",Just 88,Just "Acme\\Checkout->charge")]
--
-- An unrecognised line keeps its text and loses nothing, and blank lines are dropped so the
-- frame list has no empty rows:
--
-- >>> parseStackTrace "ValueError: nope\n\n"
-- [Frame {raw = "ValueError: nope", function = Nothing, file = Nothing, line = Nothing}]
--
-- >>> parseStackTrace "   \n"
-- []
parseStackTrace :: Text -> [Frame]
parseStackTrace = map parseFrame . filter (not . T.null . T.strip) . lines


parseFrame :: Text -> Frame
parseFrame raw = fromMaybe bare $ asum [pyFile, atFrame, php, ruby, bareLocation]
  where
    bare = Frame raw Nothing Nothing Nothing
    t = T.strip raw
    frame fn fl ln = Just bare{function = fn, file = fl, line = ln}

    -- Python:  File "app/checkout.py", line 88, in charge
    pyFile = do
      rest <- T.stripPrefix "File \"" t
      let (path, afterPath) = T.breakOn "\"" rest
      lineRest <- T.stripPrefix "\", line " afterPath
      let (num, afterNum) = T.span isDigit lineRest
      n <- readMaybe (toString num)
      frame (T.stripPrefix ", in " afterNum) (Just path) (Just n)

    -- JS/Node:      at fn (file:line:col)  |  at file:line:col
    -- Java/Kotlin:  at pkg.Class.method(File.java:88)
    -- One parser, because they are one grammar: an `at`, an optional qualified name, and a
    -- parenthesised or bare location. Splitting them produced two near-identical parsers
    -- where the first already answered for the second.
    atFrame = do
      rest <- T.stripPrefix "at " t
      let (fn, paren) = T.breakOn "(" rest
          located = if T.null paren then rest else T.dropEnd 1 (T.drop 1 paren)
      (path, n) <- fileAndLine located
      frame (nonBlank fn <* guard (not (T.null paren))) (Just path) (Just n)

    -- PHP:  #0 /srv/app/Checkout.php(88): Acme\Checkout->charge()
    php = do
      rest <- T.stripPrefix "#" t
      let afterIdx = T.strip $ T.dropWhile isDigit rest
          (path, paren) = T.breakOn "(" afterIdx
      guard $ not (T.null path) && not (T.null paren)
      let (num, afterNum) = T.span isDigit (T.drop 1 paren)
      n <- readMaybe (toString num)
      guard $ "):" `T.isPrefixOf` afterNum
      frame (nonBlank $ T.dropEnd 2 $ T.drop 2 afterNum) (Just path) (Just n)

    -- Ruby:  app/models/order.rb:88:in `charge'
    ruby = do
      let (loc, inPart) = T.breakOn ":in " t
      guard $ not (T.null inPart)
      (path, n) <- fileAndLine loc
      frame (nonBlank $ T.dropWhileEnd (== '\'') $ T.dropWhile (== '`') $ T.strip $ T.drop 4 inPart) (Just path) (Just n)

    -- Go:  \t/srv/app/checkout.go:88 +0x1f5
    -- Last, because "path:line" with nothing around it is the shape every other parser's
    -- input also contains — it must only get the lines none of them claimed.
    bareLocation = do
      (path, n) <- fileAndLine (T.takeWhile (/= ' ') t)
      frame Nothing (Just path) (Just n)


-- | @path:line@ (optionally @:col@, optionally with a trailing @in \`fn'@), split from the
-- right so a Windows drive letter or a URL scheme in the path does not swallow the line.
fileAndLine :: Text -> Maybe (Text, Int)
fileAndLine s = case reverse (T.splitOn ":" s) of
  _col : num : rest | Just n <- readMaybe (toString num), not (null rest) -> Just (T.intercalate ":" (reverse rest), n)
  num : rest | Just n <- readMaybe (toString num), not (null rest) -> Just (T.intercalate ":" (reverse rest), n)
  _ -> Nothing


nonBlank :: Text -> Maybe Text
nonBlank = guarded (not . T.null) . T.strip


-- | The frame an SDK already told us about.
--
-- OpenTelemetry's @code.*@ conventions are promoted columns on every span, so a runtime that
-- follows them has handed us the failing frame directly and there is nothing to parse. This
-- is the same shortcut Sentry takes with SDK-attached source context: the most reliable
-- answer is the one the process itself reported.
--
-- Takes a lookup rather than a map so it stays independent of however the caller stores
-- attributes — the span's attribute map is nested @Value@s, not @Text@.
--
-- >>> let attrs ps k = viaNonEmpty head [v | (k', v) <- ps, k' == k]
-- >>> frameFromAttributes (attrs [("code.file.path", "app/checkout.py"), ("code.line.number", "88"), ("code.function.name", "charge")])
-- Just (Frame {raw = "app/checkout.py:88 in charge", function = Just "charge", file = Just "app/checkout.py", line = Just 88})
--
-- A path with no line is still a frame — it just cannot anchor a snippet:
--
-- >>> (.line) <$> frameFromAttributes (attrs [("code.file.path", "app/checkout.py")])
-- Just Nothing
--
-- >>> frameFromAttributes (attrs [("code.line.number", "88")])
-- Nothing
frameFromAttributes :: (Text -> Maybe Text) -> Maybe Frame
frameFromAttributes attr = do
  path <- nonBlank =<< attr "code.file.path"
  let ln = readMaybe . toString =<< attr "code.line.number"
      fn = nonBlank =<< attr "code.function.name"
  pure Frame{raw = path <> maybe "" ((":" <>) . show) ln <> maybe "" (" in " <>) fn, function = fn, file = Just path, line = ln}


-- | The frames to show for an exception: the printed stack if there is one, else the single
-- frame the OTel @code.*@ conventions already gave us.
--
-- One function so the renderer and its caller cannot disagree about whether there is
-- anything to render — a \"Stack trace (0 frames)\" disclosure that opens onto nothing is
-- exactly the sort of drift two separate checks produce.
--
-- >>> map (.line) $ framesFor (const Nothing) "  File \"a.py\", line 3, in f"
-- [Just 3]
--
-- >>> map (.file) $ framesFor (\k -> viaNonEmpty head ["a.py" | k == "code.file.path"]) ""
-- [Just "a.py"]
--
-- >>> framesFor (const Nothing) ""
-- []
framesFor :: (Text -> Maybe Text) -> Text -> [Frame]
framesFor attr stack = case parseStackTrace stack of
  [] -> maybeToList (frameFromAttributes attr)
  fs -> fs
