module Pkg.Icons.TH (embedIconEntries) where

import Data.List (lookup)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Relude


-- | Read Font Awesome sprite sheets at compile time and embed their symbols as
-- @(kind, name, viewBox, presentation attributes, body)@ tuples.
--
-- Keeping this in a separate module satisfies Template Haskell's stage
-- restriction: the parser must be compiled before the splice which invokes it.
embedIconEntries :: [(String, FilePath)] -> TH.Q TH.Exp
embedIconEntries sprites = do
  entries <- fmap concat $ forM sprites \(kind, path) -> do
    TH.qAddDependentFile path
    contents <- TH.runIO $ TIO.readFile path
    either (fail . ((path <> ": ") <>)) pure $ parseSprite kind contents
  -- SVG fragment lookup resolves the first matching id. Preserve that behavior
  -- for the few legacy duplicate symbols already present in the sheets.
  TH.lift $ deduplicate entries


type EmbeddedIcon = (String, String, String, [(String, String)], String)


parseSprite :: String -> Text -> Either String [EmbeddedIcon]
parseSprite kind contents = stripComments contents >>= go []
  where
    go acc source = case T.breakOn "<symbol" source of
      (_, rest)
        | T.null rest ->
            if null acc
              then Left "sprite contains no <symbol> elements"
              else Right $ reverse acc
      (_, rest) -> do
        let (opening, afterOpening) = T.breakOn ">" rest
        when (T.null afterOpening) $ Left "unterminated <symbol> opening tag"
        attrs <- parseAttributes $ T.drop (T.length "<symbol") opening
        name <- requiredAttribute "id" attrs
        viewBox <- requiredAttribute "viewBox" attrs
        let (body, afterBody) = T.breakOn "</symbol>" $ T.drop 1 afterOpening
        when (T.null afterBody) $ Left $ "unterminated <symbol> for icon " <> toString name
        let presentationAttrs = filter (\(key, _) -> key /= "id" && key /= "viewBox" && key /= "width" && key /= "height") attrs
            entry =
              ( kind
              , toString name
              , toString viewBox
              , map (bimap toString toString) presentationAttrs
              , toString $ minifyMarkup $ prefixInternalIds (toText kind <> "-" <> name) body
              )
        go (entry : acc) $ T.drop (T.length "</symbol>") afterBody


deduplicate :: [EmbeddedIcon] -> [EmbeddedIcon]
deduplicate = go []
  where
    go _ [] = []
    go seen (entry@(kind, name, _, _, _) : rest)
      | (kind, name) `elem` seen = go seen rest
      | otherwise = entry : go ((kind, name) : seen) rest


stripComments :: Text -> Either String Text
stripComments source = case T.breakOn "<!--" source of
  (before, rest)
    | T.null rest -> Right before
    | otherwise -> do
        let (_, afterComment) = T.breakOn "-->" $ T.drop (T.length "<!--") rest
        when (T.null afterComment) $ Left "unterminated XML comment"
        (before <>) <$> stripComments (T.drop (T.length "-->") afterComment)


requiredAttribute :: Text -> [(Text, Text)] -> Either String Text
requiredAttribute key attrs =
  maybe (Left $ "<symbol> is missing required " <> toString key <> " attribute") Right $ lookup key attrs


parseAttributes :: Text -> Either String [(Text, Text)]
parseAttributes = go [] . T.strip
  where
    go acc input
      | T.null input = Right $ reverse acc
      | otherwise = do
          let (key, afterKey) = T.breakOn "=" input
              trimmedKey = T.strip key
          when (T.null trimmedKey || T.null afterKey) $ Left $ "invalid <symbol> attributes: " <> toString input
          let valueStart = T.stripStart $ T.drop 1 afterKey
          unless (T.isPrefixOf "\"" valueStart) $ Left $ "attribute value must use double quotes: " <> toString input
          let (value, afterValue) = T.breakOn "\"" $ T.drop 1 valueStart
          when (T.null afterValue) $ Left $ "unterminated attribute value: " <> toString input
          go ((trimmedKey, value) : acc) $ T.stripStart $ T.drop 1 afterValue


-- The sprite formatting is useful to humans but wasteful when repeated in page
-- HTML. XML whitespace between/inside tags is insignificant for these paths.
minifyMarkup :: Text -> Text
minifyMarkup = T.unwords . T.words


-- Inline SVGs share the document id namespace. Prefix definitions and their
-- local URL references so gradients/clip paths from different icons cannot
-- accidentally resolve to one another.
prefixInternalIds :: Text -> Text -> Text
prefixInternalIds prefix markup = foldl' rewrite markup $ internalIds markup
  where
    rewrite body internalId =
      T.replace ("url(#" <> internalId <> ")") ("url(#" <> prefixed internalId <> ")")
        $ T.replace ("id=\"" <> internalId <> "\"") ("id=\"" <> prefixed internalId <> "\"") body
    prefixed internalId = prefix <> "-" <> internalId


internalIds :: Text -> [Text]
internalIds = go
  where
    go source = case T.breakOn "id=\"" source of
      (_, rest) | T.null rest -> []
      (_, rest) ->
        let valueStart = T.drop (T.length "id=\"") rest
            (value, afterValue) = T.breakOn "\"" valueStart
         in value : go (T.drop 1 afterValue)
