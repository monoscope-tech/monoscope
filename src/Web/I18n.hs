{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Internationalisation. Translations live as one YAML file per language
-- in @static/i18n/@. They're embedded into the binary at compile time via
-- 'file-embed' so the runtime stays pure and we don't need to ship the YAML
-- separately. Editing a translation still requires a rebuild — but it's a
-- data-only change, so contributors don't need to read Haskell to add a new
-- language: drop a @static/i18n/<code>.yaml@, add its constructor to 'Language'
-- and 'dict', and rebuild.
module Web.I18n (
  Language (..),
  parseLanguage,
  languageCode,
  t,
  languageFromCookies,
  languageSetCookieBS,
)
where

import Data.FileEmbed (embedFile)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text.Display (Display, display)
import Data.Time.Clock (secondsToDiffTime)
import Data.Yaml qualified as Yaml
import Pkg.Deriving (WrappedEnumSC (..))
import Relude
import Web.Cookie (Cookies, SetCookie (..), defaultSetCookie)
import Web.HttpApiData (FromHttpApiData (..))


-- | The constructor, lowercased, is the language code.
--
-- >>> (map languageCode [En, Es], map parseLanguage ["es", "en", "fr"])
-- (["en","es"],[Es,En,En])
data Language = En | Es
  deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)
  deriving (Display, FromHttpApiData) via WrappedEnumSC 'Nothing "" Language


-- | Unknown codes fall back to English.
parseLanguage :: Text -> Language
parseLanguage = fromRight En . parseUrlPiece


languageCode :: Language -> Text
languageCode = display


-- | Translate a key in the given language. Missing keys return the key itself
-- so typos surface visibly in the UI (rather than rendering empty strings).
t :: Language -> Text -> Text
t lang key = fromMaybe key (Map.lookup key (dict lang))


dict :: Language -> Map.Map Text Text
dict En = en
dict Es = es


-- | Embed YAML at compile time. Decoded once at module load (lazy CAF) so
-- subsequent lookups are pure map reads. If a YAML file is malformed the
-- binary fails to start with a clear error rather than serving wrong text.
decodeEmbedded :: ByteString -> Map.Map Text Text
decodeEmbedded = either (error . ("Web.I18n: failed to decode embedded translations: " <>) . show) id . Yaml.decodeEither'


en :: Map.Map Text Text
en = decodeEmbedded $(embedFile "static/i18n/en.yaml")


es :: Map.Map Text Text
es = decodeEmbedded $(embedFile "static/i18n/es.yaml")


languageFromCookies :: Cookies -> Language
languageFromCookies = maybe En (parseLanguage . decodeUtf8) . L.lookup "lang"


languageSetCookieBS :: Language -> SetCookie
languageSetCookieBS lang =
  defaultSetCookie
    { setCookieName = "lang"
    , setCookieValue = encodeUtf8 (languageCode lang)
    , setCookiePath = Just "/"
    , setCookieMaxAge = Just (secondsToDiffTime $ 365 * 24 * 60 * 60)
    }
