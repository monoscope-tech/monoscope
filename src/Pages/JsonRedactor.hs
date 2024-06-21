{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Pages.JsonRedactor (jsonRedactorGetH, jsonRedactorPostH,JsonRedactorForm) where

import Lucid
import Lucid.Htmx (hxPost_, hxTarget_)
import Data.Aeson (encode, decodeStrict, decode, encode, object, (.=), Value(..), Object)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy.Char8 as B
import Pages.BodyWrapper (BWConfig(..), bodyWrapper)
import Models.Projects.Projects qualified as Projects
import System.Types
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Data.Default (def)
import Models.Users.Sessions qualified as Sessions
import Relude hiding (for_, some)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Key (fromText)

import Text.Megaparsec (Parsec, runParser, eof, sepBy1, some)
import Text.Megaparsec.Char (char, alphaNumChar)
import Data.Void (Void)

type Parser = Parsec Void Text

data JsonRedactorForm = JsonRedactorForm
  { jsonInput :: Value
  , jsonPathInput :: [Text]
  }
  deriving (Show)

jsonRedactorPostH :: JsonRedactorForm -> Html ()
jsonRedactorPostH (JsonRedactorForm jsonInput jsonPathInput) = do
  let redactedJson = foldr redactJson jsonInput jsonPathInput
  let originalJsonText = encodeJson jsonInput
  let redactedJsonText = encodeJson redactedJson
  div_ [] $ do
    pre_ [id_ "original-json", class_ "json-viewer border p-4"] (toHtml originalJsonText)
    pre_ [id_ "redacted-json", class_ "json-viewer border p-4"] (toHtml redactedJsonText)
  where
    redactJson path json = case runParser jsonPathParser "" path of
      Left _ -> json
      Right jp -> applyRedaction jp json

    applyRedaction jp (Object obj) = Object $ redactInObject jp obj
    applyRedaction _ v = v

    redactInObject (key:rest) obj =
      case KeyMap.lookup (fromText key) obj of
        Just v  -> KeyMap.insert (fromText key) (applyRedaction rest v) obj
        Nothing -> obj
    redactInObject [] obj = obj

decodeJson :: Text -> Either String Value
decodeJson input = case decodeStrict (TE.encodeUtf8 input) of
  Just value -> Right value
  Nothing -> Left "Invalid JSON input."

encodeJson :: Value -> Text
encodeJson = TL.toStrict . TL.fromStrict . TE.decodeUtf8 . B.toStrict . encode

jsonPathParser :: Parser [Text]
jsonPathParser = do
  segments <- jsonPathSegment `sepBy1` char '.'
  eof
  return segments

jsonPathSegment :: Parser Text
jsonPathSegment = do
  segment <- some (char '_' <|> char '-' <|> char '[' <|> char ']' <|> char ':' <|> char '?' <|> alphaNumChar)
  return $ T.pack segment

jsonRedactorGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
jsonRedactorGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just $ Sessions.persistentSession sess
          , currProject = Just project
          , pageTitle = "Json Redactor"
          }
  addRespHeaders $ bodyWrapper bwconf $ jsonRedactor pid

jsonRedactor :: Projects.ProjectId -> Html ()
jsonRedactor _ = do
  section_ [class_ "container mx-auto px-4 py-10 overflow-hidden overflow-y-scroll"] $ do
    h1_ [class_ "text-2xl font-bold mb-4"] "JSON Redactor Tool"
    div_ [class_ "w-full max-w-lg flex flex-wrap -mx-2"] $ do
      div_ [class_ "w-full md:w-1/2 px-2"] $ do
        label_ [for_ "json-input", class_ "block text-gray-700"] "JSON Input (string or URL):"
        input_ [id_ "json-input", type_ "text", class_ "mt-1 p-2 w-full border border-gray-300 rounded", placeholder_ "{\"key\": \"value\"} or http://example.com/data.json"]

      div_ [class_ "w-full md:w-1/2 px-2"] $ do
        label_ [for_ "jsonpath-input", class_ "block text-gray-700 mt-4 md:mt-0"] "JSONPath Expression:"
        input_ [id_ "jsonpath-input", type_ "text", class_ "mt-1 p-2 w-full border border-gray-300 rounded", placeholder_ "$.path.to.redact"]

    button_ [id_ "redact-button", class_ "mt-4 px-4 py-2 bg-blue-500 text-white rounded", hxPost_ "/redact", hxTarget_ "#redacted-output"] "Redact JSON"

    div_ [class_ "content mt-6 flex"] $ do
      div_ [class_ "textarea-container w-1/2"] $ do
        h2_ [class_ "text-xl font-bold"] "Original JSON"
        pre_ [id_ "original-json", class_ "json-viewer border p-4"] ""

      div_ [class_ "textarea-container w-1/2"] $ do
        h2_ [class_ "text-xl font-bold"] "Redacted JSON"
        pre_ [id_ "redacted-output", class_ "json-viewer border p-4"] ""
