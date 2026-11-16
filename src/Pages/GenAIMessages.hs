-- | GenAI messages rendered from the conventions' parts shape: the span drawer's
-- Conversation tab and the conversation transcript share these.
module Pages.GenAIMessages (
  Call (..),
  genAIAt,
  genAINum,
  message_,
  isGenAISpan,
  genAIUsage,
  genAISpanDetail_,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty qualified as AEP
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as M
import Data.Scientific (toRealFloat)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Hasql.Interpolate qualified as HI
import Lucid
import Models.Ids qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.Components (BadgeColor (..), metaChip_, tonePill_)
import Pkg.DeriveUtils (AesonText)
import Pkg.GenAI qualified as GenAI
import Pkg.Parser.Eval (jsonAsText)
import Relude
import Utils (drawerRowAttrs_, faSprite_, getDurationNSMS, nonEmptyT, prettyPrintCount, tokenFlow)
import Web.Links


-- | One GenAI span without its message bodies: the list views read thousands of these.
data Call = Call
  { id :: Text
  , traceId :: Text
  , timestamp :: UTCTime
  , duration :: Int64
  , failed :: Bool
  , name :: Text
  , service :: Maybe Text
  , operation :: Text
  , conversation :: Maybe Text
  , agent :: Maybe Text
  , model :: Maybe Text
  , inputTokens :: Maybe Double
  , outputTokens :: Maybe Double
  , cost :: Maybe Double
  , tool :: Maybe Text
  , toolArgs :: Maybe Text
  , preview :: Maybe Text
  , finish :: Maybe (AesonText GenAI.FinishReasons)
  , user :: Maybe Text
  , attributes :: Maybe (AesonText (Map Text AE.Value))
  -- ^ Read only for a conversation's detail: it carries the message bodies.
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


genAIAt :: Maybe (Map Text AE.Value) -> [Text] -> Maybe AE.Value
genAIAt attrs ks = attrs >>= Telemetry.getNestedValue ("gen_ai" : ks)


genAINum :: Maybe (Map Text AE.Value) -> [Text] -> Maybe Double
genAINum attrs ks = genAIAt attrs ks >>= \case AE.Number n -> Just (toRealFloat n); AE.String t -> readMaybe (toString t); _ -> Nothing


-- | Tool arguments and results usually arrive as JSON strings; indent them when they parse.
prettyJson :: AE.Value -> Text
prettyJson v = maybe (jsonAsText v) (decodeUtf8 . AEP.encodePretty) $ case v of
  AE.String t -> AE.decodeStrict @AE.Value (encodeUtf8 t)
  _ -> Just v


message_ :: Projects.ProjectId -> Map Text Call -> Maybe Call -> AE.Value -> Html ()
message_ pid tools stepM m = case role of
  "system" -> details_ [class_ "rounded-lg border border-strokeWeak bg-fillWeaker px-3 py-2 text-sm"] do
    summary_ [class_ "cursor-pointer text-xs font-medium text-textWeak"] "System instructions"
    div_ [class_ "mt-2 max-h-96 overflow-auto"] parts_
  _ -> article_ [class_ $ "flex flex-col gap-1.5 rounded-lg border px-3 py-2 " <> if role == "user" then "border-strokeBrand-weak bg-fillBrand-weak/40" else "border-strokeWeak bg-bgRaised"] do
    div_ [class_ "flex flex-wrap items-center gap-2 text-xs"] do
      span_ [class_ "font-semibold uppercase tracking-wide text-textWeak"] $ toHtml $ T.toTitle role
      whenJust stepM \c -> do
        span_ [class_ "text-textWeak"] $ toHtml $ T.intercalate " · " $ catMaybes [c.model, Just $ toText $ getDurationNSMS $ fromIntegral c.duration, tokenFlow c.inputTokens c.outputTokens, GenAI.formatUsd <$> c.cost]
        when c.failed $ tonePill_ ErrorBadge "Error"
        whenJust (UUID.fromText c.id) \u -> button_ ([type_ "button", class_ "ms-auto text-textBrand hover:underline"] <> drawerRowAttrs_ ((urls.logExplorer pid).logExplorerItemDetailedGet u c.timestamp Nothing (Just "tab-genai") Nothing False)) "Open span"
    parts_
  where
    role = case m of AE.Object o | Just (AE.String r) <- KM.lookup "role" o -> r; _ -> "message"
    parts = case m of AE.Object o | Just (AE.Array ps) <- KM.lookup "parts" o -> toList ps; _ -> [m]
    parts_ = div_ [class_ "flex flex-col gap-2"] $ forM_ parts (part_ tools)


part_ :: Map Text Call -> AE.Value -> Html ()
part_ tools p = case (field "type", p) of
  (Just "text", _) -> textBlock_ $ maybe "" jsonAsText $ fieldV "content"
  (Just "reasoning", _) -> details_ [class_ "text-sm"] $ summary_ [class_ "cursor-pointer text-xs text-textWeak"] "Reasoning" >> textBlock_ (maybe "" jsonAsText $ fieldV "content")
  (Just "tool_call", _) -> div_ [class_ "flex flex-col gap-1 rounded-md border border-strokeWeak bg-fillWeaker px-2 py-1.5 font-mono text-xs"] do
    div_ [class_ "flex flex-wrap items-center gap-2"] do
      faSprite_ "function" "regular" "h-3 w-3 text-iconNeutral"
      span_ [class_ "font-semibold text-textStrong"] $ toHtml $ fromMaybe "tool" $ field "name"
      whenJust (field "id" >>= (`M.lookup` tools)) \t -> do
        span_ [class_ "text-textWeak"] $ toHtml $ getDurationNSMS $ fromIntegral t.duration
        when t.failed $ tonePill_ ErrorBadge "Failed"
    whenJust (fieldV "arguments") $ pre_ [class_ "max-h-48 overflow-auto whitespace-pre-wrap break-all text-textWeak"] . toHtml . prettyJson
  (Just "tool_call_response", _) -> details_ [class_ "rounded-md border border-strokeWeak px-2 py-1.5 text-xs"] do
    summary_ [class_ "cursor-pointer font-mono text-textWeak"] $ toHtml $ "Result" <> maybe "" ((" · " <>) . (\t -> fromMaybe t.name t.tool)) (field "id" >>= (`M.lookup` tools)) <> " · " <> show (T.length response) <> " chars"
    pre_ [class_ "mt-1 max-h-96 overflow-auto whitespace-pre-wrap break-all font-mono text-textWeak"] $ toHtml $ maybe "" prettyJson $ fieldV "response"
  (_, AE.String t) -> textBlock_ t
  _ -> pre_ [class_ "overflow-auto whitespace-pre-wrap break-all font-mono text-xs text-textWeak"] $ toHtml $ jsonAsText p
  where
    fieldV k = case p of AE.Object o -> KM.lookup (fromString $ toString k) o; _ -> Nothing
    field k = fieldV k >>= \case AE.String t -> Just t; _ -> Nothing
    response = maybe "" jsonAsText $ fieldV "response"
    textBlock_ = div_ [class_ "whitespace-pre-wrap break-words text-sm text-textStrong"] . toHtml


-- * Span drawer


-- | A model call's tokens and cost in one line, for the waterfall and other dense rows.
genAIUsage :: Maybe (Map Text AE.Value) -> Maybe Text
genAIUsage attrs = nonEmptyT $ Just $ T.intercalate " · " $ catMaybes [tokenFlow (number ["usage", "input_tokens"]) (number ["usage", "output_tokens"]), GenAI.formatUsd <$> number ["usage", "cost"]]
  where
    number = genAINum attrs


isGenAISpan :: Maybe (Map Text AE.Value) -> Bool
isGenAISpan attrs = isJust $ genAIAt attrs ["operation", "name"]


-- | The drawer's Conversation tab for one GenAI span: usage, then its messages or tool I/O.
genAISpanDetail_ :: Projects.ProjectId -> Maybe (Map Text AE.Value) -> Html ()
genAISpanDetail_ pid attrs = div_ [class_ "flex flex-col gap-3 p-2"] do
  div_ [class_ "flex flex-wrap gap-1.5"] $ forM_ facts $ uncurry metaChip_
  whenJust (at ["system_instructions"]) \v -> message_ pid M.empty Nothing (AE.object ["role" AE..= ("system" :: Text), "parts" AE..= ([AE.object ["type" AE..= ("text" :: Text), "content" AE..= jsonAsText v]] :: [AE.Value])])
  forM_ (list "input" <> list "output") (message_ pid M.empty Nothing)
  when (op == Just "execute_tool") do
    whenJust (at ["tool", "call", "arguments"]) \v -> part_ M.empty (AE.object ["type" AE..= ("tool_call" :: Text), "name" AE..= txt ["tool", "name"], "arguments" AE..= v])
    whenJust (at ["tool", "call", "result"]) \v -> part_ M.empty (AE.object ["type" AE..= ("tool_call_response" :: Text), "response" AE..= v])
  where
    at = genAIAt attrs
    txt ks = at ks >>= \case AE.String t -> Just t; _ -> Nothing
    op = txt ["operation", "name"]
    list dir = case at [dir, "messages"] of Just (AE.Array ms) -> toList ms; _ -> []
    number = genAINum attrs
    facts =
      catMaybes
        [ ("Operation",) <$> op
        , ("Provider",) <$> txt ["provider", "name"]
        , ("Model",) <$> (txt ["response", "model"] <|> txt ["request", "model"])
        , ("Agent",) <$> txt ["agent", "name"]
        , ("Tool",) <$> txt ["tool", "name"]
        , ("Input tokens",) . prettyPrintCount . round <$> number ["usage", "input_tokens"]
        , ("Output tokens",) . prettyPrintCount . round <$> number ["usage", "output_tokens"]
        , ("Cache read",) . prettyPrintCount . round <$> number ["usage", "cache_read", "input_tokens"]
        , (\c -> ("Cost", GenAI.formatUsd c <> maybe "" (" " <>) (txt ["usage", "cost_source"]))) <$> number ["usage", "cost"]
        , ("Finish",) . jsonAsText <$> at ["response", "finish_reasons"]
        , ("Conversation",) <$> txt ["conversation", "id"]
        ]
