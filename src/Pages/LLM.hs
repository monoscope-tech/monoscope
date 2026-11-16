{-# LANGUAGE OrPatterns #-}

-- | LLM & agent observability: conversations, model calls, tools, tokens and cost, read
-- from GenAI spans that 'Pkg.GenAI' normalised at ingest (docs/llm-observability.md).
module Pages.LLM (
  llmGetH,
  conversationGetH,
  LLMGet (..),
  LLMBody (..),
  ConversationGet (..),
  Conversation (..),
  Signal (..),
  groupConversations,
) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Foldable1 qualified as F1
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Ids qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Components (BadgeColor (..), Deferred (..), EmptyStateAction (..), RefreshingDeferred (..), callout_, localTimeFmt_, metaChip_, tableSkeleton_, tonePill_, withDeferredBody)
import Pages.Containers (mkWindow, windowed)
import Pages.GenAIMessages (Call (..), genAIAt, message_)
import Pkg.Components.Table (Column, Features (..), Table (..), ZeroState (..), col, countedFilter, facetActions, inventoryTable, withAttrs)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (AesonText (..), unAesonTextMaybe)
import Pkg.GenAI qualified as GenAI
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, useTfReads)
import Utils (TabStrip (..), drawerRowAttrs_, faSprite_, getDurationNSMS, navTabStrip_, prettyPrintCount)
import Web.Links


data Signal = ToolError | ModelError | Truncated | Filtered | Looping | ContextPressure
  deriving stock (Bounded, Enum, Eq, Ord, Show)


signalLabel :: Signal -> Text
signalLabel = \case
  ToolError -> "Tool error"
  ModelError -> "Model error"
  Truncated -> "Truncated"
  Filtered -> "Filtered"
  Looping -> "Repeated tool call"
  ContextPressure -> "Context ≥80%"


signalHelp :: Signal -> Text
signalHelp = \case
  ToolError -> "A tool call failed."
  ModelError -> "A model call returned an error."
  Truncated -> "A reply stopped at the output token limit."
  Filtered -> "A reply was refused or cut by a content filter."
  Looping -> "The same tool ran with the same arguments three or more times."
  ContextPressure -> "A call used at least 80% of the model's context window."


data Conversation = Conversation
  { key :: Text
  , title :: Maybe Text
  , started :: UTCTime
  , lastSeen :: UTCTime
  , agents :: [Text]
  , models :: [Text]
  , services :: [Text]
  , users :: [Text]
  , turns :: Int
  , llmCalls :: Int
  , toolCalls :: Int
  , tokens :: Double
  , cost :: Maybe Double
  , errors :: Int
  , activeNs :: Int64
  , signals :: [Signal]
  }
  deriving stock (Generic, Show)


inference :: Call -> Bool
inference = GenAI.isInferenceOp . (.operation)


abnormal :: Call -> [GenAI.Abnormal]
abnormal c = maybe [] (\(AesonText (GenAI.FinishReasons rs)) -> mapMaybe GenAI.abnormalFinish rs) c.finish


-- | A trace joins the conversation any of its spans names; frameworks often set the id on the
-- root agent span only. A trace with no id is its own conversation if it ran an agent, and
-- otherwise lists only as calls, so single classifier calls don't drown real conversations.
--
-- >>> import Relude.Unsafe qualified as Unsafe
-- >>> let t = Unsafe.read @UTCTime "2026-01-01 00:00:00 UTC"
-- >>> let c tr op conv = Call "s" tr t 1000 False op Nothing op conv Nothing (Just "gpt-4o") (Just 10) (Just 5) (Just 0.01) Nothing Nothing (Just "hi") Nothing Nothing Nothing
-- >>> map (\x -> (x.key, x.turns, x.llmCalls)) $ groupConversations $ V.fromList [c "t1" "invoke_agent" (Just "conv"), c "t1" "chat" Nothing, c "t2" "chat" (Just "conv"), c "t3" "chat" Nothing, c "t4" "invoke_agent" Nothing]
-- [("conv",2,2),("t4",1,0)]
groupConversations :: V.Vector Call -> [Conversation]
groupConversations calls = sortOn (Down . (.lastSeen)) $ mapMaybe conversation $ M.toList grouped
  where
    -- Logs carry no trace (Claude Code's events): each is its own unit, never one shared "" trace.
    traces = M.fromListWith (<>) [(if T.null c.traceId then c.id else c.traceId, [c]) | c <- V.toList calls]
    traceKey (unit, spans) = asum (map (.conversation) spans) <|> (unit <$ guard (any ((`elem` ["invoke_agent", "invoke_workflow"]) . (.operation)) spans))
    grouped = M.fromListWith (<>) [(k, [spans]) | (unit, spans) <- M.toList traces, Just k <- [traceKey (unit, spans)]]
    conversation (k, traceGroups) = do
      let spans = sortOn (.timestamp) $ concat traceGroups
          chats = filter inference spans
          tools = filter ((== "execute_tool") . (.operation)) spans
          distinct f = ordNub $ mapMaybe f spans
          end c = addUTCTime (fromIntegral c.duration / 1e9) c.timestamp
          traceSpan ss = fromMaybe 0 $ do
            ne <- nonEmpty ss
            pure $ round $ 1e9 * diffUTCTime (F1.maximum $ fmap end ne) (F1.minimum $ fmap (.timestamp) ne)
      first' <- viaNonEmpty head spans
      pure
        Conversation
          { key = k
          , title = asum $ map (.preview) spans
          , started = first'.timestamp
          , lastSeen = maybe first'.timestamp (F1.maximum . fmap end) (nonEmpty spans)
          , agents = distinct (.agent)
          , models = ordNub $ mapMaybe (.model) chats
          , services = distinct (.service)
          , users = distinct (.user)
          , turns = length traceGroups
          , llmCalls = length chats
          , toolCalls = length tools
          , tokens = sum $ mapMaybe (\c -> (+) <$> c.inputTokens <*> c.outputTokens <|> c.inputTokens <|> c.outputTokens) chats
          , cost = viaNonEmpty sum (mapMaybe (.cost) chats)
          , errors = length $ filter (.failed) spans
          , activeNs = sum $ map traceSpan traceGroups
          , -- Deterministic rules, in 'Signal' order, so each finding is explainable and free.
            signals =
              [ sig
              | (sig, True) <-
                  zip
                    universe
                    [ any (\c -> c.failed && c.operation == "execute_tool") spans
                    , any (\c -> c.failed && inference c) spans
                    , any ((GenAI.Truncated `elem`) . abnormal) spans
                    , any ((GenAI.Filtered `elem`) . abnormal) spans
                    , any (>= 3) $ M.fromListWith (+) [((t, c.toolArgs), 1 :: Int) | c <- spans, Just t <- [c.tool]]
                    , any pressured spans
                    ]
              ]
          }
    pressured c = fromMaybe False do
      window <- c.model >>= GenAI.lookupPrice >>= (.contextWindow)
      used <- c.inputTokens
      pure $ used >= 0.8 * fromIntegral window


-- | The window's GenAI spans, newest first and capped so a busy project stays one bounded scan.
-- Lists skip the attributes blob, where message bodies live; a conversation's detail reads it.
selectCalls :: Projects.ProjectId -> TimePicker.TimeWindow -> Bool -> HI.Sql -> ATAuthCtx [Call]
selectCalls pid window bodies scope = do
  tf <- useTfReads
  Hasql.withHasqlTimefusion tf
    $ Hasql.interp
    $ [HI.sql|
      SELECT id::text, COALESCE(context___trace_id, ''), timestamp, COALESCE(duration, 0), COALESCE(status_code = 'ERROR', false), COALESCE(name, ''), resource___service___name,
        attributes -> 'gen_ai' -> 'operation' ->> 'name',
        attributes -> 'gen_ai' -> 'conversation' ->> 'id',
        attributes -> 'gen_ai' -> 'agent' ->> 'name',
        COALESCE(attributes -> 'gen_ai' -> 'response' ->> 'model', attributes -> 'gen_ai' -> 'request' ->> 'model'),
        (attributes -> 'gen_ai' -> 'usage' ->> 'input_tokens')::float,
        (attributes -> 'gen_ai' -> 'usage' ->> 'output_tokens')::float,
        (attributes -> 'gen_ai' -> 'usage' ->> 'cost')::float,
        attributes -> 'gen_ai' -> 'tool' ->> 'name',
        left(attributes -> 'gen_ai' -> 'tool' -> 'call' ->> 'arguments', 300),
        attributes -> 'gen_ai' -> 'input' ->> 'preview',
        attributes -> 'gen_ai' -> 'response' -> 'finish_reasons',
        attributes___user___id, |]
    <> bool [HI.sql|CASE WHEN false THEN attributes END|] [HI.sql|attributes|] bodies
    <> [HI.sql|
      FROM otel_logs_and_spans
      WHERE project_id = #{pid.toText} AND timestamp BETWEEN #{window.fromTime} AND #{window.toTime}
        AND attributes -> 'gen_ai' -> 'operation' ->> 'name' IS NOT NULL |]
    <> scope
    <> [HI.sql| ORDER BY timestamp DESC LIMIT 5000|]


data View = ConversationsView | CallsView
  deriving stock (Bounded, Enum, Eq)


viewParam :: View -> Text
viewParam = \case
  ConversationsView -> "conversations"
  CallsView -> "calls"


data LLMBody = ConversationsBody (Table Conversation) | CallsBody (Table Call)


instance ToHtml LLMBody where
  toHtml = \case
    ConversationsBody t -> toHtml t
    CallsBody t -> toHtml t
  toHtmlRaw = toHtml


newtype LLMGet = LLMPage (PageCtx (Deferred LLMBody))
  deriving (ToHtml) via (PageCtx (RefreshingDeferred LLMBody))


llmGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LLMGet)
llmGetH pid viewM agentM modelM signalM fromP toP sinceP deferredM = do
  (_, _, bw) <- mkPageCtx pid
  window <- mkWindow fromP toP sinceP
  let view = fromMaybe ConversationsView $ find ((== viewM) . Just . viewParam) universe
      link v a m s = windowed window ((urls.traces pid).llmGetH (Just $ viewParam v) a m s)
      url = link view agentM modelM signalM (Just "1")
      matches c = all (\(want, got) -> maybe True (`elem` got) want) ([(agentM, maybeToList c.agent), (modelM, maybeToList c.model)] :: [(Maybe Text, [Text])])
  body <- withDeferredBody deferredM "llmContainer" url (tableSkeleton_ 8) do
    calls <- V.fromList <$> selectCalls pid window False [HI.sql||]
    let kpis = kpis_ calls
        reset = link view Nothing Nothing Nothing Nothing
        zero = ZeroState{icon = "sparkles", title = "No LLM or agent telemetry yet", description = "Send OpenTelemetry GenAI spans from any SDK or framework — OpenLLMetry, OpenInference, Vercel AI SDK, Pydantic AI, LangChain, the OpenAI Agents SDK, Claude Code — and conversations, cost and tool calls appear here.", action = ESLink "https://monoscopehq.com/docs/sdks/llm-observability/" "Instrument your agent"}
        base :: Text -> Table a
        base name = (inventoryTable name url){features = (inventoryTable name url).features{header = Just kpis, zeroState = Just zero, searchPlaceholder = Just $ case view of ConversationsView -> "Search conversations"; CallsView -> "Search model calls"}}
    pure $ case view of
      ConversationsView ->
        let all' = V.fromList $ groupConversations calls
            shown = V.filter (\c -> maybe True (`elem` c.agents) agentM && maybe True (`elem` c.models) modelM && maybe True (`elem` map signalLabel c.signals) signalM) all'
            t = base "llm"
         in ConversationsBody
              t
                { columns = conversationColumns
                , rows = shown
                , features =
                    t.features
                      { rowLink = Just \c -> windowed window ((urls.traces pid).llmConversationGetH c.key)
                      , resultSummary = Just $ show (V.length shown) <> " of " <> show (V.length all') <> " conversations"
                      , facets =
                          Just
                            $ facetActions
                              reset
                              "llmContainer"
                              [ countedFilter "Agent" "agent" agentM (.agents) all'
                              , countedFilter "Model" "model" modelM (.models) all'
                              , countedFilter "Signal" "signal" signalM (map signalLabel . (.signals)) all'
                              ]
                      }
                }
      CallsView ->
        -- Frameworks name the agent on its root span only; each call takes its trace's.
        let agents = M.fromList [(c.traceId, a) | c <- V.toList calls, Just a <- [c.agent]]
            all' = V.map (\c -> c{agent = c.agent <|> M.lookup c.traceId agents}) $ V.filter inference calls
            shown = V.filter matches all'
            t = base "llm"
         in CallsBody
              t
                { columns = callColumns
                , rows = shown
                , features =
                    t.features
                      { rowAttrs = Just \c -> maybe [] (\u -> drawerRowAttrs_ $ (urls.logExplorer pid).logExplorerItemDetailedGet u c.timestamp Nothing (Just "tab-genai") Nothing True) (UUID.fromText c.id)
                      , resultSummary = Just $ show (V.length shown) <> " of " <> show (V.length all') <> " model calls" <> bool "" " (newest 5,000 spans)" (V.length calls >= 5000)
                      , facets = Just $ facetActions reset "llmContainer" [countedFilter "Agent" "agent" agentM (maybeToList . (.agent)) all', countedFilter "Model" "model" modelM (maybeToList . (.model)) all']
                      }
                }
  addRespHeaders
    $ LLMPage
    $ PageCtx
      bw
        { pageTitle = "LLM & Agents"
        , menuItem = Just "LLM & Agents"
        , navTabs = Just $ navTabStrip_ NavLinks "LLM views" "" [(toHtml (viewLabel v), link v Nothing Nothing Nothing Nothing, v == view) | v <- universe]
        , pageActions = Just $ div_ [class_ "inline-flex items-center gap-2", data_ "default-window" "24H"] do
            a_ [class_ "btn btn-sm", href_ $ urls.llmDashboardRedirect pid []] $ faSprite_ "dashboard" "regular" "h-3.5 w-3.5" >> "Dashboard"
            TimePicker.liveDataControls_ Nothing window.currentRange Nothing TimePicker.RefreshOnly
        , needsGridStack = True
        }
      body
  where
    viewLabel = \case
      ConversationsView -> "Conversations" :: Text
      CallsView -> "LLM calls"

    -- \| Headline numbers over model calls only: agent spans carry aggregate usage that would count twice.
    kpis_ :: V.Vector Call -> Html ()
    kpis_ calls = div_ [class_ "flex flex-col gap-3 px-4 pt-3"] do
      div_ [class_ "grid grid-cols-6 gap-px overflow-hidden rounded-lg border border-strokeWeak bg-strokeWeak max-xl:grid-cols-3 max-sm:grid-cols-2"] do
        stat "Conversations" (show $ length $ groupConversations calls) Nothing
        stat "Model calls" (prettyPrintCount n) Nothing
        stat "Tokens" (prettyPrintCount $ round $ sum (mapMaybe (.inputTokens) chats) + sum (mapMaybe (.outputTokens) chats)) Nothing
        stat "Cost" (maybe "—" (GenAI.formatUsd . sum) $ nonEmpty $ mapMaybe (.cost) chats) $ (\k -> show k <> " unpriced") <$> mfilter (> 0) (Just $ length $ filter (isNothing . (.cost)) chats)
        stat "Error rate" (if n == 0 then "—" else show (round (100 * fromIntegral (length $ filter (.failed) chats) / fromIntegral n :: Double) :: Int) <> "%") Nothing
        stat "p95 latency" (maybe "—" (toText . getDurationNSMS . fromIntegral . p95) $ nonEmpty $ map (.duration) chats) Nothing
      div_ [class_ "grid grid-cols-2 gap-3 max-lg:grid-cols-1"] $ forM_ charts $ div_ [class_ "min-h-56"] . Widget.widget_
      where
        chats = filter inference $ V.toList calls
        n = length chats
        p95 ne = fromMaybe (last ne) $ Relude.sort (toList ne) !!? (length ne * 95 `div` 100)
        stat :: Text -> Text -> Maybe Text -> Html ()
        stat label value note = div_ [class_ "flex flex-col gap-0.5 bg-bgRaised px-3 py-2"] do
          span_ [class_ "text-xs text-textWeak"] $ toHtml label
          div_ [class_ "flex items-baseline gap-1.5"] do
            span_ [class_ "text-lg font-semibold tabular-nums text-textStrong"] $ toHtml value
            whenJust note $ span_ [class_ "text-xs text-textWeak", term "data-tippy-content" "Calls to models without a published price carry tokens but no cost."] . toHtml
        inferenceKql = "attributes.gen_ai.operation.name in (\"chat\", \"text_completion\", \"generate_content\", \"embeddings\")"
        charts =
          [ Widget.infraTimeseries pid "llm-cost" "Cost by model" "$" $ inferenceKql <> " | summarize sum(attributes.gen_ai.usage.cost) by bin_auto(timestamp), attributes.gen_ai.request.model"
          , Widget.infraTimeseries pid "llm-calls" "Model calls by model" "" $ inferenceKql <> " | summarize count(*) by bin_auto(timestamp), attributes.gen_ai.request.model"
          ]

    conversationColumns :: [Column Conversation]
    conversationColumns =
      [ col "Conversation" titleCell & withAttrs [class_ "min-w-72"]
      , col "Agent" (chips . (.agents)) & withAttrs [class_ "w-36 max-lg:hidden"]
      , col "Models" (chips . (.models)) & withAttrs [class_ "w-40 max-xl:hidden"]
      , col "Turns" (num . fromIntegral . (.turns)) & withAttrs [class_ "w-16 text-right"]
      , col "Calls" (\c -> span_ [class_ "whitespace-nowrap tabular-nums text-textWeak"] $ toHtml $ show c.llmCalls <> " model · " <> show c.toolCalls <> " tool") & withAttrs [class_ "w-32 text-right max-md:hidden"]
      , col "Tokens" (num . (.tokens)) & withAttrs [class_ "w-20 text-right"]
      , col "Cost" (\c -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ maybe "—" GenAI.formatUsd c.cost) & withAttrs [class_ "w-20 text-right"]
      , col "Signals" signalsCell & withAttrs [class_ "w-44"]
      , col "Last seen" (localTimeFmt_ "MMM dd, HH:mm" . (.lastSeen)) & withAttrs [class_ "w-28 max-md:hidden"]
      ]
      where
        titleCell, signalsCell :: Conversation -> Html ()
        titleCell c = div_ [class_ "flex min-w-0 flex-col"] do
          span_ [class_ "truncate font-medium text-textStrong"] $ toHtml $ fromMaybe "Untitled conversation" c.title
          span_ [class_ "truncate text-xs text-textWeak"] $ toHtml $ T.intercalate " · " $ catMaybes [Just $ T.take 12 c.key, viaNonEmpty head c.users, viaNonEmpty head c.services, Just $ toText (getDurationNSMS $ fromIntegral c.activeNs)]
        signalsCell c = div_ [class_ "flex flex-wrap gap-1"] do
          when (c.errors > 0 && null c.signals) $ tonePill_ ErrorBadge $ show c.errors <> " errors"
          forM_ c.signals \s -> span_ [term "data-tippy-content" $ signalHelp s] $ tonePill_ (signalTone s) $ signalLabel s

    callColumns :: [Column Call]
    callColumns =
      [ col "Call" (\c -> div_ [class_ "flex min-w-0 flex-col"] $ span_ [class_ "truncate font-medium text-textStrong"] (toHtml c.name) >> span_ [class_ "truncate text-xs text-textWeak"] (toHtml $ fromMaybe "" c.preview)) & withAttrs [class_ "min-w-72"]
      , col "Model" (chips . maybeToList . (.model)) & withAttrs [class_ "w-40"]
      , col "Agent" (chips . maybeToList . (.agent)) & withAttrs [class_ "w-32 max-lg:hidden"]
      , col "Tokens" (\c -> span_ [class_ "tabular-nums text-textWeak"] $ toHtml $ fromMaybe "—" $ (\i o -> prettyPrintCount (round i) <> " → " <> prettyPrintCount (round o)) <$> c.inputTokens <*> c.outputTokens) & withAttrs [class_ "w-28 text-right"]
      , col "Cost" (\c -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ maybe "—" GenAI.formatUsd c.cost) & withAttrs [class_ "w-20 text-right"]
      , col "Latency" (\c -> span_ [class_ "tabular-nums"] $ toHtml $ getDurationNSMS $ fromIntegral c.duration) & withAttrs [class_ "w-20 text-right"]
      , col "Status" (\c -> if c.failed then tonePill_ ErrorBadge "Error" else forM_ (viaNonEmpty head $ abnormal c) (tonePill_ WarningBadge . show)) & withAttrs [class_ "w-24"]
      , col "Time" (localTimeFmt_ "MMM dd, HH:mm:ss" . (.timestamp)) & withAttrs [class_ "w-28 max-md:hidden"]
      ]


signalTone :: Signal -> BadgeColor
signalTone = \case
  (ToolError; ModelError) -> ErrorBadge
  (Truncated; Filtered; Looping; ContextPressure) -> WarningBadge


chips :: [Text] -> Html ()
chips xs = div_ [class_ "flex min-w-0 flex-wrap gap-1"] $ forM_ (take 2 xs) (span_ [class_ "truncate rounded bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak"] . toHtml) >> when (length xs > 2) (span_ [class_ "text-xs text-textWeak"] $ toHtml $ "+" <> show @Text (length xs - 2))


num :: Double -> Html ()
num = span_ [class_ "tabular-nums text-textWeak"] . toHtml . prettyPrintCount . round


-- * Conversation detail


data ConversationDetail = ConversationDetail {pid :: Projects.ProjectId, key :: Text, window :: TimePicker.TimeWindow, summary :: Maybe Conversation, steps :: [Call]}


newtype ConversationGet = ConversationPage (PageCtx ConversationDetail)
  deriving newtype (ToHtml)


conversationGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ConversationGet)
conversationGetH pid key fromP toP sinceP = do
  (_, _, bw) <- mkPageCtx pid
  window <- mkWindow fromP toP sinceP
  steps <-
    reverse
      <$> selectCalls
        pid
        window
        True
        [HI.sql| AND (context___trace_id = #{key} OR attributes -> 'gen_ai' -> 'conversation' ->> 'id' = #{key} OR context___trace_id IN (
            SELECT context___trace_id FROM otel_logs_and_spans
            WHERE project_id = #{pid.toText} AND timestamp BETWEEN #{window.fromTime} AND #{window.toTime} AND context___trace_id <> ''
              AND attributes -> 'gen_ai' -> 'conversation' ->> 'id' = #{key})) |]
  let summary = viaNonEmpty head $ groupConversations $ V.fromList steps
  addRespHeaders
    $ ConversationPage
    $ PageCtx
      bw
        { pageTitle = "LLM & Agents"
        , pageTitleUrl = Just $ windowed window ((urls.traces pid).llmGetH Nothing Nothing Nothing Nothing) Nothing
        , pageTitleSuffix = Just $ maybe key (T.take 60 . fromMaybe "Conversation" . (.title)) summary
        , menuItem = Just "LLM & Agents"
        , pageActions = Just $ TimePicker.liveDataControls_ Nothing window.currentRange Nothing TimePicker.RefreshOnly
        }
      ConversationDetail{pid, key, window, summary, steps}


instance ToHtml ConversationDetail where
  toHtml = toHtmlRaw . conversationDetail_
  toHtmlRaw = toHtml


conversationDetail_ :: ConversationDetail -> Html ()
conversationDetail_ d = div_ [class_ "mx-auto flex w-full max-w-6xl flex-col gap-4 p-4"] $ case d.summary of
  Nothing -> callout_ WarningBadge Nothing "No spans for this conversation in the selected time range. Widen the range from the picker above."
  Just c -> do
    header_ [class_ "flex flex-col gap-2"] do
      div_ [class_ "flex flex-wrap items-start gap-3"] do
        h1_ [class_ "min-w-0 flex-1 text-lg font-semibold text-textStrong text-pretty max-sm:basis-full"] $ toHtml $ fromMaybe "Untitled conversation" c.title
        askAi_ c
        a_ ([class_ "btn btn-sm", href_ explorer] <> navTabAttrs) $ faSprite_ "explore" "regular" "h-3.5 w-3.5" >> "Open spans in Explorer"
      div_ [class_ "flex flex-wrap gap-1.5"] do
        forM_ c.agents $ metaChip_ "Agent"
        forM_ c.models $ metaChip_ "Model"
        forM_ (take 1 c.users) $ metaChip_ "User"
        metaChip_ "Turns" $ show c.turns
        metaChip_ "Calls" $ show c.llmCalls <> " model · " <> show c.toolCalls <> " tool"
        metaChip_ "Tokens" $ prettyPrintCount $ round c.tokens
        metaChip_ "Cost" $ maybe "unpriced" GenAI.formatUsd c.cost
        metaChip_ "Active time" $ toText $ getDurationNSMS $ fromIntegral c.activeNs
    unless (null c.signals) $ div_ [class_ "flex flex-col gap-1.5"] $ forM_ c.signals \s -> callout_ (signalTone s) Nothing $ strong_ (toHtml $ signalLabel s) >> " — " >> toHtml (signalHelp s)
    div_ [class_ "group/ctab flex flex-col gap-3"] do
      div_ [role_ "radiogroup", Aria.label_ "Conversation views", class_ "tabs tabs-box w-max"] do
        label_ [class_ "tab has-[:checked]:tab-active"] $ input_ [type_ "radio", name_ "ctab", class_ "sr-only ctab-transcript", checked_] >> "Transcript"
        label_ [class_ "tab has-[:checked]:tab-active"] $ input_ [type_ "radio", name_ "ctab", class_ "sr-only ctab-timeline"] >> "Timeline"
      section_ [class_ "hidden group-has-[.ctab-transcript:checked]/ctab:block"] transcript_
      section_ [class_ "hidden group-has-[.ctab-timeline:checked]/ctab:block"] timeline_
  where
    explorer = (urls.logExplorer d.pid).logExplorerGet (Just $ "attributes.gen_ai.conversation.id == \"" <> d.key <> "\" or trace_id == \"" <> d.key <> "\"") Nothing d.window.sinceQuery d.window.fromQuery d.window.toQuery (Just "spans") Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    -- \| Hands the conversation to the assistant dock, which reads the page it was asked from.
    askAi_ :: Conversation -> Html ()
    askAi_ c =
      button_
        [ type_ "button"
        , class_ "btn btn-sm btn-primary"
        , hxPost_ $ urls.aiDockPost d.pid
        , hxVals_ $ "js:{path: location.pathname + location.search, title: document.title, query: " <> decodeUtf8 (AE.encode prompt) <> "}"
        , hxTarget_ "#ai-dock"
        , hxSwap_ "outerMorph"
        , [__|on htmx:after:request set #ai-dock-open.checked to true|]
        ]
        $ faSprite_ "sparkles" "regular" "h-3.5 w-3.5"
        >> "Ask AI about this conversation"
      where
        prompt = "Analyse the agent conversation on this page (gen_ai.conversation.id or trace id " <> c.key <> "). Summarise what the user wanted, whether the agent achieved it, where it went wrong (tool errors, loops, truncation, wrong tool choice), and what drove the cost and latency. Query its spans if you need more than the page shows." :: Text

    -- \| Each message once: an LLM call's input repeats the history before it, so only messages
    -- not already shown are rendered, then the call's reply with its model, latency and cost.
    transcript_ :: Html ()
    transcript_ = div_ [class_ "flex flex-col gap-3"] do
      unless (any (\s -> isJust (messages "input" s) || isJust (messages "output" s)) source) $ callout_ NeutralBadge Nothing "These spans carry no message content. Enable content capture in your instrumentation (OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT=SPAN_ONLY for OpenTelemetry GenAI) to see the transcript."
      sequence_ rendered
      where
        calls = filter inference d.steps
        source = if any (isJust . messages "input") calls then calls else filter ((== "invoke_agent") . (.operation)) d.steps
        at c = genAIAt (unAesonTextMaybe c.attributes)
        messages dir c = at c [dir, "messages"] >>= \case AE.Array ms -> Just (toList ms); _ -> Nothing
        tools = M.fromList [(i, c) | c <- d.steps, Just (AE.String i) <- [at c ["tool", "call", "id"]]]
        rendered = go S.empty source
        go _ [] = []
        go seen (s : rest) =
          let fresh = filter (\m -> not (S.member (AE.encode m) seen)) (fromMaybe [] $ messages "input" s)
              out = fromMaybe [] $ messages "output" s
              seen' = foldr (S.insert . AE.encode) seen (fresh <> out)
           in (forM_ fresh (message_ d.pid tools Nothing) >> forM_ out (message_ d.pid tools (Just s))) : go seen' rest

    -- \| AI spans in execution order, with a bar on the conversation's own clock and the running cost.
    timeline_ :: Html ()
    timeline_ = div_ [class_ "overflow-hidden rounded-lg border border-strokeWeak"] $ table_ [class_ "w-full text-sm"] do
      thead_ [class_ "bg-fillWeaker text-xs text-textWeak"] $ tr_ do
        th_ [class_ "px-3 py-1.5 text-left font-medium"] "Step"
        th_ [class_ "w-1/3 px-3 py-1.5 text-left font-medium max-md:hidden"] "When"
        th_ [class_ "px-3 py-1.5 text-right font-medium"] "Duration"
        th_ [class_ "px-3 py-1.5 text-right font-medium"] "Tokens"
        th_ [class_ "px-3 py-1.5 text-right font-medium"] "Cost · total"
      tbody_ $ forM_ (zip d.steps running) \(c, total) -> do
        let s = c
            offset = pct (diffUTCTime s.timestamp start)
            width = max 0.5 $ pct (fromIntegral s.duration / 1e9)
        tr_ ([class_ "border-t border-strokeWeak hover:bg-fillHover"] <> maybe [] (\u -> drawerRowAttrs_ $ (urls.logExplorer d.pid).logExplorerItemDetailedGet u s.timestamp Nothing (Just "tab-genai") Nothing True) (UUID.fromText s.id)) do
          td_ [class_ "px-3 py-1.5"] $ div_ [class_ "flex min-w-0 items-center gap-2"] do
            faSprite_ (opIcon c.operation) "regular" "h-3 w-3 shrink-0 text-iconNeutral"
            span_ [class_ "truncate text-textStrong"] $ toHtml s.name
            when s.failed $ tonePill_ ErrorBadge "Error"
          td_ [class_ "px-3 py-1.5 max-md:hidden"] $ div_ [class_ "relative h-2 rounded-sm bg-fillWeak"] $ div_ [class_ $ "absolute inset-y-0 rounded-sm " <> if s.failed then "bg-fillError-strong" else "bg-fillBrand-strong", style_ $ "left:" <> show offset <> "%;width:" <> show (min width (100 - offset)) <> "%"] ""
          td_ [class_ "px-3 py-1.5 text-right tabular-nums text-textWeak"] $ toHtml $ getDurationNSMS $ fromIntegral s.duration
          td_ [class_ "px-3 py-1.5 text-right tabular-nums text-textWeak"] $ toHtml $ maybe "" (\i -> prettyPrintCount (round i) <> maybe "" ((" → " <>) . prettyPrintCount . round) c.outputTokens) c.inputTokens
          td_ [class_ "px-3 py-1.5 text-right tabular-nums text-textWeak"] $ toHtml $ maybe "" (\x -> GenAI.formatUsd x <> " · " <> GenAI.formatUsd total) c.cost
      where
        start = maybe d.window.fromTime (.timestamp) (viaNonEmpty head d.steps)
        end = maybe start (F1.maximum . fmap (\s -> addUTCTime (fromIntegral s.duration / 1e9) s.timestamp)) (nonEmpty d.steps)
        span' = max 0.001 $ diffUTCTime end start
        pct x = realToFrac (100 * x / span') :: Double
        running = drop 1 $ scanl (+) 0 $ map (fromMaybe 0 . (.cost)) d.steps
        opIcon = \case
          "execute_tool" -> "function"
          "invoke_agent" -> "sparkles"
          _ -> "comment"
