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
  Unit (..),
  Topic (..),
  conversationsOf,
  topicsOf,
) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Foldable1 qualified as F1
import Data.HashSet qualified as HS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Ids qualified as Projects
import Models.Projects.DashboardTemplates qualified as DashboardTemplates
import Models.Projects.Dashboards qualified as Dashboards
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), llmCallsUrl, mkPageCtx, navTabAttrs)
import Pages.Components (BadgeColor (..), Deferred (..), EmptyStateAction (..), RefreshingDeferred (..), callout_, localTimeFmt_, metaChip_, tableSkeleton_, tonePill_, withDeferredBody)
import Pages.Containers (mkWindow, windowed)
import Pages.GenAIMessages (Call (..), genAIAt, message_)
import Pkg.Components.Table (Column, Features (..), Table (..), ZeroState (..), col, countedFilter, facetActions, inventoryTable, withAttrs)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (unAesonTextMaybe)
import Pkg.Drain qualified as Drain
import Pkg.GenAI qualified as GenAI
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, useTfReads)
import Utils (TabStrip (..), drawerRowAttrs_, faSprite_, getDurationNSMS, navTabStrip_, prettyPrintCount, tokenFlow)
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
  , llmCalls :: Int64
  , toolCalls :: Int64
  , tokens :: Double
  , cost :: Maybe Double
  , errors :: Int64
  , activeNs :: Int64
  , signals :: [Signal]
  }
  deriving stock (Generic, Show)


-- | One trace's GenAI spans aggregated in the store; a trace-less record (a log) is its own unit.
data Unit = Unit
  { unit :: Text
  , conversation :: Maybe Text
  , agent :: Maybe Text
  , model :: Maybe Text
  , user :: Maybe Text
  , service :: Maybe Text
  , started :: UTCTime
  , lastSeen :: UTCTime
  , endUs :: Int64
  , llmCalls :: Int64
  , toolCalls :: Int64
  , agentRuns :: Int64
  , tokens :: Double
  , cost :: Maybe Double
  , errors :: Int64
  , toolErrors :: Int64
  , modelErrors :: Int64
  , truncated :: Int64
  , filtered :: Int64
  , repeats :: Int64
  , maxInput :: Maybe Double
  , firstPreview :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | A trace joins the conversation any of its spans names (frameworks often set the id on the root
-- agent span only). A trace with no id is its own conversation if it ran an agent; otherwise it is
-- only model calls, so single classifier calls don't drown real conversations.
--
-- >>> import Relude.Unsafe qualified as Unsafe
-- >>> let t = Unsafe.read @UTCTime "2026-01-01 00:00:00 UTC"
-- >>> let u name conv runs = Unit name conv Nothing (Just "gpt-4o") Nothing Nothing t t 0 1 0 runs 15 (Just 0.01) 0 0 0 0 0 0 Nothing (Just "1767225600|hi")
-- >>> map (\c -> (c.key, c.turns, c.llmCalls, c.title)) $ conversationsOf [u "t1" (Just "conv") 1, u "t2" (Just "conv") 0, u "t3" Nothing 0, u "t4" Nothing 1]
-- [("conv",2,2,Just "hi"),("t4",1,1,Just "hi")]
conversationsOf :: [Unit] -> [Conversation]
conversationsOf units = sortOn (Down . (.lastSeen)) $ mapMaybe conversation $ M.toList grouped
  where
    grouped = M.fromListWith (<>) [(k, [u]) | u <- units, Just k <- [u.conversation <|> (u.unit <$ guard (u.agentRuns > 0))]]
    conversation (k, us) = do
      ne <- nonEmpty us
      let total :: Num n => (Unit -> n) -> n
          total f = sum $ map f us
          distinct f = ordNub $ mapMaybe f us
          end u = posixSecondsToUTCTime (fromIntegral u.endUs / 1e6)
      pure
        Conversation
          { key = k
          , title = T.drop 1 . snd . T.breakOn "|" <$> viaNonEmpty F1.minimum (mapMaybe (.firstPreview) us)
          , started = F1.minimum $ fmap (.started) ne
          , lastSeen = F1.maximum $ fmap end ne
          , agents = distinct (.agent)
          , models = distinct (.model)
          , services = distinct (.service)
          , users = distinct (.user)
          , turns = length us
          , llmCalls = total (.llmCalls)
          , toolCalls = total (.toolCalls)
          , tokens = total (.tokens)
          , cost = viaNonEmpty sum (mapMaybe (.cost) us)
          , errors = total (.errors)
          , activeNs = total \u -> u.endUs * 1000 - round (utcTimeToPOSIXSeconds u.started * 1e9)
          , -- Deterministic rules, in 'Signal' order, so each finding is explainable and free.
            signals = [sig | (sig, True) <- zip universe [total (.toolErrors) > 0, total (.modelErrors) > 0, total (.truncated) > 0, total (.filtered) > 0, any ((>= 2) . (.repeats)) us, any pressured us]]
          }
    pressured u = fromMaybe False do
      window <- u.model >>= GenAI.lookupPrice >>= (.contextWindow)
      used <- u.maxInput
      pure $ used >= 0.8 * fromIntegral window


-- | The window's GenAI spans aggregated per trace in the store: one bounded scan of the
-- ingest-stamped @kind=ai@ summary element, and every total covers the whole window.
selectUnits :: Projects.ProjectId -> TimePicker.TimeWindow -> HI.Sql -> ATAuthCtx [Unit]
selectUnits pid window scope = do
  tf <- useTfReads
  Hasql.withHasqlTimefusion tf
    $ Hasql.interp
    $ [HI.sql|
      WITH spans AS (
        SELECT CASE WHEN COALESCE(context___trace_id, '') = '' THEN id::text ELSE context___trace_id END AS unit,
          timestamp, COALESCE(duration, 0) AS dur, COALESCE(status_code = 'ERROR', false) AS failed,
          attributes -> 'gen_ai' -> 'operation' ->> 'name' AS op,
          attributes -> 'gen_ai' -> 'conversation' ->> 'id' AS conv,
          attributes -> 'gen_ai' -> 'agent' ->> 'name' AS agent,
          COALESCE(attributes -> 'gen_ai' -> 'response' ->> 'model', attributes -> 'gen_ai' -> 'request' ->> 'model') AS model,
          (attributes -> 'gen_ai' -> 'usage' ->> 'input_tokens')::float8 AS input_tokens,
          (attributes -> 'gen_ai' -> 'usage' ->> 'output_tokens')::float8 AS output_tokens,
          (attributes -> 'gen_ai' -> 'usage' ->> 'cost')::float8 AS cost,
          attributes -> 'gen_ai' -> 'tool' ->> 'name' AS tool,
          left(attributes -> 'gen_ai' -> 'tool' -> 'call' ->> 'arguments', 300) AS tool_args,
          attributes -> 'gen_ai' -> 'input' ->> 'preview' AS preview,
          summary AS tags, attributes___user___id AS usr, resource___service___name AS svc
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText} AND timestamp BETWEEN #{window.fromTime} AND #{window.toTime}
          AND #{Telemetry.genAIMarker} = ANY(summary) |]
    <> scope
    <> [HI.sql|
      )
      SELECT unit, max(conv) AS conv, max(agent) AS agent, max(model) AS model, max(usr) AS usr, max(svc) AS svc,
        min(timestamp) AS started, max(timestamp) AS last_ts, max(CAST(EXTRACT(EPOCH FROM timestamp) * 1000000 AS BIGINT) + dur / 1000) AS end_us,
        sum(CASE WHEN op IN ('chat', 'text_completion', 'generate_content', 'embeddings') THEN 1 ELSE 0 END) AS llm_calls,
        sum(CASE WHEN op = 'execute_tool' THEN 1 ELSE 0 END) AS tool_calls,
        sum(CASE WHEN op IN ('invoke_agent', 'invoke_workflow') THEN 1 ELSE 0 END) AS agent_runs,
        sum(CASE WHEN op IN ('chat', 'text_completion', 'generate_content', 'embeddings') THEN COALESCE(input_tokens, 0) + COALESCE(output_tokens, 0) ELSE 0 END) AS tokens,
        sum(CASE WHEN op IN ('chat', 'text_completion', 'generate_content', 'embeddings') THEN cost END) AS cost_sum,
        sum(CASE WHEN failed THEN 1 ELSE 0 END) AS errors,
        sum(CASE WHEN failed AND op = 'execute_tool' THEN 1 ELSE 0 END) AS tool_errors,
        sum(CASE WHEN failed AND op IN ('chat', 'text_completion', 'generate_content', 'embeddings') THEN 1 ELSE 0 END) AS model_errors,
        sum(CASE WHEN #{Telemetry.genAIFinishMarker GenAI.Truncated} = ANY(tags) THEN 1 ELSE 0 END) AS truncated,
        sum(CASE WHEN #{Telemetry.genAIFinishMarker GenAI.Filtered} = ANY(tags) THEN 1 ELSE 0 END) AS filtered,
        sum(CASE WHEN op = 'execute_tool' THEN 1 ELSE 0 END) - count(DISTINCT CASE WHEN op = 'execute_tool' THEN COALESCE(tool, '') || COALESCE(tool_args, '') END) AS repeats,
        max(input_tokens) AS max_input,
        min(CASE WHEN preview IS NOT NULL THEN CAST(CAST(EXTRACT(EPOCH FROM timestamp) AS BIGINT) AS TEXT) || '|' || preview END) AS first_preview
      FROM spans GROUP BY unit ORDER BY last_ts DESC LIMIT #{unitCap}
    |]


unitCap :: Int64
unitCap = 10000


-- | A conversation's spans with their attributes, oldest first, for the transcript and timeline.
selectCalls :: Projects.ProjectId -> TimePicker.TimeWindow -> HI.Sql -> ATAuthCtx [Call]
selectCalls pid window scope = do
  tf <- useTfReads
  Hasql.withHasqlTimefusion tf
    $ Hasql.interp
    $ [HI.sql|
      SELECT id::text, COALESCE(context___trace_id, ''), timestamp, COALESCE(duration, 0), COALESCE(status_code = 'ERROR', false), COALESCE(name, '') AS span_name, resource___service___name,
        attributes -> 'gen_ai' -> 'operation' ->> 'name' AS operation,
        attributes -> 'gen_ai' -> 'conversation' ->> 'id' AS conversation,
        attributes -> 'gen_ai' -> 'agent' ->> 'name' AS agent,
        COALESCE(attributes -> 'gen_ai' -> 'response' ->> 'model', attributes -> 'gen_ai' -> 'request' ->> 'model') AS model,
        (attributes -> 'gen_ai' -> 'usage' ->> 'input_tokens')::float8 AS input_tokens,
        (attributes -> 'gen_ai' -> 'usage' ->> 'output_tokens')::float8 AS output_tokens,
        (attributes -> 'gen_ai' -> 'usage' ->> 'cost')::float8 AS cost,
        attributes -> 'gen_ai' -> 'tool' ->> 'name' AS tool,
        left(attributes -> 'gen_ai' -> 'tool' -> 'call' ->> 'arguments', 300) AS tool_args,
        attributes -> 'gen_ai' -> 'input' ->> 'preview' AS preview,
        attributes -> 'gen_ai' -> 'response' -> 'finish_reasons' AS finish,
        attributes___user___id, attributes
      FROM otel_logs_and_spans
      WHERE project_id = #{pid.toText} AND timestamp BETWEEN #{window.fromTime} AND #{window.toTime}
        AND #{Telemetry.genAIMarker} = ANY(summary) |]
    <> scope
    <> [HI.sql| ORDER BY timestamp ASC LIMIT 2000|]


-- | Conversations span hours, so these pages open on a day rather than the app-wide five minutes.
llmWindow :: Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx TimePicker.TimeWindow
llmWindow fromP toP sinceP = mkWindow fromP toP (sinceP <|> ("24H" <$ guard (all isNothing [fromP, toP])))


-- | Recurring openings: Drain over the conversations' first user messages, the clustering log patterns use.
data Topic = Topic {template :: Text, conversations :: [Conversation]}


-- | Each conversation joins the first final template its tokens match.
--
-- >>> import Relude.Unsafe qualified as Unsafe
-- >>> let t = Unsafe.read @UTCTime "2026-01-01 00:00:00 UTC"
-- >>> let c k title = Conversation k (Just title) t t [] [] [] [] 1 1 0 10 Nothing 0 0 []
-- >>> map (\x -> (x.template, map (.key) x.conversations)) $ topicsOf t [c "a" "refund order 1234 failed", c "b" "refund order 98 failed", c "c" "why is checkout slow"]
-- [("refund order <*> failed",["a","b"]),("why is checkout slow",["c"])]
topicsOf :: UTCTime -> [Conversation] -> [Topic]
topicsOf now convs = sortOn (Down . length . (.conversations)) [Topic tmpl members | tmpl <- ordNub (map (.templateStr) results), let members = [c | (c, Just t) <- assigned, t == tmpl], not (null members)]
  where
    titled = [(c, Drain.generateDrainTokens t) | c <- convs, Just t <- [c.title]]
    results = V.toList $ Drain.getAllLogGroups $ Drain.buildDrainTree snd ((.key) . fst) (const Nothing) Drain.emptyDrainTree (V.fromList titled) now
    matching toks r = V.length r.templateTokens == V.length toks && V.and (V.zipWith (\p x -> p == x || Drain.isPlaceholderToken p) r.templateTokens toks)
    assigned = [(c, (.templateStr) <$> find (matching toks) results) | (c, toks) <- titled]


data View = ConversationsView | TopicsView
  deriving stock (Bounded, Enum, Eq)


viewParam :: View -> Text
viewParam = \case
  ConversationsView -> "conversations"
  TopicsView -> "topics"


data LLMBody = ConversationsBody (Table Conversation) | TopicsBody (Table Topic)


instance ToHtml LLMBody where
  toHtml = \case
    ConversationsBody t -> toHtml t
    TopicsBody t -> toHtml t
  toHtmlRaw = toHtml


newtype LLMGet = LLMPage (PageCtx (Deferred LLMBody))
  deriving (ToHtml) via (PageCtx (RefreshingDeferred LLMBody))


-- | The LLM dashboard template's opening widgets: one definition serves this page and the dashboard.
overviewWidgets :: Projects.ProjectId -> [Widget.Widget]
overviewWidgets pid =
  [ w{Widget._projectId = Just pid, Widget.standalone = Just True}
  | d <- take 1 [d | d <- DashboardTemplates.dashboardTemplatesCompiled, d.file == Just "llm-observability.yaml"]
  , w <- concatMap (\g -> fromMaybe [g] g.children) (take 3 d.widgets)
  ]


llmGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LLMGet)
llmGetH pid viewM agentM modelM signalM topicM fromP toP sinceP deferredM = do
  (_, _, bw) <- mkPageCtx pid
  window <- llmWindow fromP toP sinceP
  now <- Time.currentTime
  let view = fromMaybe ConversationsView $ find ((== viewM) . Just . viewParam) universe
      link v a m s t = windowed window ((urls.traces pid).llmGetH (Just $ viewParam v) a m s t)
      url = link view agentM modelM signalM topicM (Just "1")
      want m xs = maybe True (`elem` xs) m
  body <- withDeferredBody deferredM "llmContainer" url (tableSkeleton_ 8) do
    units <- selectUnits pid window [HI.sql||]
    let conversations = conversationsOf units
        topics = topicsOf now conversations
        capped = bool "" (" (newest " <> show unitCap <> " traces)") (length units >= fromIntegral unitCap)
        reset = link view Nothing Nothing Nothing Nothing Nothing
        zero = ZeroState{icon = "sparkles", title = "No LLM or agent telemetry yet", description = "Send OpenTelemetry GenAI spans from any SDK or framework — OpenLLMetry, OpenInference, Vercel AI SDK, Pydantic AI, LangChain, the OpenAI Agents SDK, Claude Code — and conversations, cost and tool calls appear here.", action = ESLink "https://monoscopehq.com/docs/sdks/llm-observability/" "Instrument your agent"}
        table, base :: Table a
        table = inventoryTable "llm" url
        base = table{features = table.features{header = Just overview, zeroState = Just zero, searchPlaceholder = Just $ case view of ConversationsView -> "Search conversations"; TopicsView -> "Search topics"}}
        inTopic c = maybe True (\t -> any (\tp -> tp.template == t && any ((== c.key) . (.key)) tp.conversations) topics) topicM
    pure $ case view of
      ConversationsView ->
        let all' = V.fromList conversations
            shown = V.filter (\c -> want agentM c.agents && want modelM c.models && want signalM (map signalLabel c.signals) && inTopic c) all'
         in ConversationsBody
              base
                { columns = conversationColumns
                , rows = shown
                , features =
                    base.features
                      { rowLink = Just \c -> windowed window ((urls.traces pid).llmConversationGetH c.key)
                      , resultSummary = Just $ show (V.length shown) <> " of " <> show (V.length all') <> " conversations" <> capped
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
      TopicsView ->
        TopicsBody
          base
            { columns = topicColumns
            , rows = V.fromList topics
            , features = base.features{rowLink = Just \t -> link ConversationsView Nothing Nothing Nothing (Just t.template) Nothing, resultSummary = Just $ show (length topics) <> " topics across " <> show (length conversations) <> " conversations" <> capped}
            }
  addRespHeaders
    $ LLMPage
    $ PageCtx
      bw
        { pageTitle = "LLM & Agents"
        , menuItem = Just "LLM & Agents"
        , navTabs = Just $ navTabStrip_ NavLinks "LLM views" "" $ [(toHtml (viewLabel v), link v Nothing Nothing Nothing Nothing Nothing, v == view) | v <- universe] <> [("LLM calls", llmCallsUrl pid window.sinceQuery window.fromQuery window.toQuery, False)]
        , pageActions = Just $ div_ [class_ "inline-flex items-center gap-2", data_ "default-window" "24H"] do
            a_ [class_ "btn btn-sm", href_ $ urls.llmDashboardRedirect pid []] $ faSprite_ "dashboard" "regular" "h-3.5 w-3.5" >> "Dashboard"
            TimePicker.liveDataControls_ Nothing window.currentRange Nothing TimePicker.RefreshOnly
        , needsGridStack = True
        }
      body
  where
    viewLabel = \case
      ConversationsView -> "Conversations" :: Text
      TopicsView -> "Topics"

    overview :: Html ()
    overview = div_ [class_ "grid grid-cols-4 gap-3 px-4 pt-3 max-lg:grid-cols-2"] $ forM_ (overviewWidgets pid) \w ->
      div_ [class_ $ if w.wType `elem` [Widget.WTStat, Widget.WTTimeseriesStat] then "h-28" else "col-span-2 min-h-56"] $ Widget.widget_ w

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

    topicColumns :: [Column Topic]
    topicColumns =
      [ col "Topic" (\t -> span_ [class_ "font-mono text-sm text-textStrong"] $ toHtml t.template) & withAttrs [class_ "min-w-72"]
      , col "Conversations" (num . fromIntegral . length . (.conversations)) & withAttrs [class_ "w-28 text-right"]
      , col "Tokens" (num . sum . map (.tokens) . (.conversations)) & withAttrs [class_ "w-20 text-right"]
      , col "Cost" (\t -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ maybe "—" GenAI.formatUsd $ viaNonEmpty sum $ mapMaybe (.cost) t.conversations) & withAttrs [class_ "w-20 text-right"]
      , col "With signals" (\t -> num $ fromIntegral $ length $ filter (not . null . (.signals)) t.conversations) & withAttrs [class_ "w-28 text-right"]
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
  window <- llmWindow fromP toP sinceP
  let scope =
        [HI.sql| AND (context___trace_id = #{key} OR attributes -> 'gen_ai' -> 'conversation' ->> 'id' = #{key} OR context___trace_id IN (
            SELECT context___trace_id FROM otel_logs_and_spans
            WHERE project_id = #{pid.toText} AND timestamp BETWEEN #{window.fromTime} AND #{window.toTime} AND context___trace_id <> ''
              AND #{Telemetry.genAIMarker} = ANY(summary) AND attributes -> 'gen_ai' -> 'conversation' ->> 'id' = #{key})) |]
  summary <- viaNonEmpty head . conversationsOf <$> selectUnits pid window scope
  steps <- selectCalls pid window scope
  addRespHeaders
    $ ConversationPage
    $ PageCtx
      bw
        { pageTitle = "LLM & Agents"
        , pageTitleUrl = Just $ windowed window ((urls.traces pid).llmGetH Nothing Nothing Nothing Nothing Nothing) Nothing
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
        calls = filter (GenAI.isInferenceOp . (.operation)) d.steps
        source = if any (isJust . messages "input") calls then calls else filter ((== "invoke_agent") . (.operation)) d.steps
        at c = genAIAt (unAesonTextMaybe c.attributes)
        messages dir c = at c [dir, "messages"] >>= \case AE.Array ms -> Just (toList ms); _ -> Nothing
        tools = M.fromList [(i, c) | c <- d.steps, Just (AE.String i) <- [at c ["tool", "call", "id"]]]
        rendered = go HS.empty source
        go _ [] = []
        go seen (s : rest) =
          let fresh = filter (not . (`HS.member` seen)) (fromMaybe [] $ messages "input" s)
              out = fromMaybe [] $ messages "output" s
              seen' = foldr HS.insert seen (fresh <> out)
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
        let offset = pct (diffUTCTime c.timestamp start)
            width = max 0.5 $ pct (fromIntegral c.duration / 1e9)
        tr_ ([class_ "border-t border-strokeWeak hover:bg-fillHover"] <> maybe [] (\u -> drawerRowAttrs_ $ (urls.logExplorer d.pid).logExplorerItemDetailedGet u c.timestamp Nothing (Just "tab-genai") Nothing False) (UUID.fromText c.id)) do
          td_ [class_ "px-3 py-1.5"] $ div_ [class_ "flex min-w-0 items-center gap-2"] do
            faSprite_ (opIcon c.operation) "regular" "h-3 w-3 shrink-0 text-iconNeutral"
            span_ [class_ "truncate text-textStrong"] $ toHtml c.name
            when c.failed $ tonePill_ ErrorBadge "Error"
          td_ [class_ "px-3 py-1.5 max-md:hidden"] $ div_ [class_ "relative h-2 rounded-sm bg-fillWeak"] $ div_ [class_ $ "absolute inset-y-0 rounded-sm " <> if c.failed then "bg-fillError-strong" else "bg-fillBrand-strong", style_ $ "left:" <> show offset <> "%;width:" <> show (min width (100 - offset)) <> "%"] ""
          td_ [class_ "px-3 py-1.5 text-right tabular-nums text-textWeak"] $ toHtml $ getDurationNSMS $ fromIntegral c.duration
          td_ [class_ "px-3 py-1.5 text-right tabular-nums text-textWeak"] $ toHtml $ fromMaybe "" $ tokenFlow c.inputTokens c.outputTokens
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
