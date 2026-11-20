{-# LANGUAGE OrPatterns #-}

-- | LLM & agent observability: conversations, model calls, tools, tokens and cost, read
-- from GenAI spans that 'Pkg.GenAI' normalised at ingest (docs/llm-observability.md).
module Pages.LLM (
  llmGetH,
  displayTitle,
  conversationGetH,
  LLMGet (..),
  LLMBody (..),
  ConversationGet (..),
  Conversation (..),
  Signal (..),
  signalLabel,
  Unit (..),
  Topic (..),
  conversationsOf,
  topicsOf,
  evaluateSignals,
  llmSignalPostH,
  llmSignalRunPostH,
  parseVerdicts,
  LLMSignal (..),
  SignalRow (..),
  SignalForm (..),
  SignalQueued (..),
  Flag (..),
  SignalRejection (..),
) where

import BackgroundJobs.Types qualified as Jobs
import Control.Lens ((^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens qualified as AL
import Data.Char (isAlpha)
import Data.Default (def)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM qualified as ELLM
import Data.Foldable1 qualified as F1
import Data.HashSet qualified as HS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Ids qualified as Projects
import Models.Projects.DashboardTemplates qualified as DashboardTemplates
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), llmCallsUrl, mkPageCtx, navTabAttrs)
import Pages.Components (BadgeColor (..), Deferred (..), EmptyStateAction (..), FieldCfg (..), FieldSize (..), RefreshingDeferred (..), callout_, formField_, localTimeFmt_, primaryButton_, tableSkeleton_, tonePill_, withDeferredBody)
import Pages.Containers (mkWindow, windowed)
import Pages.GenAIMessages (Call (..), genAIAt, genAIMessages, messageParts, message_, roleAvatar_)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.Components.Table (Column, Features (..), Table (..), ZeroState (..), col, countedFilter, facetActions, fillCol, inventoryTable, withAttrs)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (DB, UUIDId, unAesonTextMaybe)
import Pkg.Drain qualified as Drain
import Pkg.GenAI qualified as GenAI
import Pkg.Parser.Eval (jsonAsText)
import Relude hiding (Reader, ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ActionResult, Done, Outcome (..), Refusal (..), RespHeaders, act, addRespHeaders, done, useTfReads)
import Utils (TabStrip (..), countNoun, drawerRowAttrs_, faSprite_, getDurationNSMS, navTabStrip_, nonEmptyT, prettyPrintCount, replaceAllFormats, tokenFlow, truncateText)
import Web.FormUrlEncoded (FromForm)
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


-- | What a conversation is flagged with: a built-in rule, or a plain-language signal's verdict (name, reason).
data Flag = Detected Signal | Judged Text Text
  deriving stock (Eq, Show)


flagLabel :: Flag -> Text
flagLabel = \case
  Detected s -> signalLabel s
  Judged name _ -> name


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
  , signals :: [Flag]
  , units :: [Text]
  , opening :: Maybe Text
  -- ^ The first tool call, named when no prompt was captured (Pi sends only each turn's delta).
  }
  deriving stock (Generic, Show)


-- | One trace's GenAI spans within an hour, aggregated in the store; a trace-less record (a log) is its own unit.
data Unit = Unit
  { hour :: UTCTime
  , unit :: Text
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
  , firstTool :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


-- | One trace's hours as one unit. Repeats are summed per hour, so a tool call repeated across an
-- hour boundary goes uncounted.
instance Semigroup Unit where
  a <> b =
    Unit
      { hour = min a.hour b.hour
      , unit = a.unit
      , conversation = max a.conversation b.conversation
      , agent = max a.agent b.agent
      , model = max a.model b.model
      , user = max a.user b.user
      , service = max a.service b.service
      , started = min a.started b.started
      , lastSeen = max a.lastSeen b.lastSeen
      , endUs = max a.endUs b.endUs
      , llmCalls = a.llmCalls + b.llmCalls
      , toolCalls = a.toolCalls + b.toolCalls
      , agentRuns = a.agentRuns + b.agentRuns
      , tokens = a.tokens + b.tokens
      , cost = both (+) a.cost b.cost
      , errors = a.errors + b.errors
      , toolErrors = a.toolErrors + b.toolErrors
      , modelErrors = a.modelErrors + b.modelErrors
      , truncated = a.truncated + b.truncated
      , filtered = a.filtered + b.filtered
      , repeats = a.repeats + b.repeats
      , maxInput = max a.maxInput b.maxInput
      , firstPreview = both min a.firstPreview b.firstPreview
      , firstTool = both min a.firstTool b.firstTool
      }
    where
      both f x y = liftA2 f x y <|> x <|> y


-- | A conversation with no captured prompt is named after its first tool call, else its agent.
displayTitle :: Conversation -> Text
displayTitle c = fromMaybe (maybe "Untitled conversation" (<> " run") $ viaNonEmpty head c.agents) (c.title <|> c.opening)


-- | A tool call as a title: its name and the first string argument, on one line.
--
-- >>> map toolOpening ["bash\t{\"command\":\"git diff --stat\\nHEAD\"}", "query_telemetry\t{\"limit\":5,\"query\":\"errors | count\"}", "submit_review\t", "ls\t{\"path\":"]
-- ["bash: git diff --stat HEAD","query_telemetry: errors | count","submit_review","ls: {\"path\":"]
toolOpening :: Text -> Text
toolOpening raw =
  let (name, args) = second (T.drop 1) $ T.breakOn "\t" raw
      firstString = \case AE.Object o -> listToMaybe [v | AE.String v <- toList o]; _ -> Nothing
      arg = fromMaybe args $ firstString =<< AE.decodeStrict (encodeUtf8 args)
   in T.take 120 $ name <> bool (": " <> unwords (words arg)) "" (T.null $ T.strip arg)


-- | A trace joins the conversation any of its spans names (frameworks often set the id on the root
-- agent span only). A trace with no id is its own conversation if it ran an agent; otherwise it is
-- only model calls, so single classifier calls don't drown real conversations.
--
-- >>> import Relude.Unsafe qualified as Unsafe
-- >>> let t = Unsafe.read @UTCTime "2026-01-01 00:00:00 UTC"
-- >>> let u name conv runs = Unit t name conv Nothing (Just "gpt-4o") Nothing Nothing t t 0 1 0 runs 15 (Just 0.01) 0 0 0 0 0 0 Nothing (Just "1767225600|hi") Nothing
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
            signals = [Detected sig | (sig, True) <- zip universe [total (.toolErrors) > 0, total (.modelErrors) > 0, total (.truncated) > 0, total (.filtered) > 0, any ((>= 2) . (.repeats)) us, any pressured us]]
          , units = map (.unit) us
          , opening = toolOpening . T.drop 1 . snd . T.breakOn "|" <$> viaNonEmpty F1.minimum (mapMaybe (.firstTool) us)
          }
    pressured u = fromMaybe False do
      window <- u.model >>= GenAI.lookupPrice >>= (.contextWindow)
      used <- u.maxInput
      pure $ used >= 0.8 * fromIntegral window


-- | GenAI spans in @[from, to)@ aggregated per trace and hour in the store. Only the promoted gen_ai
-- columns and three @attributes@ paths are read: the @summary@ array and the rest of the blob cost
-- seconds to decode across a day (all of a project's rows are scanned to find the GenAI ones).
-- Rows ingested before the columns were promoted (2026-11-16) have none and are not listed.
selectUnits :: (DB es, Labeled "timefusion" Hasql :> es, Reader AuthContext :> es) => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es [Unit]
selectUnits pid from to = do
  tf <- useTfReads
  -- OR'd rather than LIKE ANY, which TimeFusion plans only over a literal array.
  let finished a = "(" <> mconcat (intersperse " OR " [[HI.sql|finish LIKE #{"%\"" <> r <> "\"%"}|] | r <- GenAI.abnormalReasons a]) <> ")"
  Hasql.withHasqlTimefusion tf
    $ Hasql.interp
      [HI.sql|
      WITH spans AS (
        SELECT date_trunc('hour', timestamp) AS hour, CASE WHEN COALESCE(context___trace_id, '') = '' THEN id::text ELSE context___trace_id END AS unit,
          timestamp, COALESCE(duration, 0) AS dur, COALESCE(status_code = 'ERROR', false) AS failed,
          attributes___gen_ai___operation___name AS op,
          attributes___gen_ai___operation___name = ANY(#{V.fromList GenAI.inferenceOps}) AS model_op,
          attributes___gen_ai___operation___name = ANY(#{V.fromList GenAI.agentRunOps}) AS agent_op,
          attributes___gen_ai___conversation___id AS conv, attributes___gen_ai___agent___name AS agent, attributes___gen_ai___request___model AS model,
          CAST(attributes___gen_ai___usage___input_tokens AS DOUBLE PRECISION) AS input_tokens,
          CAST(attributes___gen_ai___usage___output_tokens AS DOUBLE PRECISION) AS output_tokens,
          attributes___gen_ai___usage___cost AS cost, attributes___gen_ai___tool___name AS tool,
          left(attributes -> 'gen_ai' -> 'tool' -> 'call' ->> 'arguments', 300) AS tool_args,
          attributes -> 'gen_ai' -> 'input' ->> 'preview' AS preview,
          CAST(attributes -> 'gen_ai' -> 'response' -> 'finish_reasons' AS TEXT) AS finish,
          attributes___user___id AS usr, resource___service___name AS svc
        FROM otel_logs_and_spans
        WHERE project_id = #{pid.toText} AND timestamp >= #{from} AND timestamp < #{to} AND attributes___gen_ai___operation___name IS NOT NULL
      )
      SELECT hour, unit, max(conv) AS conv, max(agent) AS agent, max(model) AS model, max(usr) AS usr, max(svc) AS svc,
        min(timestamp) AS started, max(timestamp) AS last_ts, max(CAST(EXTRACT(EPOCH FROM timestamp) * 1000000 AS BIGINT) + dur / 1000) AS end_us,
        sum(CASE WHEN model_op THEN 1 ELSE 0 END) AS llm_calls,
        sum(CASE WHEN op = 'execute_tool' THEN 1 ELSE 0 END) AS tool_calls,
        sum(CASE WHEN agent_op THEN 1 ELSE 0 END) AS agent_runs,
        sum(CASE WHEN model_op THEN COALESCE(input_tokens, 0) + COALESCE(output_tokens, 0) ELSE 0 END) AS tokens,
        sum(CASE WHEN model_op THEN cost END) AS cost_sum,
        sum(CASE WHEN failed THEN 1 ELSE 0 END) AS errors,
        sum(CASE WHEN failed AND op = 'execute_tool' THEN 1 ELSE 0 END) AS tool_errors,
        sum(CASE WHEN failed AND model_op THEN 1 ELSE 0 END) AS model_errors,
        sum(CASE WHEN ^{finished GenAI.Truncated} THEN 1 ELSE 0 END) AS truncated,
        sum(CASE WHEN ^{finished GenAI.Filtered} THEN 1 ELSE 0 END) AS filtered,
        sum(CASE WHEN op = 'execute_tool' THEN 1 ELSE 0 END) - count(DISTINCT CASE WHEN op = 'execute_tool' THEN COALESCE(tool, '') || COALESCE(tool_args, '') END) AS repeats,
        max(input_tokens) AS max_input,
        min(CASE WHEN preview IS NOT NULL THEN CAST(CAST(EXTRACT(EPOCH FROM timestamp) AS BIGINT) AS TEXT) || '|' || preview END) AS first_preview,
        min(CASE WHEN op = 'execute_tool' AND tool IS NOT NULL THEN CAST(CAST(EXTRACT(EPOCH FROM timestamp) AS BIGINT) AS TEXT) || '|' || tool || chr(9) || COALESCE(tool_args, '') END) AS first_tool
      FROM spans GROUP BY hour, unit ORDER BY last_ts DESC LIMIT #{unitCap}
    |]


unitCap :: Int64
unitCap = 10000


-- | The window's units, one per trace. Each settled hour (closed 10 minutes ago, so late spans have
-- landed) is aggregated once into @apis.llm_unit_hours@; only the window's unsettled edges are read
-- from the store per request, so a day costs about what its last hour does.
windowUnits :: (DB es, Labeled "timefusion" Hasql :> es, Reader AuthContext :> es, Time.Time :> es) => Projects.ProjectId -> TimePicker.TimeWindow -> Eff es [Unit]
windowUnits pid window = do
  now <- Time.currentTime
  let ceilHour t = posixSecondsToUTCTime $ fromIntegral (3600 * ceiling (utcTimeToPOSIXSeconds t / 3600) :: Int64)
      settled = takeWhile (\h -> addUTCTime 3600 h <= window.toTime && addUTCTime 4200 h <= now) $ iterate (addUTCTime 3600) (ceilHour window.fromTime)
  cached <- M.fromList . map (\(h, HI.AsJsonb us) -> (h, us)) <$> bool (Hasql.interp [HI.sql| SELECT hour, units FROM apis.llm_unit_hours WHERE project_id = #{pid} AND hour = ANY(#{V.fromList settled}) AND computed_at >= hour + interval '70 minutes' |]) (pure []) (null settled)
  fresh <- case nonEmpty (filter (`M.notMember` cached) settled) of
    Nothing -> pure M.empty
    Just missing -> do
      let (lo, hi) = (F1.minimum missing, addUTCTime 3600 $ F1.maximum missing)
      -- An hour with no GenAI spans is stored empty, so it is not scanned again.
      computed <- M.unionWith (<>) (M.fromList [(h, []) | h <- takeWhile (< hi) (iterate (addUTCTime 3600) lo)]) . M.fromListWith (<>) . map (\u -> (u.hour, [u])) <$> selectUnits pid lo hi
      Hasql.interpExecute_
        [HI.sql| INSERT INTO apis.llm_unit_hours (project_id, hour, units, computed_at)
          SELECT #{pid}, h, u, #{now} FROM unnest(#{V.fromList $ M.keys computed}::timestamptz[], #{V.fromList $ map AE.toJSON $ M.elems computed}::jsonb[]) AS v(h, u)
          ON CONFLICT (project_id, hour) DO UPDATE SET units = EXCLUDED.units, computed_at = EXCLUDED.computed_at |]
      pure computed
  edges <- case (settled, viaNonEmpty last settled) of
    (h0 : _, Just hN) -> (<>) <$> live window.fromTime h0 <*> live (addUTCTime 3600 hN) window.toTime
    _ -> live window.fromTime window.toTime
  pure $ M.elems $ M.fromListWith (flip (<>)) [(u.unit, u) | u <- concat (M.elems cached <> M.elems fresh) <> edges]
  where
    live from to = bool (selectUnits pid from to) (pure []) (from >= to)


-- | A conversation's spans with their attributes, oldest first, for the transcript and timeline.
selectCalls :: (DB es, Labeled "timefusion" Hasql :> es, Reader AuthContext :> es) => Projects.ProjectId -> UTCTime -> UTCTime -> Int64 -> [Text] -> Eff es [Call]
selectCalls pid from to limit units = do
  tf <- useTfReads
  Hasql.withHasqlTimefusion tf
    $ Hasql.interp
      [HI.sql|
      SELECT id::text, COALESCE(context___trace_id, ''), timestamp, COALESCE(duration, 0), COALESCE(status_code = 'ERROR', false), COALESCE(name, '') AS span_name, resource___service___name,
        attributes___gen_ai___operation___name AS operation,
        attributes___gen_ai___conversation___id AS conversation,
        attributes___gen_ai___agent___name AS agent,
        COALESCE(attributes -> 'gen_ai' -> 'response' ->> 'model', attributes___gen_ai___request___model) AS model,
        CAST(attributes___gen_ai___usage___input_tokens AS DOUBLE PRECISION) AS input_tokens,
        CAST(attributes___gen_ai___usage___output_tokens AS DOUBLE PRECISION) AS output_tokens,
        attributes___gen_ai___usage___cost AS cost,
        attributes___gen_ai___tool___name AS tool,
        left(attributes -> 'gen_ai' -> 'tool' -> 'call' ->> 'arguments', 300) AS tool_args,
        attributes -> 'gen_ai' -> 'input' ->> 'preview' AS preview,
        attributes -> 'gen_ai' -> 'response' -> 'finish_reasons' AS finish,
        attributes___user___id, COALESCE(context___span_id, '') AS span_id, NULLIF(parent_id, '') AS parent_span_id,
        (attributes -> 'gen_ai' -> 'usage' -> 'cache_read' ->> 'input_tokens')::float8 AS cache_read, attributes
      FROM otel_logs_and_spans
      WHERE project_id = #{pid.toText} AND timestamp >= #{from} AND timestamp <= #{to} AND attributes___gen_ai___operation___name IS NOT NULL
        AND (CASE WHEN COALESCE(context___trace_id, '') = '' THEN id::text ELSE context___trace_id END) = ANY(#{V.fromList units})
      ORDER BY timestamp ASC LIMIT #{limit} |]


-- | Appends each conversation's judged plain-language signals to its built-in flags.
withJudged :: DB es => Projects.ProjectId -> [Conversation] -> Eff es [Conversation]
withJudged pid convs = do
  matches <- M.fromListWith (flip (<>)) . map (\(k, name, reason) -> (k, [Judged name reason])) <$> selectMatches pid (V.fromList $ map (.key) convs)
  pure [c{signals = c.signals <> M.findWithDefault [] c.key matches} | c <- convs]


-- | Conversations span hours, so these pages open on a day rather than the app-wide five minutes.
llmWindow :: Time.Time :> es => Maybe Text -> Maybe Text -> Maybe Text -> Eff es TimePicker.TimeWindow
--
-- The refreshing shell resends an unset picker as @since=&from=&to=@; blanks are absent here too,
-- or the list read the app-wide five minutes while the page's charts read the day.
llmWindow fromP toP sinceP = mkWindow fromP toP (nonEmptyT sinceP <|> ("24H" <$ guard (all (isNothing . nonEmptyT) [fromP, toP])))


-- | Recurring openings: Drain over the conversations' first user messages, the clustering log patterns use.
data Topic = Topic {template :: Text, conversations :: [Conversation]}


-- | Each conversation joins the first final template its tokens match. Only a token that is wholly a
-- value (a number, id, email) becomes a placeholder, so "p95" or "500s" read as written.
--
-- >>> import Relude.Unsafe qualified as Unsafe
-- >>> let t = Unsafe.read @UTCTime "2026-01-01 00:00:00 UTC"
-- >>> let c k title = Conversation k (Just title) t t [] [] [] [] 1 1 0 10 Nothing 0 0 [] [] Nothing
-- >>> sortOn fst $ map (\x -> (x.template, map (.key) x.conversations)) $ topicsOf t [c "a" "refund order 1234 failed", c "b" "refund order 98 failed", c "c" "why is checkout slow", c "d" "p95 latency on 500s"]
-- [("p95 latency on 500s",["d"]),("refund order <*> failed",["a","b"]),("why is checkout slow",["c"])]
topicsOf :: UTCTime -> [Conversation] -> [Topic]
topicsOf now convs = sortOn (Down . length . (.conversations)) [Topic tmpl members | tmpl <- ordNub (map (.templateStr) results), let members = [c | (c, Just t) <- assigned, t == tmpl], not (null members)]
  where
    titled = [(c, V.map valueToken $ Drain.tokenizeForDrain t) | c <- convs, Just t <- [c.title]]
    valueToken tok = let r = replaceAllFormats tok in if T.any isAlpha tok && not ("{" `T.isPrefixOf` r && "}" `T.isSuffixOf` r && T.count "{" r == 1) then tok else Drain.normalizePlaceholder r
    results = V.toList $ Drain.getAllLogGroups $ Drain.buildDrainTree snd ((.key) . fst) (const Nothing) Drain.emptyDrainTree (V.fromList titled) now
    matching toks r = V.length r.templateTokens == V.length toks && V.and (V.zipWith (\p x -> p == x || Drain.isPlaceholderToken p) r.templateTokens toks)
    assigned = [(c, (.templateStr) <$> find (matching toks) results) | (c, toks) <- titled]


data View = ConversationsView | TopicsView | SignalsView
  deriving stock (Bounded, Enum, Eq)


viewLabel :: View -> Text
viewLabel = \case
  ConversationsView -> "Conversations"
  TopicsView -> "Topics"
  SignalsView -> "Signals"


viewParam :: View -> Text
viewParam = T.toLower . viewLabel


data LLMBody = ConversationsBody (Table Conversation) | TopicsBody (Table Topic) | SignalsBody (Table SignalRow)


-- | A Signals table row: a built-in rule counted over the window's conversations, or a plain-language signal's verdicts.
data SignalRow = SignalRow {name :: Text, description :: Text, matched :: Int64, judged :: Int64, builtIn :: Bool}


instance ToHtml LLMBody where
  toHtml = \case
    ConversationsBody t -> toHtml t
    TopicsBody t -> toHtml t
    SignalsBody t -> toHtml t
  toHtmlRaw = toHtml


-- | The project whose overview widgets head the page (none on Signals, where the form leads), and the page.
data LLMGet = LLMPage (Maybe Projects.ProjectId) (PageCtx (Deferred LLMBody))


-- | The overview widgets sit outside the deferred container, so they fetch while the list loads.
instance ToHtml LLMGet where
  toHtml (LLMPage overview (PageCtx bw body)) = toHtml $ PageCtx bw $ whenJust overview overview_ >> toHtml (RefreshingDeferred body)
  toHtmlRaw = toHtml


-- | The LLM dashboard template's opening widgets: one definition serves this page and the dashboard.
overview_ :: Projects.ProjectId -> Html ()
overview_ pid = div_ [class_ "px-4 pt-3"] $ Widget.embeddedWidgets_ pid $ take 3 $ DashboardTemplates.templateTab "llm-observability.yaml" "Overview" []


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
    let conversationsIn = windowUnits pid window >>= \units -> (,bool "" (" (newest " <> show unitCap <> " traces)") (length units >= fromIntegral unitCap)) <$> withJudged pid (conversationsOf units)
        reset = link view Nothing Nothing Nothing Nothing Nothing
        zero = ZeroState{icon = "sparkles", title = "No LLM or agent telemetry yet", description = "Send OpenTelemetry GenAI spans from any SDK or framework — OpenLLMetry, OpenInference, Vercel AI SDK, Pydantic AI, LangChain, the OpenAI Agents SDK, Claude Code — and conversations, cost and tool calls appear here.", action = ESLink "https://monoscopehq.com/docs/sdks/llm-observability/" "Instrument your agent"}
        table, base :: Table a
        table = inventoryTable "llm" url
        base = table{features = table.features{zeroState = Just zero, searchPlaceholder = Just $ "Search " <> viewParam view}}
    case view of
      ConversationsView -> do
        (conversations, capped) <- conversationsIn
        let topics = topicsOf now conversations
            labels = map flagLabel . (.signals)
            inTopic c = maybe True (\t -> any (\tp -> tp.template == t && any ((== c.key) . (.key)) tp.conversations) topics) topicM
            all' = V.fromList conversations
            shown = V.filter (\c -> want agentM c.agents && want modelM c.models && want signalM (labels c) && inTopic c) all'
        pure
          $ ConversationsBody
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
                            , countedFilter "Signal" "signal" signalM labels all'
                            ]
                    }
              }
      SignalsView -> do
        signals <- selectSignals pid
        (conversations, capped) <- conversationsIn
        let flagged sig = fromIntegral (length [c | c <- conversations, Detected sig `elem` c.signals])
            rows = [SignalRow s.name s.description s.matched s.judged False | s <- signals] <> [SignalRow (signalLabel sig) (flagHelp $ Detected sig) (flagged sig) (fromIntegral $ length conversations) True | sig <- universe]
        pure
          $ SignalsBody
            base
              { columns =
                  [ col
                      "Signal"
                      ( \s -> div_ [class_ "flex min-w-0 flex-col"] do
                          span_ [class_ "flex items-center gap-2 font-medium text-textStrong"] $ toHtml s.name >> when s.builtIn (span_ [class_ "rounded bg-fillWeak px-1.5 py-0.5 text-xs font-normal text-textWeak"] "Built-in")
                          span_ [class_ "truncate text-xs text-textWeak"] (toHtml s.description)
                      )
                      & fillCol
                  , col "Matched" (\s -> if s.matched > 0 then span_ [class_ "rounded-full bg-fillBrand-weak px-2 py-0.5 text-xs font-medium tabular-nums text-textBrand"] (toHtml $ prettyPrintCount $ fromIntegral s.matched) else num 0) & withAttrs [class_ "w-24 text-right"]
                  , col
                      "Match rate"
                      ( \s -> div_ [class_ "flex items-center gap-2"] do
                          let rate = if s.judged > 0 then 100 * fromIntegral s.matched / fromIntegral s.judged else 0 :: Double
                          div_ [class_ "h-1.5 flex-1 overflow-hidden rounded-full bg-fillWeak"] $ div_ [class_ "h-full rounded-full bg-fillBrand-strong", style_ $ "width:" <> show rate <> "%"] ""
                          span_ [class_ "w-10 text-right text-xs tabular-nums text-textWeak"] $ toHtml $ show @Text (round rate :: Int) <> "%"
                      )
                      & withAttrs [class_ "w-48"]
                  , col "Judged" (num . fromIntegral . (.judged)) & withAttrs [class_ "w-20 text-right"]
                  ]
              , rows = V.fromList rows
              , features =
                  base.features
                    { header = Just $ details_ ([class_ "group/new mt-3 rounded-xl border border-strokeWeak bg-bgRaised"] <> [open_ "" | null signals]) do
                        summary_ [class_ "flex cursor-pointer list-none flex-wrap items-center gap-x-3 gap-y-1 px-4 py-3 [&::-webkit-details-marker]:hidden"] do
                          span_ [class_ "inline-flex items-center gap-2 text-sm font-semibold text-textStrong"] $ span_ [class_ "grid h-6 w-6 place-items-center rounded-md bg-fillBrand-strong text-white"] (faSprite_ "plus" "regular" "h-3 w-3") >> "New signal"
                          span_ [class_ "min-w-0 flex-1 text-sm text-textWeak"] "Describe a behaviour in plain language. A model checks each day's conversations and flags the ones that match."
                          unless (null signals) $ button_ [type_ "button", class_ "btn btn-sm btn-ghost", hxPost_ (urls.traces pid).llmSignalRunPost, hxSwap_ "none", [__|on click halt the event's default|]] $ faSprite_ "arrows-rotate" "regular" "h-3.5 w-3.5" >> "Judge new conversations now"
                          faSprite_ "chevron-down" "regular" "h-3.5 w-3.5 text-iconNeutral transition-transform group-open/new:rotate-180"
                        form_ [class_ "flex flex-col gap-5 border-t border-strokeWeak p-4", hxPost_ (urls.traces pid).llmSignalPost, hxSwap_ "none"] do
                          div_ [class_ "grid gap-3 lg:grid-cols-[18rem_minmax(0,1fr)]"] do
                            formField_ FieldSm def{placeholder = "User frustration", extraAttrs = [maxlength_ "80"], inputId = Just "signal-name"} "Name" "name" True Nothing
                            formField_ FieldSm def{inputType = "textarea", placeholder = "The user repeats a request, says the answer is wrong, or gives up.", extraAttrs = [maxlength_ "2000", rows_ "3"], inputId = Just "signal-description"} "What to look for" "description" True Nothing
                          primaryButton_ [type_ "submit", class_ "self-start"] "Create and judge today's conversations"
                          div_ [class_ "flex flex-col gap-2"] do
                            span_ [class_ "text-xs font-medium text-textWeak"] "Or start from an example"
                            div_ [class_ "grid gap-2 sm:grid-cols-2 xl:grid-cols-3"] $ forM_ signalExamples \(role, name, description) ->
                              button_
                                [ type_ "button"
                                , class_ "flex items-start gap-3 rounded-lg border border-strokeWeak px-3 py-2 text-start transition-[background-color,border-color,scale] hover:border-strokeBrand-weak hover:bg-fillBrand-weak active:scale-[0.96]"
                                , data_ "name" name
                                , data_ "description" description
                                , [__|on click set #signal-name.value to @data-name then set #signal-description.value to @data-description then call #signal-description.focus()|]
                                ]
                                do
                                  roleAvatar_ role
                                  span_ [class_ "flex min-w-0 flex-col gap-0.5"] do
                                    span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml name
                                    span_ [class_ "text-xs text-textWeak"] $ toHtml description
                    , rowLink = Just \s -> link ConversationsView Nothing Nothing (Just s.name) Nothing Nothing
                    , resultSummary = Just $ countNoun (length signals) "custom signal" <> " · built-ins over " <> countNoun (length conversations) "conversation" <> capped
                    }
              }
      TopicsView -> do
        (conversations, capped) <- conversationsIn
        let topics = topicsOf now conversations
        pure
          $ TopicsBody
            base
              { columns = topicColumns
              , rows = V.fromList topics
              , features = base.features{rowLink = Just \t -> link ConversationsView Nothing Nothing Nothing (Just t.template) Nothing, resultSummary = Just $ show (length topics) <> " topics across " <> show (length conversations) <> " conversations" <> capped}
              }
  addRespHeaders
    $ LLMPage (pid <$ guard (view /= SignalsView))
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
    chips :: [Text] -> Html ()
    chips xs = div_ [class_ "flex min-w-0 flex-wrap gap-1"] $ forM_ (take 2 xs) (span_ [class_ "truncate rounded bg-fillWeak px-1.5 py-0.5 text-xs text-textWeak"] . toHtml) >> when (length xs > 2) (span_ [class_ "text-xs text-textWeak"] $ toHtml $ "+" <> show @Text (length xs - 2))

    num :: Double -> Html ()
    num = span_ [class_ "tabular-nums text-textWeak"] . toHtml . prettyPrintCount . round

    -- Starting points for a first signal, three per actor; each fills the form.
    signalExamples :: [(Text, Text, Text)]
    signalExamples =
      [ ("user", "User frustration", "The user repeats a request, says the answer is wrong, or gives up.")
      , ("user", "Question left unanswered", "The user asks something and the conversation ends without an answer to that question.")
      , ("user", "Asks for a human", "The user asks to speak to a person, contact support, or escalate.")
      , ("tool", "Wrong tool", "The agent calls a tool that cannot answer the question, or ignores what a tool returned.")
      , ("tool", "Tool failure ignored", "A tool returns an error or an empty result and the agent carries on as if it succeeded.")
      , ("tool", "Redundant lookups", "The agent fetches the same data more than once, or looks up something it already had.")
      , ("assistant", "Unsupported claims", "The assistant states numbers, names or links that no tool result or user message supports.")
      , ("assistant", "Refused a valid request", "The assistant declines, or says it cannot help with, a request it should handle.")
      , ("assistant", "Leaked instructions or data", "The assistant reveals its system prompt, internal instructions, credentials, or data belonging to another user.")
      ]

    conversationColumns :: [Column Conversation]
    conversationColumns =
      [ col "Conversation" titleCell & fillCol
      , col "Agent" (chips . (.agents)) & withAttrs [class_ "w-36 max-lg:hidden"]
      , col "Models" (chips . (.models)) & withAttrs [class_ "w-40 max-xl:hidden"]
      , col "Turns" (num . fromIntegral . (.turns)) & withAttrs [class_ "w-16 text-right"]
      , col "Model · tool calls" (\c -> span_ [class_ "whitespace-nowrap tabular-nums text-textWeak"] $ toHtml $ show @Text c.llmCalls <> " · " <> show c.toolCalls) & withAttrs [class_ "w-36 text-right max-md:hidden"]
      , col "Tokens" (num . (.tokens)) & withAttrs [class_ "w-20 text-right"]
      , col "Cost" (\c -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ maybe "—" GenAI.formatUsd c.cost) & withAttrs [class_ "w-20 text-right"]
      , col "Signals" signalsCell & withAttrs [class_ "w-44"]
      , col "Last seen" (localTimeFmt_ "MMM dd, HH:mm" . (.lastSeen)) & withAttrs [class_ "w-28 whitespace-nowrap max-md:hidden"]
      ]
      where
        titleCell, signalsCell :: Conversation -> Html ()
        titleCell c = div_ [class_ "flex min-w-0 flex-col"] do
          span_ [class_ $ "truncate font-medium " <> maybe "text-textWeak" (const "text-textStrong") c.title, title_ $ fromMaybe "" c.title] $ toHtml $ displayTitle c
          span_ [class_ "truncate text-xs text-textWeak"] $ toHtml $ T.intercalate " · " $ catMaybes [Just $ T.take 12 c.key, viaNonEmpty head c.users, viaNonEmpty head c.services, Just $ toText (getDurationNSMS $ fromIntegral c.activeNs)]
        signalsCell c = div_ [class_ "flex flex-wrap gap-1"] do
          when (c.errors > 0 && null c.signals) $ tonePill_ ErrorBadge $ countNoun c.errors "error"
          forM_ c.signals \f -> span_ [term "data-tippy-content" $ flagHelp f] $ tonePill_ (flagTone f) $ flagLabel f

    topicColumns :: [Column Topic]
    topicColumns =
      [ col "Topic" (\t -> span_ [class_ "block truncate text-sm text-textStrong", title_ t.template] $ toHtml t.template) & fillCol
      , col "Conversations" (num . fromIntegral . length . (.conversations)) & withAttrs [class_ "w-28 text-right"]
      , col "Tokens" (num . sum . map (.tokens) . (.conversations)) & withAttrs [class_ "w-20 text-right"]
      , col "Cost" (\t -> span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ maybe "—" GenAI.formatUsd $ viaNonEmpty sum $ mapMaybe (.cost) t.conversations) & withAttrs [class_ "w-20 text-right"]
      , col "With signals" (\t -> num $ fromIntegral $ length $ filter (not . null . (.signals)) t.conversations) & withAttrs [class_ "w-28 text-right"]
      ]


flagTone :: Flag -> BadgeColor
flagTone = \case
  Detected (ToolError; ModelError) -> ErrorBadge
  Detected (Truncated; Filtered; Looping; ContextPressure) -> WarningBadge
  Judged{} -> BrandBadge


flagHelp :: Flag -> Text
flagHelp = \case
  Detected s -> case s of
    ToolError -> "A tool call failed."
    ModelError -> "A model call returned an error."
    Truncated -> "A reply stopped at the output token limit."
    Filtered -> "A reply was refused or cut by a content filter."
    Looping -> "The same tool ran with the same arguments three or more times."
    ContextPressure -> "A call used at least 80% of the model's context window."
  Judged _ reason -> reason


-- * Conversation detail


data ConversationDetail = ConversationDetail {pid :: Projects.ProjectId, key :: Text, window :: TimePicker.TimeWindow, summary :: Maybe Conversation, steps :: [Call]}


newtype ConversationGet = ConversationPage (PageCtx ConversationDetail)
  deriving newtype (ToHtml)


conversationGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ConversationGet)
conversationGetH pid key fromP toP sinceP = do
  (_, _, bw) <- mkPageCtx pid
  window <- llmWindow fromP toP sinceP
  summary <- viaNonEmpty head <$> (withJudged pid . filter ((== key) . (.key)) . conversationsOf =<< windowUnits pid window)
  steps <- maybe (pure []) (\c -> selectCalls pid c.started c.lastSeen 2000 c.units) summary
  addRespHeaders
    $ ConversationPage
    $ PageCtx
      bw
        { pageTitle = "LLM & Agents"
        , pageTitleUrl = Just $ windowed window ((urls.traces pid).llmGetH Nothing Nothing Nothing Nothing Nothing) Nothing
        , pageTitleSuffix = Just $ maybe key (truncateText 60 . fromMaybe "Conversation" . (.title)) summary
        , menuItem = Just "LLM & Agents"
        , pageActions = Just $ TimePicker.liveDataControls_ Nothing window.currentRange Nothing TimePicker.RefreshOnly
        }
      ConversationDetail{pid, key, window, summary, steps}


instance ToHtml ConversationDetail where
  toHtml = toHtmlRaw . conversationDetail_
  toHtmlRaw = toHtml


conversationDetail_ :: ConversationDetail -> Html ()
conversationDetail_ d = div_ [class_ "mx-auto flex w-full max-w-7xl flex-col gap-5 p-4"] $ case d.summary of
  Nothing -> callout_ WarningBadge Nothing "No spans for this conversation in the selected time range. Widen the range from the picker above."
  Just c -> do
    header_ [class_ "flex flex-col gap-1.5"] do
      div_ [class_ "flex flex-wrap items-start gap-3"] do
        h1_ [class_ "min-w-0 flex-1 text-xl font-semibold text-textStrong text-pretty line-clamp-2 max-sm:basis-full", title_ $ fromMaybe "" c.title] $ toHtml $ displayTitle c
        div_ [class_ "flex flex-wrap items-center gap-2"] do
          askAi_ c
          whenJust (find (not . T.null . (.traceId)) d.steps) \s ->
            a_ (class_ "btn btn-sm" : LogItem.traceLinkAttrs_ d.pid s.traceId s.timestamp) $ faSprite_ "diagram-project" "regular" "h-3.5 w-3.5" >> "Open trace"
          a_ ([class_ "btn btn-sm btn-ghost", href_ explorer] <> navTabAttrs) $ faSprite_ "explore" "regular" "h-3.5 w-3.5" >> "Spans in Explorer"
      div_ [class_ "flex flex-wrap items-center gap-x-2 gap-y-1 text-sm text-textWeak"]
        $ sequence_
        $ intersperse (span_ [class_ "text-textDisabled", Aria.hidden_ "true"] "·")
        $ [toHtml $ T.intercalate ", " c.agents | not (null c.agents)]
        <> [toHtml $ T.intercalate ", " c.models | not (null c.models)]
        <> [faSprite_ "user" "regular" "h-3 w-3" >> toHtml u | u <- take 1 c.users]
        <> [localTimeFmt_ "MMM dd, HH:mm:ss" c.started]
        <> [toHtml $ show @Text c.turns <> bool " turns" " turn" (c.turns == 1)]
    div_ [class_ "grid grid-cols-2 gap-px overflow-hidden rounded-xl border border-strokeWeak bg-strokeWeak sm:grid-cols-3 xl:grid-cols-6"] do
      stat_ "Cost" (maybe "—" GenAI.formatUsd c.cost) (bool "all calls priced" (show unpriced <> " unpriced calls") (unpriced > 0)) False
      stat_ "Tokens" (prettyPrintCount $ round c.tokens) (T.intercalate " · " $ ["in " <> count (.inputTokens), "out " <> count (.outputTokens)] <> ["cached " <> count (.cacheRead) | any (isJust . (.cacheRead)) inference]) False
      stat_ "Model calls" (show c.llmCalls) (maybe "" (("avg " <>) . toText . getDurationNSMS . round) avgLatency) False
      stat_ "Tool calls" (show c.toolCalls) (bool "none failed" (show failedTools <> " failed") (failedTools > 0)) (failedTools > 0)
      stat_ "Errors" (show c.errors) ("of " <> show (length d.steps) <> " spans") (c.errors > 0)
      stat_ "Duration" (toText $ getDurationNSMS $ round $ wall * 1e9) (if abs (fromIntegral c.activeNs - wall * 1e9) < 1e6 then show (length c.units) <> bool " traces" " trace" (length c.units == 1) else "active " <> toText (getDurationNSMS $ fromIntegral c.activeNs)) False
    unless (null c.signals) $ div_ [class_ "flex flex-col gap-1.5"] $ forM_ c.signals \f -> callout_ (flagTone f) Nothing $ strong_ (toHtml $ flagLabel f) >> " — " >> toHtml (flagHelp f)
    div_ [class_ "grid items-start gap-5 lg:grid-cols-[minmax(0,1fr)_19rem]"] do
      div_ [class_ "group/ctab flex min-w-0 flex-col gap-3"] do
        div_ [role_ "radiogroup", Aria.label_ "Conversation views", class_ "tabs tabs-box w-max"] do
          label_ [class_ "tab has-[:checked]:tab-active"] $ input_ [type_ "radio", name_ "ctab", class_ "sr-only ctab-timeline", checked_] >> "Timeline"
          label_ [class_ "tab has-[:checked]:tab-active"] $ input_ [type_ "radio", name_ "ctab", class_ "sr-only ctab-transcript"] >> "Transcript"
        section_ [class_ "hidden group-has-[.ctab-timeline:checked]/ctab:block"] timeline_
        section_ [class_ "hidden group-has-[.ctab-transcript:checked]/ctab:block"] transcript_
      aside_ [class_ "flex flex-col gap-4"] do
        card_ "Cost by model" $ forM_ byModel \(model, calls) -> do
          let cost = sum (mapMaybe (.cost) calls)
          div_ [class_ "flex flex-col gap-1"] do
            div_ [class_ "flex items-baseline gap-2 text-sm"] do
              span_ [class_ "min-w-0 flex-1 truncate font-medium text-textStrong"] $ toHtml model
              span_ [class_ "tabular-nums text-textStrong"] $ toHtml $ if any (isJust . (.cost)) calls then GenAI.formatUsd cost else "unpriced"
            div_ [class_ "h-1.5 overflow-hidden rounded-full bg-fillWeak"] $ div_ [class_ "h-full rounded-full bg-fillBrand-strong", style_ $ "width:" <> show (share cost) <> "%"] ""
            span_ [class_ "text-xs tabular-nums text-textWeak"] $ toHtml $ show (length calls) <> bool " calls" " call" (length calls == 1) <> " · " <> prettyPrintCount (round $ sum $ mapMaybe (\x -> (+) <$> x.inputTokens <*> x.outputTokens <|> x.inputTokens <|> x.outputTokens) calls) <> " tokens"
        unless (null byTool) $ card_ "Tools" $ forM_ byTool \(tool, calls) -> do
          let failed = length (filter (.failed) calls)
          div_ [class_ "flex items-baseline gap-2 text-sm"] do
            span_ [class_ "min-w-0 flex-1 truncate font-mono text-xs text-textStrong"] $ toHtml tool
            when (failed > 0) $ span_ [class_ "inline-flex items-center gap-1 text-xs text-textError"] $ faSprite_ "alert-triangle" "regular" "h-3 w-3" >> toHtml (show @Text failed <> " failed")
            span_ [class_ "text-xs tabular-nums text-textWeak"] $ toHtml $ show @Text (length calls) <> "× · " <> toText (getDurationNSMS $ fromIntegral $ sum (map (.duration) calls) `div` fromIntegral (length calls))
  where
    explorer = (urls.logExplorer d.pid).logExplorerGet (Just $ "attributes.gen_ai.conversation.id == \"" <> d.key <> "\" or trace_id == \"" <> d.key <> "\"") Nothing d.window.sinceQuery d.window.fromQuery d.window.toQuery (Just "spans") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    inference = filter (GenAI.isInferenceOp . (.operation)) d.steps
    unpriced = length $ filter (isNothing . (.cost)) inference
    count f = prettyPrintCount $ round $ sum $ mapMaybe f inference
    avgLatency = viaNonEmpty (\xs -> fromIntegral (sum xs) / fromIntegral (length xs) :: Double) (map (.duration) inference)
    failedTools = length [s | s <- d.steps, s.operation == "execute_tool", s.failed]
    byModel = sortOn (Down . sum . mapMaybe (.cost) . snd) $ M.toList $ M.fromListWith (flip (<>)) [(fromMaybe "unknown model" s.model, [s]) | s <- inference]
    byTool = sortOn (Down . length . snd) $ M.toList $ M.fromListWith (flip (<>)) [(fromMaybe s.name s.tool, [s]) | s <- d.steps, s.operation == "execute_tool"]
    share x = maybe 0 (\t -> if t > 0 then 100 * x / t else 0) (summaryCost =<< d.summary) :: Double
    summaryCost s = s.cost
    start = maybe d.window.fromTime (.timestamp) (viaNonEmpty head d.steps)
    end = maybe start (F1.maximum . fmap (\s -> addUTCTime (fromIntegral s.duration / 1e9) s.timestamp)) (nonEmpty d.steps)
    wall = max 0.000001 $ realToFrac (diffUTCTime end start) :: Double

    stat_ :: Text -> Text -> Text -> Bool -> Html ()
    stat_ label value sub alarming = div_ [class_ "flex min-w-0 flex-col gap-0.5 bg-bgRaised px-4 py-3"] do
      span_ [class_ "text-xs font-medium text-textWeak"] $ toHtml label
      span_ [class_ $ "flex items-center gap-1.5 text-xl font-semibold tabular-nums " <> bool "text-textStrong" "text-textError" alarming] $ when alarming (faSprite_ "alert-triangle" "regular" "h-4 w-4") >> toHtml value
      span_ [class_ "text-xs tabular-nums text-textWeak"] $ toHtml sub

    card_ :: Text -> Html () -> Html ()
    card_ title body = section_ [class_ "flex flex-col gap-3 rounded-xl border border-strokeWeak bg-bgRaised p-4"] $ h2_ [class_ "text-sm font-semibold text-textStrong"] (toHtml title) >> body

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
        source = if any (isJust . messages "input") inference then inference else filter ((`elem` GenAI.agentRunOps) . (.operation)) d.steps
        at c = genAIAt (unAesonTextMaybe c.attributes)
        messages dir c = (\ms -> ms <$ guard (not $ null ms)) $ genAIMessages (unAesonTextMaybe c.attributes) dir
        tools = M.fromList [(i, c) | c <- d.steps, Just (AE.String i) <- [at c ["tool", "call", "id"]]]
        rendered = go HS.empty source
        go _ [] = []
        go seen (s : rest) =
          let fresh = filter (not . (`HS.member` seen)) (fromMaybe [] $ messages "input" s)
              out = fromMaybe [] $ messages "output" s
              seen' = foldr HS.insert seen (fresh <> out)
           in (forM_ fresh (message_ d.pid tools Nothing) >> forM_ out (message_ d.pid tools (Just s))) : go seen' rest

    -- \| The AI spans as a waterfall: nested under their parent span, on the conversation's own
    -- clock, coloured by kind, with each step's tokens, cost and the running total.
    timeline_ :: Html ()
    timeline_ = div_ [class_ "flex flex-col gap-2"] do
      div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-1 text-xs text-textWeak"] do
        forM_ ([AgentStep, ModelStep, ToolStep] :: [StepKind]) \k -> span_ [class_ "inline-flex items-center gap-1.5"] $ span_ [class_ "w-2.5"] (swatch_ k False) >> toHtml (kindLabel k)
        span_ [class_ "inline-flex items-center gap-1.5"] $ span_ [class_ "w-2.5"] (swatch_ ModelStep True) >> "Error"
        span_ [class_ "ms-auto tabular-nums"] $ toHtml $ show @Text (length d.steps) <> " spans over " <> toText (getDurationNSMS $ round $ wall * 1e9)
      div_ [class_ "overflow-hidden rounded-xl border border-strokeWeak bg-bgRaised"] do
        div_ [class_ "grid grid-cols-[minmax(0,1fr)_6.5rem] items-end gap-3 border-b border-strokeWeak bg-fillWeaker px-3 py-1.5 text-xs font-medium text-textWeak md:grid-cols-[minmax(13rem,2fr)_minmax(10rem,3fr)_5.5rem_6.5rem]"] do
          span_ "Span"
          div_ [class_ "relative h-4 max-md:hidden"] $ forM_ ([0, 0.25, 0.5, 0.75, 1] :: [Double]) \q ->
            span_ [class_ $ "absolute bottom-0 whitespace-nowrap tabular-nums font-normal " <> bool "-translate-x-1/2" "-translate-x-full" (q == 1), style_ $ "left:" <> show (q * 100) <> "%"] $ toHtml $ if q == 0 then "0" else toText $ getDurationNSMS $ round $ q * wall * 1e9
          span_ [class_ "text-right max-md:hidden"] "Tokens"
          span_ [class_ "text-right"] "Cost · total"
        forM_ (zip3 ordered running subtrees) \((depth, s), total, (subCost, subTokens)) -> do
          let offset = pct $ realToFrac (diffUTCTime s.timestamp start)
              width = pct (fromIntegral s.duration / 1e9)
              k = kindOf s.operation
          div_ ([class_ "grid cursor-pointer grid-cols-[minmax(0,1fr)_6.5rem] items-center gap-3 border-b border-strokeWeak px-3 py-1.5 transition-colors last:border-b-0 hover:bg-fillHover md:grid-cols-[minmax(13rem,2fr)_minmax(10rem,3fr)_5.5rem_6.5rem]"] <> maybe [] (\u -> drawerRowAttrs_ $ (urls.logExplorer d.pid).logExplorerItemDetailedGet u s.timestamp Nothing (Just "tab-genai") Nothing False) (UUID.fromText s.id)) do
            div_ [class_ "flex min-w-0 items-center gap-2", style_ $ "padding-inline-start:" <> show (depth * 16) <> "px"] do
              kindIcon_ k
              div_ [class_ "flex min-w-0 flex-col"] do
                span_ [class_ "truncate text-sm text-textStrong"] $ toHtml $ case k of
                  ToolStep -> fromMaybe s.name s.tool
                  AgentStep -> fromMaybe s.name s.agent
                  (ModelStep; OtherStep) -> s.name
                span_ [class_ "truncate text-xs text-textWeak"] $ toHtml $ T.intercalate " · " $ catMaybes [Just $ kindLabel k, s.model <* guard (k == ModelStep), s.toolArgs <* guard (k == ToolStep)]
              when s.failed $ tonePill_ ErrorBadge "Error"
            div_ [class_ "relative h-6 max-md:hidden"] do
              forM_ ([25, 50, 75] :: [Int]) \q -> div_ [class_ "absolute inset-y-0 border-l border-dashed border-strokeWeak", style_ $ "left:" <> show q <> "%"] ""
              div_ [class_ "absolute top-1/2 h-2.5 -translate-y-1/2 rounded-sm", style_ $ "left:" <> show offset <> "%;width:max(" <> show (min width (100 - offset)) <> "%, 6px)"] $ swatch_ k s.failed
              span_ [class_ "absolute top-1/2 -translate-y-1/2 whitespace-nowrap text-2xs tabular-nums text-textWeak", style_ $ if offset + width < 78 then "left:calc(" <> show offset <> "% + max(" <> show width <> "%, 6px) + 6px)" else "right:calc(" <> show (100 - offset) <> "% + 6px)"] $ toHtml $ getDurationNSMS $ fromIntegral s.duration
            -- An agent owns its calls' usage: its row carries the sub-tree's totals, muted.
            span_ [class_ "text-right text-xs tabular-nums text-textWeak max-md:hidden"] $ toHtml $ case k of
              AgentStep | subTokens > 0 -> "Σ " <> prettyPrintCount (round subTokens) <> " tok"
              _ -> fromMaybe "" $ tokenFlow s.inputTokens s.outputTokens
            div_ [class_ "flex flex-col items-end text-xs tabular-nums"] $ case (k, stepCost s) of
              (_, Just c) -> span_ [class_ "text-textStrong"] (toHtml $ GenAI.formatUsd c) >> span_ [class_ "text-2xs text-textWeak"] (toHtml $ "Σ " <> GenAI.formatUsd total)
              (AgentStep, Nothing) -> whenJust subCost \c -> span_ [class_ "text-textWeak"] $ toHtml $ "Σ " <> GenAI.formatUsd c
              _ -> pass
      where
        pct x = 100 * x / wall :: Double
        running = drop 1 $ scanl (+) 0 $ map (fromMaybe 0 . stepCost . snd) ordered
        -- Each row's descendants (the rows after it until the depth returns to its own): their priced cost and model tokens.
        subtrees =
          [ (viaNonEmpty sum (mapMaybe (stepCost . snd) kids), sum [fromMaybe 0 x.inputTokens + fromMaybe 0 x.outputTokens | (_, x) <- kids, GenAI.isInferenceOp x.operation])
          | ((depth, _), rest) <- zip ordered (drop 1 $ tails ordered)
          , let kids = takeWhile ((> depth) . fst) rest
          ]
        kindOf :: Text -> StepKind
        kindOf op
          | op `elem` ("create_agent" : GenAI.agentRunOps) = AgentStep
          | GenAI.isInferenceOp op = ModelStep
          | op == "execute_tool" = ToolStep
          | otherwise = OtherStep

        kindLabel :: StepKind -> Text
        kindLabel = \case
          AgentStep -> "Agent"
          ModelStep -> "Model call"
          ToolStep -> "Tool"
          OtherStep -> "Span"

        -- The kind colour is categorical, like service colours; an error always reads red.
        swatch_ :: StepKind -> Bool -> Html ()
        swatch_ k failed = span_ [class_ $ "block h-2.5 w-full min-w-1.5 rounded-sm " <> if failed then "bg-fillError-strong" else case k of { AgentStep -> "bg-violet-400"; ModelStep -> "bg-blue-400"; ToolStep -> "bg-teal-400"; OtherStep -> "bg-fillNeutral-strong" }] ""

        kindIcon_ :: StepKind -> Html ()
        kindIcon_ = \case
          AgentStep -> span_ [class_ "grid h-6 w-6 shrink-0 place-items-center rounded-md bg-violet-400/15 text-violet-600 dark:text-violet-300"] $ faSprite_ "sparkles" "regular" "h-3.5 w-3.5"
          ModelStep -> span_ [class_ "grid h-6 w-6 shrink-0 place-items-center rounded-md bg-blue-400/15 text-blue-600 dark:text-blue-300"] $ faSprite_ "comment" "regular" "h-3.5 w-3.5"
          ToolStep -> span_ [class_ "grid h-6 w-6 shrink-0 place-items-center rounded-md bg-teal-400/15 text-teal-700 dark:text-teal-300"] $ faSprite_ "function" "regular" "h-3.5 w-3.5"
          OtherStep -> span_ [class_ "grid h-6 w-6 shrink-0 place-items-center rounded-md bg-fillWeak text-iconNeutral"] $ faSprite_ "diagram-project" "regular" "h-3.5 w-3.5"
        -- Only model calls are priced; a producer's own total on an agent span would count twice.
        stepCost s = s.cost <* guard (GenAI.isInferenceOp s.operation)
        -- Spans under their parent, siblings in time order; a parent outside this conversation makes a root.
        ordered = concatMap (walk (0 :: Int)) roots
        known = S.fromList [s.spanId | s <- d.steps, not (T.null s.spanId)]
        children = M.fromListWith (flip (<>)) [(p, [s]) | s <- d.steps, Just p <- [s.parentId], p /= s.spanId, p `S.member` known]
        roots = [s | s <- d.steps, maybe True (\p -> p == s.spanId || not (p `S.member` known)) s.parentId]
        walk depth s = (depth, s) : if depth >= 24 then [] else concatMap (walk (depth + 1)) (M.findWithDefault [] s.spanId children)


data StepKind = AgentStep | ModelStep | ToolStep | OtherStep
  deriving stock (Eq)


-- * Signals described in plain language


-- | A behaviour a model checks conversations for, with how many it has judged and matched.
data LLMSignal = LLMSignal {id :: UUIDId "llm_signal", name :: Text, description :: Text, matched :: Int64, judged :: Int64}
  deriving stock (Generic)
  deriving anyclass (HI.DecodeRow)


selectSignals :: DB es => Projects.ProjectId -> Eff es [LLMSignal]
selectSignals pid =
  Hasql.interp
    [HI.sql| SELECT s.id, s.name, s.description, count(*) FILTER (WHERE m.matched), count(m.signal_id)
      FROM apis.llm_signals s LEFT JOIN apis.llm_signal_matches m ON m.signal_id = s.id
      WHERE s.project_id = #{pid} GROUP BY s.id ORDER BY s.created_at |]


-- | (conversation key, signal name, reason) for the matches among these conversations.
selectMatches :: DB es => Projects.ProjectId -> V.Vector Text -> Eff es [(Text, Text, Text)]
selectMatches pid keys =
  Hasql.interp
    [HI.sql| SELECT m.conversation_key, s.name, m.reason FROM apis.llm_signal_matches m JOIN apis.llm_signals s ON s.id = m.signal_id
      WHERE s.project_id = #{pid} AND m.matched AND m.conversation_key = ANY(#{keys}) |]


data Verdict = Verdict {key :: Text, match :: Bool, reason :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data JudgeError = JudgeCallFailed Text | JudgeUnparseable Text
  deriving stock (Show)


-- | The judge's answer, fenced or not. A key answered twice is dropped ('Drain.answeredOnce').
--
-- >>> parseVerdicts "```json\n[{\"key\":\"c1\",\"match\":true,\"reason\":\"asked twice\"}]\n```"
-- Right [Verdict {key = "c1", match = True, reason = "asked twice"}]
-- >>> map (.key) <$> parseVerdicts "[{\"key\":\"a\",\"match\":true,\"reason\":\"\"},{\"key\":\"b\",\"match\":true,\"reason\":\"\"},{\"key\":\"a\",\"match\":false,\"reason\":\"\"}]"
-- Right ["b"]
-- >>> isLeft $ parseVerdicts "I think yes"
-- True
parseVerdicts :: Text -> Either Text [Verdict]
parseVerdicts = bimap toText (Drain.answeredOnce (.key)) . AE.eitherDecodeStrict . encodeUtf8 . ELLM.stripCodeBlock


-- | Judges the day's conversations each signal has not judged yet (at most 20 per signal per run):
-- one window read and one span fetch for the project, then one model call per signal.
evaluateSignals :: (DB es, ELLM.LLM :> es, Labeled "timefusion" Hasql :> es, Log :> es, Reader AuthContext :> es, Time.Time :> es) => Projects.ProjectId -> Eff es ()
evaluateSignals pid = do
  window <- llmWindow Nothing Nothing Nothing
  -- Run hourly, this also aggregates the day's newly settled hours before anyone opens the pages.
  convs <- conversationsOf <$> windowUnits pid window
  signals <- selectSignals pid
  unless (null signals) do
    ctx <- ask @AuthContext
    judged <-
      fromList
        <$> Hasql.interp @[(UUIDId "llm_signal", Text)]
          [HI.sql| SELECT signal_id, conversation_key FROM apis.llm_signal_matches
            WHERE signal_id = ANY(#{V.fromList $ map (.id) signals}) AND conversation_key = ANY(#{V.fromList $ map (.key) convs}) |]
    let pending :: LLMSignal -> [Conversation]
        pending sig = take 20 [c | c <- convs, not $ (sig.id, c.key) `S.member` judged]
        wanted = ordNubOn (.key) $ concatMap pending signals
    calls <- maybe (pure []) (\ws -> selectCalls pid (F1.minimum $ fmap (.started) ws) (F1.maximum $ fmap (.lastSeen) ws) 5000 (concatMap (.units) ws)) (nonEmpty wanted)
    let byUnit = M.fromListWith (flip (<>)) [(bool c.traceId c.id (T.null c.traceId), [c]) | c <- calls]
        texts = M.fromList [(c.key, conversationText c.title $ sortOn (.timestamp) $ concatMap (\u -> M.findWithDefault [] u byUnit) c.units) | c <- wanted]
    forM_ signals \sig -> whenNotNull (pending sig) \(toList -> todo) -> do
      let prompt =
            unlines
              $ [ "You judge AI agent conversations for an observability product."
                , "Signal \"" <> sig.name <> "\": " <> sig.description
                , "For each conversation below, decide whether the signal applies. Answer with JSON only: an array of {\"key\": string, \"match\": boolean, \"reason\": one short sentence}."
                ]
              <> ["<conversation key=\"" <> c.key <> "\">\n" <> M.findWithDefault "" c.key texts <> "\n</conversation>" | c <- todo]
      reply <- ELLM.callLLM ctx.env.openaiSmallModel prompt ctx.env.openaiApiKey
      case first JudgeCallFailed reply >>= first JudgeUnparseable . parseVerdicts of
        Left err -> Log.logAttention "LLM signal evaluation failed" (AE.object ["signal" AE..= sig.id, "error" AE..= show @Text err])
        Right verdicts -> do
          let vs = filter ((`elem` map (.key) todo) . (.key)) verdicts
          Hasql.interpExecute_
            [HI.sql| INSERT INTO apis.llm_signal_matches (signal_id, conversation_key, matched, reason)
              SELECT #{sig.id}, k, m, r FROM unnest(#{V.fromList $ map (.key) vs}::text[], #{V.fromList $ map (.match) vs}::bool[], #{V.fromList $ map (.reason) vs}::text[]) AS v(k, m, r)
              ON CONFLICT (signal_id, conversation_key) DO UPDATE SET matched = EXCLUDED.matched, reason = EXCLUDED.reason, evaluated_at = now() |]
  where
    -- What the judge reads: the opening request, the latest call's messages, and failed tools.
    conversationText :: Maybe Text -> [Call] -> Text
    conversationText title calls = T.take 6000 $ unlines $ maybe [] (\t -> ["Opening request: " <> t]) title <> map line history <> ["Failed tool: " <> fromMaybe c.name c.tool | c <- calls, c.failed, c.operation == "execute_tool"]
      where
        msgs c = genAIMessages (unAesonTextMaybe c.attributes)
        history = maybe [] (\c -> msgs c "input" <> msgs c "output") $ viaNonEmpty last [c | c <- calls, not $ null $ msgs c "input"]
        line (messageParts -> (role, parts)) = role <> ": " <> unwords (map part parts)
        part p = case p ^? AL.key "type" . AL._String of
          Just "tool_call" -> "[calls " <> fromMaybe "tool" (p ^? AL.key "name" . AL._String) <> "]"
          Just "tool_call_response" -> "[result " <> T.take 200 (maybe "" jsonAsText (p ^? AL.key "response")) <> "]"
          Just _ -> maybe "" jsonAsText (p ^? AL.key "content")
          Nothing -> jsonAsText p


data SignalForm = SignalForm {name :: Text, description :: Text}
  deriving stock (Generic)
  deriving anyclass (FromForm)


data SignalRejection = SignalIncomplete | SignalNameTaken Text
  deriving stock (Eq, Show)


instance Refusal SignalRejection where
  refusalMessage = \case
    SignalIncomplete -> "A signal needs a name (up to 80 characters) and a description (up to 2,000)"
    SignalNameTaken name -> "A signal named “" <> name <> "” already exists"


-- | Queued for judging: a new signal reloads the Signals view to list it; a re-run only says so.
data SignalQueued = SignalCreated Projects.ProjectId (UUIDId "llm_signal") | SignalsRequeued
  deriving stock (Eq, Show)


instance Outcome SignalQueued where
  outcomeRedirect = \case
    SignalCreated pid _ -> Just $ (urls.traces pid).llmGetH (Just $ viewParam SignalsView) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    SignalsRequeued -> Nothing
  outcomeToast _ = Just "Judging today's conversations against your signals. Matches appear in a minute."


llmSignalPostH :: Projects.ProjectId -> SignalForm -> ATAuthCtx (ActionResult SignalRejection SignalQueued)
llmSignalPostH pid form = act do
  _ <- ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  taken <- (map signalLabel universe <>) . map (.name) <$> selectSignals pid
  let (name, description) = (T.strip form.name, T.strip form.description)
  if
    | T.null name || T.null description || T.length name > 80 || T.length description > 2000 -> pure $ Left SignalIncomplete
    | any ((== T.toLower name) . T.toLower) taken -> pure $ Left $ SignalNameTaken name
    | otherwise -> do
        sid <- Hasql.interpScalar [HI.sql| INSERT INTO apis.llm_signals (project_id, name, description) VALUES (#{pid}, #{name}, #{description}) RETURNING id |]
        Right (SignalCreated pid sid) <$ Jobs.insertJob (Jobs.EvaluateLLMSignals pid)


llmSignalRunPostH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Done SignalQueued))
llmSignalRunPostH pid = do
  _ <- ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  Jobs.insertJob (Jobs.EvaluateLLMSignals pid) >> done SignalsRequeued
