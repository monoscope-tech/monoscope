module System.Types (
  ATBaseCtx,
  ProjectToolCtx,
  ATAuthCtx,
  ATBackgroundCtx,
  ATBackgroundEffects,
  DB,
  runBackground,
  addRespHeaders,
  addTriggerEvent,
  addToast,
  addSuccessToast,
  addErrorToast,
  toastError,
  addWidgetJSON,
  addReswap,
  HXRedirectDest,
  HXReswap (..),
  XWidgetJSON,
  TriggerEvents,
  RespHeaders,
  redirectCS,
  effToServantHandler,
  effToServantHandlerTest,
  effToServantHandlerTestHTTP,
  effToHandler,
  atAuthToBase,
  atAuthToBaseTest,
  useTfReads,
  ApiPrincipal (..),
  Done (..),
  Refused (..),
  Refusal (..),
  Outcome (..),
  Rendered (..),
  Toast (..),
  successToast,
  ToastLevel (..),
  Action,
  Unrefusable,
  ActionResult,
  act,
  done,
)
where

import Control.Monad.Except qualified as Except
import Data.Aeson qualified as AE
import Data.Effectful.AgentArtifacts qualified as Artifacts
import Data.Effectful.AgentWorkspace qualified as Workspace
import Data.Effectful.CodeReview qualified as CodeReview
import Data.Effectful.Hasql (Hasql, TestClock, runHasqlPool, runHasqlPoolSynced, runMutableTime)
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.Notify qualified
import Data.Effectful.Sleep (Sleep, runNoSleep, runSleep)
import Data.Effectful.UUID (UUIDEff, runStaticUUIDRef, runUUID)
import Data.Effectful.Wreq (HTTP, runHTTPGolden, runHTTPWreq)
import Data.Map qualified as Map
import Data.UUID qualified as UUID
import Effectful
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled, runLabeled)
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Effectful.State.Static.Local qualified as State
import Effectful.Time (Time, runTime)
import Effectful.Timeout (Timeout, runTimeout)
import Log qualified
import Models.Projects.Projects qualified as Sessions
import OpenTelemetry.Trace (TracerProvider)
import Pkg.DeriveUtils (DB)
import Relude
import Lucid (Html, ToHtml (..))
import Servant (AuthProtect, HasStatus (..), Header, Headers, ServerError, StdMethod, UVerb, Union, Verb, addHeader, noHeader, respond)
import Servant.HTML.Lucid (HTML)
import Servant qualified
import Servant.Htmx (HXRedirect, HXTrigger)
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import System.Tracing (Tracing)
import System.Tracing qualified as Tracing
import Web.Cookie (SetCookie)


-- a map of events to be triggered on the clientside.
-- https://htmx.org/headers/hx-trigger/
type TriggerEvents = Map Text [AE.Value]


type HXRedirectDest = Maybe Text


newtype WidgetJSON = WidgetJSON {unWidgetJSON :: Text}
  deriving stock (Show)


type XWidgetJSON = Maybe WidgetJSON
newtype HXReswap = HXReswap (Maybe Text)


type CommonWebEffects =
  '[ Artifacts.AgentArtifacts
   , ELLM.LLM
   , Effectful.Reader.Static.Reader AuthContext
   , UUIDEff
   , Sleep
   , HTTP
   , Hasql
   , Labeled "timefusion" Hasql
   , Time
   , Log
   , Tracing
   , Concurrent
   , Ki.StructuredConcurrency
   , Timeout
   , Error ServerError
   , Effectful.IOE
   ]


type ProjectToolCtx es =
  ( Artifacts.AgentArtifacts :> es
  , ELLM.LLM :> es
  , Effectful.Reader.Static.Reader AuthContext :> es
  , UUIDEff :> es
  , HTTP :> es
  , Sleep :> es
  , Hasql :> es
  , Labeled "timefusion" Hasql :> es
  , Time :> es
  , Log :> es
  , Tracing :> es
  , Concurrent :> es
  , Ki.StructuredConcurrency :> es
  , Timeout :> es
  , IOE :> es
  )


type ATBaseCtx = Effectful.Eff CommonWebEffects


type ATAuthCtx =
  Effectful.Eff
    ( Data.Effectful.Notify.Notify
        ': State.State TriggerEvents
        ': State.State HXRedirectDest
        ': State.State XWidgetJSON
        ': State.State HXReswap
        ': Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
        ': CommonWebEffects
    )


-- | Whether reads go to TimeFusion rather than the legacy Postgres store.
useTfReads :: Effectful.Reader.Static.Reader AuthContext :> es => Eff es Bool
useTfReads = (.env.enableTimefusionReads) <$> Effectful.Reader.Static.ask @AuthContext


atAuthToBase :: Headers '[Header "Set-Cookie" SetCookie] Sessions.Session -> ATAuthCtx a -> ATBaseCtx a
atAuthToBase sessionWithCookies page =
  page
    & Data.Effectful.Notify.runNotifyProduction
    & State.evalState Map.empty -- TriggerEvents
    & State.evalState Nothing -- HXRedirectDest
    & State.evalState Nothing -- XWidgetJSON
    & State.evalState (HXReswap Nothing)
    & Effectful.Reader.Static.runReader sessionWithCookies


-- | Test variant that captures notifications in provided IORef
atAuthToBaseTest :: IORef [Data.Effectful.Notify.Notification] -> Headers '[Header "Set-Cookie" SetCookie] Sessions.Session -> ATAuthCtx a -> ATBaseCtx a
atAuthToBaseTest notifRef sessionWithCookies page =
  page
    & Data.Effectful.Notify.runNotifyTest notifRef
    & State.evalState Map.empty -- TriggerEvents
    & State.evalState Nothing -- HXRedirectDest
    & State.evalState Nothing -- XWidgetJSON
    & State.evalState (HXReswap Nothing)
    & Effectful.Reader.Static.runReader sessionWithCookies


-- | `effToServantHandler` for live services
effToServantHandler :: AuthContext -> Log.Logger -> TracerProvider -> ATBaseCtx a -> Servant.Handler a
effToServantHandler env logger tp app =
  app
    & Artifacts.runAgentArtifactsReal env
    & ELLM.runLLMReal
    & Effectful.Reader.Static.runReader env
    & runUUID
    & runSleep
    & runHTTPWreq
    & runHasqlPool env.hasqlPool
    & runLabeled @"timefusion" (runHasqlPool env.hasqlTimefusionPool)
    & runTime
    & Logging.runLog (show env.config.environment) logger env.config.logLevel
    & Tracing.runTracing tp
    & runConcurrent
    & Ki.runStructuredConcurrency
    & runTimeout
    & effToHandler


-- | `effToServantHandler` exists specifically to be used in tests,
-- so the UUID effect is fixed to a deterministic pool. The IORef holds a
-- pre-populated pool of deterministic UUIDs shared across multiple handler
-- invocations in the same test (so creates in a loop get distinct ids).
-- The 'TestClock' drives the 'Time' effect and is shared across handler
-- invocations so tests can advance time between calls.
effToServantHandlerTest :: TestClock -> IORef [UUID.UUID] -> AuthContext -> Log.Logger -> TracerProvider -> ATBaseCtx a -> Servant.Handler a
effToServantHandlerTest = effToServantHandlerTestHTTP (runHTTPGolden "./test/golden/")


-- | 'effToServantHandlerTest' with a caller-chosen HTTP interpreter, e.g.
-- 'Data.Effectful.Wreq.runHTTPRecord' to assert on outgoing request payloads.
effToServantHandlerTestHTTP :: (forall x hes. IOE :> hes => Eff (HTTP ': hes) x -> Eff hes x) -> TestClock -> IORef [UUID.UUID] -> AuthContext -> Log.Logger -> TracerProvider -> ATBaseCtx a -> Servant.Handler a
effToServantHandlerTestHTTP http clock uuidRef env logger tp app =
  app
    & Artifacts.runAgentArtifactsTest (const $ Left Artifacts.StorageUnavailable) (\_ _ -> Left Artifacts.ArtifactUnavailable)
    & ELLM.runLLMGolden "./test/golden/"
    & Effectful.Reader.Static.runReader env
    & runStaticUUIDRef uuidRef
    & runNoSleep
    & http
    & runHasqlPoolSynced clock env.hasqlPool
    & runLabeled @"timefusion" (runHasqlPool env.hasqlTimefusionPool)
    & runMutableTime clock
    & Logging.runLog (show env.config.environment) logger env.config.logLevel
    & Tracing.runTracing tp
    & runConcurrent
    & Ki.runStructuredConcurrency
    & runTimeout
    & effToHandler


effToHandler
  :: forall (a :: Type)
   . ()
  => Eff '[Error ServerError, IOE] a
  -> Servant.Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
  either Except.throwError pure v


-- Effect list factored out so consumers (e.g. Pkg.Queue's Kafka loop) can layer
-- extra effects on top: @Eff (KafkaConsumer : Error e : ATBackgroundEffects)@.
type ATBackgroundEffects :: [Effect]
type ATBackgroundEffects =
  '[ Data.Effectful.Notify.Notify
   , Effectful.Reader.Static.Reader AuthContext
   , Hasql
   , Labeled "timefusion" Hasql
   , Time
   , Log
   , Tracing
   , UUIDEff
   , Sleep
   , HTTP
   , ELLM.LLM
   , CodeReview.CodeReview
   , Workspace.Workspace
   , Artifacts.AgentArtifacts
   , Concurrent
   , Ki.StructuredConcurrency
   , Timeout
   , Effectful.IOE
   ]


type ATBackgroundCtx :: Type -> Type
type ATBackgroundCtx = Effectful.Eff ATBackgroundEffects


runBackground :: Log.Logger -> AuthContext -> TracerProvider -> ATBackgroundCtx a -> IO a
runBackground logger appCtx tp process =
  process
    & Data.Effectful.Notify.runNotifyProduction
    & Effectful.Reader.Static.runReader appCtx
    & runHasqlPool appCtx.hasqlPool
    & runLabeled @"timefusion" (runHasqlPool appCtx.hasqlTimefusionPool)
    & runTime
    & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger appCtx.config.logLevel
    & Tracing.runTracing tp
    & runUUID
    & runSleep
    & runHTTPWreq
    & ELLM.runLLMReal
    & CodeReview.runCodeReviewReal (CodeReview.ArtifactStore <$> appCtx.config.impactReviewUsageDir <*> appCtx.config.impactReviewCacheDir) (sequenceA $ CodeReview.workerHost (map toString appCtx.config.impactReviewSkills) appCtx.config.impactReviewPrepareScript appCtx.config.impactReviewDependencyArchive <$> appCtx.config.impactReviewGuestImage <*> appCtx.config.impactReviewAgent) appCtx.config.impactReviewAgent (map toString appCtx.config.impactReviewSkills)
    & Workspace.runWorkspaceReal
    & Artifacts.runAgentArtifactsReal appCtx
    & runConcurrent
    & Ki.runStructuredConcurrency
    & runTimeout
    & Effectful.runEff


type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)


-- | How an @/api/v1@ request reached its project: with a key or session, or through
-- the world-readable demo bypass, which must never be handed credentials.
data ApiPrincipal = Authenticated {projectId :: Sessions.ProjectId} | DemoReader {projectId :: Sessions.ProjectId}
  deriving stock (Eq, Show)


type instance AuthServerData (AuthProtect "api-key-auth") = ApiPrincipal


type RespHeaders =
  Headers
    '[ HXTrigger
     , HXRedirect
     , Header "X-Widget-JSON" Text
     , Header "HX-Reswap" Text
     ]


addRespHeaders :: (State.State HXRedirectDest :> es, State.State HXReswap :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es) => a -> Eff es (RespHeaders a)
addRespHeaders resp = do
  triggerEvents <- State.get @TriggerEvents
  redirectDest <- State.get @HXRedirectDest
  widgetJSON <- State.get @XWidgetJSON
  HXReswap reswap <- State.get @HXReswap
  -- HTMX 4 passes object payloads through unchanged, including arrays.
  pure
    $ addHeader (decodeUtf8 $ AE.encode $ Map.map (\values -> AE.object ["value" AE..= values]) triggerEvents)
    $ maybe noHeader addHeader redirectDest
    $ maybe noHeader (\w -> addHeader w.unWidgetJSON) widgetJSON
    $ maybe noHeader addHeader reswap resp


-- redirectCS adds a header to the request, which in turn triggers a client side redirect via HTMX redirect header.
redirectCS :: State.State HXRedirectDest :> es => Text -> Eff es ()
redirectCS txt = State.modify $ \_ -> Just txt


addTriggerEvent :: State.State TriggerEvents :> es => Text -> AE.Value -> Eff es ()
addTriggerEvent key value = State.modify $ \events ->
  let updatedList = case Map.lookup key events of
        Just values -> values ++ [value] -- If the key exists, append the new value to the list
        Nothing -> [value] -- If the key doesn't exist, start a new list with the value
   in Map.insert key updatedList events


addToast :: State.State TriggerEvents :> es => Text -> Text -> Maybe Text -> Eff es ()
addToast toastType title descM = addTriggerEvent "triggerToast" $ AE.toJSON $ [toastType, title] ++ catMaybes [descM]


addSuccessToast :: State.State TriggerEvents :> es => Text -> Maybe Text -> Eff es ()
addSuccessToast = addToast "success"


addReswap :: State.State HXReswap :> es => Text -> Eff es ()
addReswap x = State.put $ HXReswap $ Just x


addErrorToast :: (IOE :> es, Log :> es, State.State TriggerEvents :> es) => Text -> Maybe Text -> Eff es ()
addErrorToast msg msg2 = do
  Logging.logAttention_ $ "ERROR: " <> msg <> " => " <> maybeToMonoid msg2
  addToast "error" msg msg2


-- | Toast an error and short-circuit with the given response payload.
-- Collapses the recurrent `addErrorToast msg Nothing >> addRespHeaders payload` pattern.
toastError
  :: (IOE :> es, Log :> es, State.State HXRedirectDest :> es, State.State HXReswap :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es)
  => Text -> a -> Eff es (RespHeaders a)
toastError msg payload = addErrorToast msg Nothing *> addRespHeaders payload


-- Add widget JSON for inclusion in response headers
addWidgetJSON :: State.State XWidgetJSON :> es => Text -> Eff es ()
addWidgetJSON widgetJson = State.modify $ \_ -> Just (WidgetJSON widgetJson)


-- * Typed answers to browser actions
--
-- A form or button action answers with a domain value instead of opaque HTML. Its route
-- lists every alternative ('Action' = 200 'Done' outcome | 422 'Refused' reason), so the
-- API type, tests and handler signature all name what the endpoint does and how it can fail.
-- 'Outcome' says how a success is presented (body, redirect, toast); 'Refusal' says what a
-- refusal tells the user. Handlers build answers with 'act' (may be refused) or 'done'.


-- | The action succeeded; the body is the outcome's 'outcomeBody'.
newtype Done a = Done a


instance Outcome a => ToHtml (Done a) where
  toHtml (Done a) = toHtml $ outcomeBody a
  toHtmlRaw = toHtml


instance HasStatus (Done a) where type StatusOf (Done a) = 200


-- | The request was understood but refused: 422, the reason as an error toast, and no swap, so the form keeps its input.
newtype Refused e = Refused e


instance ToHtml (Refused e) where
  toHtml _ = mempty
  toHtmlRaw _ = mempty


instance HasStatus (Refused e) where type StatusOf (Refused e) = 422


-- | Expected, user-correctable failures, one constructor per case.
class Refusal e where
  refusalMessage :: e -> Text
  refusalDetail :: e -> Maybe Text
  refusalDetail = const Nothing


data ToastLevel = SuccessToast | ErrorToast
  deriving stock (Eq, Show)


data Toast = Toast ToastLevel Text (Maybe Text)
  deriving stock (Eq, Show)


-- | A literal is a success toast with no detail.
instance IsString Toast where fromString = successToast . toText


successToast :: Text -> Toast
successToast t = Toast SuccessToast t Nothing


-- | How a successful outcome is presented: its body, where the browser goes next, and what it is told.
class Outcome a where
  outcomeBody :: a -> Html ()
  outcomeBody = const mempty
  outcomeRedirect :: a -> Maybe Text
  outcomeRedirect = const Nothing
  outcomeToast :: a -> Maybe Toast
  outcomeToast = const Nothing


-- | An outcome that is just its rendering: @deriving (Outcome) via Rendered T@.
newtype Rendered a = Rendered a


instance ToHtml a => Outcome (Rendered a) where outcomeBody (Rendered a) = toHtml a


type Action :: StdMethod -> Type -> Type -> Type
type Action method e a = UVerb method '[HTML] '[RespHeaders (Done a), RespHeaders (Refused e)]
type Unrefusable :: StdMethod -> Type -> Type
type Unrefusable method a = Verb method 200 '[HTML] (RespHeaders (Done a))
type ActionResult :: Type -> Type -> Type
type ActionResult e a = Union '[RespHeaders (Done a), RespHeaders (Refused e)]


type AnswerState es = (State.State HXRedirectDest :> es, State.State HXReswap :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es)


-- | Answer with an outcome that cannot be refused.
done :: (AnswerState es, Outcome a) => a -> Eff es (RespHeaders (Done a))
done a = do
  traverse_ redirectCS (outcomeRedirect a)
  for_ (outcomeToast a) \(Toast level title detail) -> addToast (case level of SuccessToast -> "success"; ErrorToast -> "error") title detail
  addRespHeaders (Done a)


-- | Run an action that may be refused.
act :: (AnswerState es, Outcome a, Refusal e) => Eff es (Either e a) -> Eff es (ActionResult e a)
act run =
  run >>= \case
    Left e -> addToast "error" (refusalMessage e) (refusalDetail e) >> addReswap "none" >> (respond =<< addRespHeaders (Refused e))
    Right a -> respond =<< done a
