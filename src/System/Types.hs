module System.Types (
  ATBaseCtx,
  ATAuthCtx,
  ATBackgroundCtx,
  DB,
  runBackground,
  withTimefusion,
  addRespHeaders,
  addTriggerEvent,
  addToast,
  addSuccessToast,
  addErrorToast,
  addWidgetJSON,
  addReswap,
  HXRedirectDest,
  XWidgetJSON,
  TriggerEvents,
  RespHeaders,
  redirectCS,
  effToServantHandler,
  effToServantHandlerTest,
  effToHandler,
  atAuthToBase,
  atAuthToBaseTest,
)
where

import Control.Monad.Except qualified as Except
import Data.Aeson qualified as AE
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.Notify qualified
import Data.Effectful.UUID (UUIDEff, runStaticUUID, runUUID)
import Data.Effectful.Wreq (HTTP, runHTTPGolden, runHTTPWreq)
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Effectful
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled, labeled, runLabeled)
import Effectful.Log (Log)
import Effectful.PostgreSQL (WithConnection, runWithConnectionPool)
import Effectful.Reader.Static qualified
import Effectful.State.Static.Local qualified as State
import Effectful.Time (Time, runTime)
import Pkg.TestClock (TestClock, runMutableTime, runWithTimeSyncedPool)
import Log qualified
import Models.Users.Sessions qualified as Sessions
import OpenTelemetry.Trace (TracerProvider)
import Pkg.DeriveUtils (DB)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (AuthProtect, Header, Headers, ServerError, addHeader, noHeader)
import Servant qualified
import Servant.Htmx (HXRedirect, HXTriggerAfterSettle)
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
type HXReswap = Maybe Text


type CommonWebEffects =
  '[ ELLM.LLM
   , Effectful.Reader.Static.Reader AuthContext
   , UUIDEff
   , HTTP
   , WithConnection
   , Labeled "timefusion" WithConnection
   , Time
   , Log
   , Tracing
   , Concurrent
   , Ki.StructuredConcurrency
   , Error ServerError
   , Effectful.IOE
   ]


type ATBaseCtx = Effectful.Eff CommonWebEffects


type ATAuthCtx =
  Effectful.Eff
    ( Data.Effectful.Notify.Notify
        ': State.State TriggerEvents
        ': State.State HXRedirectDest
        ': State.State XWidgetJSON
        ': Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
        ': CommonWebEffects
    )


atAuthToBase :: Headers '[Header "Set-Cookie" SetCookie] Sessions.Session -> ATAuthCtx a -> ATBaseCtx a
atAuthToBase sessionWithCookies page =
  page
    & Data.Effectful.Notify.runNotifyProduction
    & State.evalState Map.empty -- TriggerEvents
    & State.evalState Nothing -- HXRedirectDest
    & State.evalState Nothing -- XWidgetJSON
    & Effectful.Reader.Static.runReader sessionWithCookies


-- | Test variant that captures notifications in provided IORef
atAuthToBaseTest :: IORef [Data.Effectful.Notify.Notification] -> Headers '[Header "Set-Cookie" SetCookie] Sessions.Session -> ATAuthCtx a -> ATBaseCtx a
atAuthToBaseTest notifRef sessionWithCookies page =
  page
    & Data.Effectful.Notify.runNotifyTest notifRef
    & State.evalState Map.empty -- TriggerEvents
    & State.evalState Nothing -- HXRedirectDest
    & State.evalState Nothing -- XWidgetJSON
    & Effectful.Reader.Static.runReader sessionWithCookies


-- | `effToServantHandler` for live services
effToServantHandler :: AuthContext -> Log.Logger -> TracerProvider -> ATBaseCtx a -> Servant.Handler a
effToServantHandler env logger tp app =
  app
    & ELLM.runLLMReal
    & Effectful.Reader.Static.runReader env
    & runUUID
    & runHTTPWreq
    & runWithConnectionPool env.pool
    & runLabeled @"timefusion" (runWithConnectionPool env.timefusionPgPool)
    & runTime
    & Logging.runLog (show env.config.environment) logger env.config.logLevel
    & Tracing.runTracing tp
    & runConcurrent
    & Ki.runStructuredConcurrency
    & effToHandler


-- | `effToServantHandler` exists specifically to be used in tests,
-- so the UUID and Time effects are fixed to constants.
-- Accepts a 'TestClock' for mutable, advanceable time in tests.
effToServantHandlerTest :: AuthContext -> Log.Logger -> TracerProvider -> TestClock -> ATBaseCtx a -> Servant.Handler a
effToServantHandlerTest env logger tp testClock app =
  app
    & ELLM.runLLMGolden "./tests/golden/"
    & Effectful.Reader.Static.runReader env
    & runStaticUUID (map (UUID.fromWords 0 0 0) [1 .. 1000])
    & runHTTPGolden "./tests/golden/"
    & runWithTimeSyncedPool testClock env.pool
    & runLabeled @"timefusion" (runWithConnectionPool env.timefusionPgPool)
    & runMutableTime testClock
    & Logging.runLog (show env.config.environment) logger env.config.logLevel
    & Tracing.runTracing tp
    & runConcurrent
    & Ki.runStructuredConcurrency
    & effToHandler


effToHandler
  :: forall (a :: Type)
   . ()
  => Eff '[Error ServerError, IOE] a
  -> Servant.Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
  either Except.throwError pure v


type ATBackgroundCtx :: Type -> Type
type ATBackgroundCtx =
  Effectful.Eff
    '[ Data.Effectful.Notify.Notify
     , Effectful.Reader.Static.Reader AuthContext
     , WithConnection
     , Labeled "timefusion" WithConnection
     , Time
     , Log
     , Tracing
     , UUIDEff
     , HTTP
     , ELLM.LLM
     , Concurrent
     , Ki.StructuredConcurrency
     , Effectful.IOE
     ]


runBackground :: Log.Logger -> AuthContext -> TracerProvider -> ATBackgroundCtx a -> IO a
runBackground logger appCtx tp process =
  process
    & Data.Effectful.Notify.runNotifyProduction
    & Effectful.Reader.Static.runReader appCtx
    & runWithConnectionPool appCtx.pool
    & runLabeled @"timefusion" (runWithConnectionPool appCtx.timefusionPgPool)
    & runTime
    & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger appCtx.config.logLevel
    & Tracing.runTracing tp
    & runUUID
    & runHTTPWreq
    & ELLM.runLLMReal
    & runConcurrent
    & Ki.runStructuredConcurrency
    & Effectful.runEff


-- | Route a WithConnection action through the timefusion pool when enabled, otherwise use the main pool.
withTimefusion :: (DB es, Labeled "timefusion" WithConnection :> es) => Bool -> Eff (WithConnection ': es) a -> Eff es a
withTimefusion useTimefusion q = if useTimefusion then labeled @"timefusion" @WithConnection q else subsume q


type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)


type RespHeaders =
  Headers
    '[ HXTriggerAfterSettle
     , HXRedirect
     , Header "X-Widget-JSON" Text
     , Header "HX-Reswap" Text
     ]


addRespHeaders :: (State.State HXRedirectDest :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es) => a -> Eff es (RespHeaders a)
addRespHeaders resp = do
  triggerEvents <- State.get @TriggerEvents
  redirectDest <- State.get @HXRedirectDest
  widgetJSON <- State.get @XWidgetJSON
  reswap <- State.get @HXReswap
  pure
    $ addHeader (decodeUtf8 $ AE.encode triggerEvents)
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
addReswap x = State.modify $ \_ -> Just x


addErrorToast :: (IOE :> es, Log :> es, State.State TriggerEvents :> es) => Text -> Maybe Text -> Eff es ()
addErrorToast msg msg2 = do
  Logging.logAttention_ $ "ERROR: " <> msg <> " => " <> maybeToMonoid msg2
  addToast "error" msg msg2


-- Add widget JSON for inclusion in response headers
addWidgetJSON :: State.State XWidgetJSON :> es => Text -> Eff es ()
addWidgetJSON widgetJson = State.modify $ \_ -> Just (WidgetJSON widgetJson)
