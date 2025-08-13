{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module System.Types (
  ATBaseCtx,
  ATAuthCtx,
  ATBackgroundCtx,
  runBackground,
  addRespHeaders,
  addTriggerEvent,
  addToast,
  addSuccessToast,
  addErrorToast,
  addWidgetJSON,
  HXRedirectDest,
  XWidgetJSON,
  TriggerEvents,
  RespHeaders,
  redirectCS,
  effToServantHandler,
  effToServantHandlerTest,
  effToHandler,
  atAuthToBase,
)
where

import Control.Monad.Except qualified as Except
import Data.Aeson qualified as AE
import Data.Effectful.Notify qualified
import Data.Effectful.UUID (UUIDEff, runStaticUUID, runUUID)
import Data.Effectful.Wreq (HTTP, runHTTPGolden, runHTTPWreq)
import Data.Map qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Effectful
import Effectful.Concurrent.Async (Concurrent, runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Ki qualified as Ki
import Effectful.Labeled (Labeled, runLabeled)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local qualified as State
import Effectful.Time (Time, runFrozenTime, runTime)
import Log qualified
import Models.Users.Sessions qualified as Sessions
import OpenTelemetry.Trace (TracerProvider)
import Relude
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


type CommonWebEffects =
  '[ Effectful.Reader.Static.Reader AuthContext
   , UUIDEff
   , HTTP
   , DB
   , Labeled "timefusion" DB
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
    ( State.State TriggerEvents
        ': State.State HXRedirectDest
        ': State.State XWidgetJSON
        ': Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
        ': CommonWebEffects
    )


atAuthToBase :: Headers '[Header "Set-Cookie" SetCookie] Sessions.Session -> ATAuthCtx a -> ATBaseCtx a
atAuthToBase sessionWithCookies page =
  page
    & State.evalState Map.empty -- TriggerEvents
    & State.evalState Nothing -- HXRedirectDest
    & State.evalState Nothing -- XWidgetJSON
    & Effectful.Reader.Static.runReader sessionWithCookies


-- | `effToServantHandler` for live services
effToServantHandler :: AuthContext -> Log.Logger -> TracerProvider -> ATBaseCtx a -> Servant.Handler a
effToServantHandler env logger tp app =
  app
    & Effectful.Reader.Static.runReader env
    & runUUID
    & runHTTPWreq
    & runDB env.pool
    & runLabeled @"timefusion" (runDB env.timefusionPgPool)
    & runTime
    & Logging.runLog (show env.config.environment) logger
    & Tracing.runTracing tp
    & runConcurrent
    & Ki.runStructuredConcurrency
    & effToHandler


-- | `effToServantHandler` exists specifically to be used in tests,
-- so the UUID and Time effects are fixed to constants.
effToServantHandlerTest :: AuthContext -> Log.Logger -> TracerProvider -> ATBaseCtx a -> Servant.Handler a
effToServantHandlerTest env logger tp app =
  app
    & Effectful.Reader.Static.runReader env
    & runStaticUUID (map (UUID.fromWords 0 0 0) [1 .. 10])
    & runHTTPGolden "./golden/"
    & runDB env.pool
    & runLabeled @"timefusion" (runDB env.timefusionPgPool)
    & runFrozenTime (posixSecondsToUTCTime 0)
    & Logging.runLog (show env.config.environment) logger
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
     , DB
     , Labeled "timefusion" DB
     , Time
     , Log
     , Tracing
     , UUIDEff
     , HTTP
     , Concurrent
     , Ki.StructuredConcurrency
     , Effectful.IOE
     ]


runBackground :: Log.Logger -> AuthContext -> TracerProvider -> ATBackgroundCtx a -> IO a
runBackground logger appCtx tp process =
  process
    & Data.Effectful.Notify.runNotifyProduction
    & Effectful.Reader.Static.runReader appCtx
    & runDB appCtx.pool
    & runLabeled @"timefusion" (runDB appCtx.timefusionPgPool)
    & runTime
    & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
    & Tracing.runTracing tp
    & runUUID
    & runHTTPWreq
    & runConcurrent
    & Ki.runStructuredConcurrency
    & Effectful.runEff


type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)


type RespHeaders =
  Headers
    '[ HXTriggerAfterSettle
     , HXRedirect
     , Header "X-Widget-JSON" Text
     ]


addRespHeaders :: (State.State HXRedirectDest :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es) => a -> Eff es (RespHeaders a)
addRespHeaders resp = do
  triggerEvents <- State.get @TriggerEvents
  redirectDest <- State.get @HXRedirectDest
  widgetJSON <- State.get @XWidgetJSON
  pure
    $ addHeader (decodeUtf8 $ AE.encode triggerEvents)
    $ maybe noHeader addHeader redirectDest
    $ maybe noHeader (\w -> addHeader w.unWidgetJSON) widgetJSON resp


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


addErrorToast :: (Log :> es, State.State TriggerEvents :> es) => Text -> Maybe Text -> Eff es ()
addErrorToast msg msg2 = do
  Log.logAttention_ $ "ERROR: " <> msg <> " => " <> maybeToMonoid msg2
  addToast "error" msg msg2


-- Add widget JSON for inclusion in response headers
addWidgetJSON :: State.State XWidgetJSON :> es => Text -> Eff es ()
addWidgetJSON widgetJson = State.modify $ \_ -> Just (WidgetJSON widgetJson)
