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
  HXRedirectDest,
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
import Data.Effectful.UUID (UUIDEff, runStaticUUID, runUUID)
import Data.Effectful.Wreq (HTTP, runHTTPGolden, runHTTPWreq)
import Data.Map qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID qualified as UUID
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.State.Static.Local qualified as State
import Effectful.Time (Time, runFrozenTime, runTime)
import Log qualified
import Models.Users.Sessions qualified as Sessions
import Relude
import Servant (AuthProtect, Header, Headers, ServerError, addHeader, noHeader)
import Servant qualified
import Servant.Htmx (HXRedirect, HXTriggerAfterSettle)
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import Web.Cookie (SetCookie)


-- a map of events to be triggered on the clientside.
-- https://htmx.org/headers/hx-trigger/
type TriggerEvents = Map Text [AE.Value]


type HXRedirectDest = Maybe Text


type CommonWebEffects =
  '[ Effectful.Reader.Static.Reader AuthContext
   , UUIDEff
   , HTTP
   , DB
   , Time
   , Log
   , Error ServerError
   , Effectful.IOE
   ]


type ATBaseCtx = Effectful.Eff CommonWebEffects


type ATAuthCtx =
  Effectful.Eff
    ( State.State TriggerEvents
        ': State.State HXRedirectDest
        ': Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
        ': CommonWebEffects
    )


atAuthToBase :: Headers '[Header "Set-Cookie" SetCookie] Sessions.Session -> ATAuthCtx a -> ATBaseCtx a
atAuthToBase sessionWithCookies page =
  page
    & State.evalState Map.empty
    & State.evalState Nothing
    & Effectful.Reader.Static.runReader sessionWithCookies


-- | `effToServantHandler` for live services
effToServantHandler :: AuthContext -> Log.Logger -> ATBaseCtx a -> Servant.Handler a
effToServantHandler env logger app =
  app
    & Effectful.Reader.Static.runReader env
    & runUUID
    & runHTTPWreq
    & runDB env.pool
    & runTime
    & Logging.runLog (show env.config.environment) logger
    & effToHandler


-- | `effToServantHandler` exists specifically to be used in tests,
-- so the UUID and Time effects are fixed to constants.
effToServantHandlerTest :: AuthContext -> Log.Logger -> ATBaseCtx a -> Servant.Handler a
effToServantHandlerTest env logger app =
  app
    & Effectful.Reader.Static.runReader env
    & (runStaticUUID $ map (UUID.fromWords 0 0 0) [1 .. 10])
    & runHTTPGolden "./golden/"
    & runDB env.pool
    & runFrozenTime (posixSecondsToUTCTime 0)
    & Logging.runLog (show env.config.environment) logger
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
    '[ Effectful.Reader.Static.Reader AuthContext
     , DB
     , Time
     , Log
     , Effectful.IOE
     ]


runBackground :: Log.Logger -> AuthContext -> ATBackgroundCtx a -> IO a
runBackground logger appCtx process =
  process
    & Effectful.Reader.Static.runReader appCtx
    & runDB appCtx.pool
    & runTime
    & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
    & Effectful.runEff


type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)


type RespHeaders =
  Headers
    '[ HXTriggerAfterSettle
     , HXRedirect
     ]


addRespHeaders :: a -> ATAuthCtx (RespHeaders (a))
addRespHeaders resp = do
  triggerEvents <- State.get @TriggerEvents
  redirectDest <- State.get @HXRedirectDest
  pure $ addHeader (decodeUtf8 $ AE.encode triggerEvents) $ maybe noHeader addHeader redirectDest $ resp


-- redirectCS adds a header to the request, which in turn triggers a client side redirect via HTMX redirect header.
redirectCS :: Text -> ATAuthCtx ()
redirectCS txt = State.modify $ \_ -> Just txt


addTriggerEvent :: Text -> AE.Value -> ATAuthCtx ()
addTriggerEvent key value = State.modify $ \events ->
  let updatedList = case Map.lookup key events of
        Just values -> values ++ [value] -- If the key exists, append the new value to the list
        Nothing -> [value] -- If the key doesn't exist, start a new list with the value
   in Map.insert key updatedList events


addToast :: Text -> Text -> Maybe Text -> ATAuthCtx ()
addToast toastType title descM = addTriggerEvent ("triggerToast") $ AE.toJSON $ [toastType, title] ++ catMaybes [descM]


addSuccessToast :: Text -> Maybe Text -> ATAuthCtx ()
addSuccessToast = addToast "success"


addErrorToast :: Text -> Maybe Text -> ATAuthCtx ()
addErrorToast = addToast "error"
