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
) where

import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error)
import Data.Aeson qualified as AE
import Effectful.Log (Log)
import Data.Map qualified as Map
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Time (Time, runTime)
import Log qualified
import Models.Users.Sessions qualified as Sessions
import Relude (IO, Semigroup ((<>)), Type, show, (&), Text, Maybe(..), Map, ($), decodeUtf8, pure, maybe, (++), catMaybes) 
import Servant (AuthProtect, Header, Headers, ServerError, addHeader, noHeader)
import Servant.Server.Experimental.Auth (AuthServerData)
import Effectful.State.Static.Local qualified as State
import System.Config (
  AuthContext (config, jobsPool),
  EnvConfig (environment),
 )
import System.Logging qualified as Logging
import Web.Cookie (SetCookie)
import Servant.Htmx (HXRedirect, HXTriggerAfterSettle)



-- a map of events to be triggered on the clientside.
-- https://htmx.org/headers/hx-trigger/
type TriggerEvents = Map Text [AE.Value]


type HXRedirectDest = Maybe Text




type ATBaseCtx :: Type -> Type
type ATBaseCtx =
  Effectful.Eff
    '[ Effectful.Reader.Static.Reader AuthContext
     , DB
     , Time
     , Log
     , Error ServerError
     , Effectful.IOE
     ]


type ATAuthCtx :: Type -> Type
type ATAuthCtx =
  Effectful.Eff
    '[ State.State TriggerEvents
     , State.State HXRedirectDest
     , Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
     , Effectful.Reader.Static.Reader AuthContext
     , DB
     , Time
     , Log
     , Error ServerError
     , Effectful.IOE
     ]


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
    & runDB appCtx.jobsPool
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

