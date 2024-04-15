{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module System.Types (
  ATBaseCtx,
  ATAuthCtx,
  ATBackgroundCtx,
  runBackground,
) where

import Data.Pool as Pool (Pool)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Time (Time, runTime)
import Log qualified
import Models.Users.Sessions qualified as Sessions
import Prelude
import Servant (AuthProtect, Header, Headers, ServerError)
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Config
import System.Logging qualified as Logging
import Web.Cookie


type ATBaseCtx :: Type -> Type
type ATBaseCtx =
  Eff
    '[ Effectful.Reader.Static.Reader AuthContext
     , DB
     , Time
     , Log
     , Error ServerError
     , IOE
     ]


type ATAuthCtx :: Type -> Type
type ATAuthCtx =
  Eff
    '[ Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
     , Effectful.Reader.Static.Reader AuthContext
     , DB
     , Time
     , Log
     , Error ServerError
     , IOE
     ]


type ATBackgroundCtx :: Type -> Type
type ATBackgroundCtx =
  Eff
    '[ Effectful.Reader.Static.Reader AuthContext
     , DB
     , Time
     , Log
     , IOE
     ]


runBackground :: Log.Logger -> AuthContext -> ATBackgroundCtx a -> IO a
runBackground logger appCtx process =
  process
    & Effectful.Reader.Static.runReader appCtx
    & runDB appCtx.jobsPool
    & runTime
    & Logging.runLog ("background-job:" <> show appCtx.config.environment) logger
    & runEff


type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
