{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module System.Types (
  ATBaseCtx,
  ATAuthCtx,
  ATBackgroundCtx,
  runBackground,
) where

import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Database.PostgreSQL.Simple
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Time (Time)
import Data.Pool as Pool (Pool)
import Models.Users.Sessions qualified as Sessions
import Relude
import Servant (AuthProtect, Header, Headers, ServerError)
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Config
import Effectful.PostgreSQL.Transact.Effect (runDB)
import System.Logging qualified as Logging
import Web.Cookie
import Effectful.Time (runTime)
import Log qualified


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

runBackground :: Log.Logger -> AuthContext-> ATBackgroundCtx a -> IO a
runBackground logger appCtx process =
   process 
    & Effectful.Reader.Static.runReader appCtx 
    & runDB appCtx.jobsPool
    & runTime
    & Logging.runLog ("background-job:"<> show appCtx.config.environment) logger
    & runEff


type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
