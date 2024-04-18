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

import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, runDB)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Time (Time, runTime)
import Log qualified
import Models.Users.Sessions qualified as Sessions
import Relude (IO, Semigroup ((<>)), Type, show, (&))
import Servant (AuthProtect, Header, Headers, ServerError)
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Config (
  AuthContext (config, jobsPool),
  EnvConfig (environment),
 )
import System.Logging qualified as Logging
import Web.Cookie (SetCookie)


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
    '[ Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
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
