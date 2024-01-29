{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module System.Types (
  ATBaseCtx,
  ATAuthCtx,
) where

import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import Models.Users.Sessions qualified as Sessions
import Relude
import Servant (AuthProtect, Header, Headers, ServerError)
import Servant.Server.Experimental.Auth (AuthServerData)
import System.Config
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


type instance
  AuthServerData (AuthProtect "optional-cookie-auth") =
    (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
