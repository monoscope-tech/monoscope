{-# LANGUAGE RankNTypes #-}

module Web.Error (
  renderError,
)
where

import Effectful
import Effectful.Error.Static (Error, throwError)
import Network.HTTP.Types.Status
import Relude
import Servant (ServerError (..))
import System.Config


renderError :: forall (es :: [Effect]) (a :: Type). Error ServerError :> es => AuthContext -> Status -> Eff es a
renderError _env status = throwError $ ServerError{errHTTPCode = statusCode status, errBody = "error page: " <> show @LByteString status, errReasonPhrase = "", errHeaders = []}
