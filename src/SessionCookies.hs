module SessionCookies (craftSessionCookie, emptySessionCookie) where

import Data.UUID qualified as UUID
import Models.Users.Sessions qualified as Sessions
import Relude
import Web.Cookie

-- | This function builds a cookie with the provided content
craftSessionCookie ::
  -- | Cookie content
  Sessions.PersistentSessionId ->
  -- | Remember the cookie for 1 week
  Bool ->
  SetCookie
craftSessionCookie (Sessions.PersistentSessionId content) rememberSession =
  defaultSetCookie
    { setCookieValue = UUID.toASCIIBytes content
    , setCookieName = "apitoolkit_session"
    , setCookiePath = Just "/"
    , setCookieHttpOnly = True
    , setCookieSameSite = Just sameSiteLax
    , setCookieMaxAge = if rememberSession then Just 604800 else Nothing
    , setCookieSecure = True
    }

emptySessionCookie :: SetCookie
emptySessionCookie =
  defaultSetCookie
    { setCookieName = "apitoolkit_session"
    , setCookieValue = ""
    , setCookieMaxAge = Just 0
    }
