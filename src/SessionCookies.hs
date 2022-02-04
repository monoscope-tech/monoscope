module SessionCookies where

import qualified Data.UUID as UUID
import qualified Models.Users.Sessions as Sessions
import Relude
import Servant (Header, Headers, addHeader)
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
    { setCookieValue = UUID.toASCIIBytes content,
      setCookieName = "apitoolkit_session",
      setCookiePath = Just "/",
      setCookieHttpOnly = True,
      setCookieSameSite = Just sameSiteStrict,
      setCookieMaxAge = if rememberSession then Just 604800 else Nothing,
      setCookieSecure = True
    }

emptySessionCookie :: SetCookie
emptySessionCookie =
  defaultSetCookie
    { setCookieName = "apitoolkit_session",
      setCookieValue = "",
      setCookieMaxAge = Just 0
    }

addCookie ::
  SetCookie ->
  a ->
  Headers '[Header "Set-Cookie" SetCookie] a
addCookie cookies continuation = addHeader cookies continuation

deleteCookie :: a -> Headers '[Header "Set-Cookie" SetCookie] a
deleteCookie continuation = addHeader emptySessionCookie continuation
