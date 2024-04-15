module Network.Wai.Middleware.Heartbeat (heartbeatMiddleware) where

import Network.HTTP.Types
import Network.Wai
import Prelude


heartbeatMiddleware :: Middleware
heartbeatMiddleware app req sendResponse =
  case rawPathInfo req of
    "/heartbeat" ->
      if getVerb
        then heartbeat
        else app req sendResponse
    _ -> app req sendResponse
  where
    getVerb = (requestMethod req == methodGet) || (requestMethod req == methodHead)
    heartbeat = sendResponse $ responseLBS status200 [("Content-Type", "text/plain")] "Ok"
