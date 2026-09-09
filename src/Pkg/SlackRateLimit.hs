module Pkg.SlackRateLimit (SlackRateLimited (..), withRateLimits) where

import Control.Exception (ErrorCall (..))
import Control.Lens ((?~), (^.))
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as HTTP
import Data.Text qualified as T
import Data.Time (UTCTime)
import Effectful (Eff, type (:>))
import Effectful.Dispatch.Dynamic (interpose, send)
import Hasql.Interpolate qualified as HI
import Network.HTTP.Types (statusCode, statusIsSuccessful)
import Network.Wreq qualified as Wreq
import Relude
import System.Types (DB)
import UnliftIO.Exception (throwIO)


newtype SlackRateLimited = SlackRateLimited UTCTime
  deriving stock (Show)
  deriving anyclass (Exception)


-- | Cooldowns are shared by workers and survive process restarts. Non-Slack
-- requests retain their original transport options and error handling.
withRateLimits :: (DB es, HTTP.HTTP :> es) => Text -> Eff es a -> Eff es a
withRateLimits workspace = interpose @HTTP.HTTP $ \_ request -> case request of
  HTTP.PostWith options url body -> limited workspace options url $ \opts -> send $ HTTP.PostWith opts url body
  HTTP.GetWith options url -> limited workspace options url $ \opts -> send $ HTTP.GetWith opts url
  _ -> send @HTTP.HTTP $ coerce request


limited :: DB es => Text -> HTTP.Options -> String -> (HTTP.Options -> Eff es (HTTP.Response LByteString)) -> Eff es (HTTP.Response LByteString)
limited workspace options url request = case T.stripPrefix "https://slack.com/api/" $ toText url of
  Nothing -> request options
  Just path -> do
    let method = T.takeWhile (/= '?') path
    deadline <-
      Hasql.interpOne @(HI.OneColumn UTCTime)
        [HI.sql|SELECT retry_at FROM apis.slack_api_cooldowns
        WHERE team_id = #{workspace} AND method = #{method} AND retry_at > clock_timestamp()|]
    for_ deadline $ \(HI.OneColumn retryAt) -> throwIO $ SlackRateLimited retryAt
    response <- request $ options & Wreq.checkResponse ?~ (\_ _ -> pass)
    if statusCode (response ^. Wreq.responseStatus) == 429
      then do
        let seconds = max 1 $ fromMaybe 60 $ readMaybe @Int $ toString $ decodeUtf8 @Text $ response ^. Wreq.responseHeader "Retry-After"
        saved <-
          Hasql.interpOne @(HI.OneColumn UTCTime)
            [HI.sql|INSERT INTO apis.slack_api_cooldowns (team_id, method, retry_at)
            VALUES (#{workspace}, #{method}, clock_timestamp() + #{seconds} * interval '1 second')
            ON CONFLICT (team_id, method) DO UPDATE
              SET retry_at = GREATEST(slack_api_cooldowns.retry_at, EXCLUDED.retry_at)
            RETURNING retry_at|]
        for_ saved $ \(HI.OneColumn retryAt) -> throwIO $ SlackRateLimited retryAt
        throwIO $ ErrorCall "Saving the Slack rate-limit deadline returned no row"
      else do
        unless (statusIsSuccessful $ response ^. Wreq.responseStatus) $ throwIO $ ErrorCall "Slack returned an unsuccessful HTTP status"
        pure response
