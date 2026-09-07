module Models.Telemetry.RUM (
  RumBucket (..),
  RumPage (..),
  RumError (..),
  RumBreakdown (..),
  ReplaySession (..),
  RumSession (..),
  SessionFilter (..),
  VitalSample (..),
  VitalTrendPoint (..),
  PageVitalPoint (..),
  RumQueryResult (..),
  RumQuery (..),
  RumCacheKey (..),
  rumPanelCacheGet,
  withSharedCache,
  rumPanelCacheSet,
) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (AesonText (..), DB)
import Relude
import Utils (toXXHash)


data RumBucket = FiveMinutes | OneHour | SixHours
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)


data RumPage = RumPage
  { path :: Text
  , views :: Int64
  , p75LoadMs :: Maybe Double
  , lastSeen :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data RumError = RumError
  { timestamp :: UTCTime
  , errorType :: Text
  , message :: Text
  , sessionId :: Maybe Text
  , userId :: Maybe Text
  , path :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


-- | One user agent's traffic. The user agent string is classified into browser, OS and
-- device family in the page layer; the query only groups and counts.
data RumBreakdown = RumBreakdown
  { userAgent :: Text
  , sessions :: Int64
  , views :: Int64
  , errors :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data ReplaySession = ReplaySession
  { id :: UUID.UUID
  , startedAt :: UTCTime
  , endedAt :: UTCTime
  , userId :: Maybe Text
  , userName :: Maybe Text
  , userEmail :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data SessionFilter = AllSessionRows | ErrorSessionRows | ReplaySessionRows
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)


data RumSession = RumSession
  { id :: Text
  , startedAt :: UTCTime
  , endedAt :: UTCTime
  , events :: Int64
  , errors :: Int64
  , views :: Int64
  , userId :: Maybe Text
  , userName :: Maybe Text
  , userEmail :: Maybe Text
  , service :: Maybe Text
  , lastPage :: Maybe Text
  , hasReplay :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data VitalTrendPoint = VitalTrendPoint
  { bucket :: UTCTime
  , metricName :: Text
  , p75 :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data PageVitalPoint = PageVitalPoint
  { page :: Text
  , metricName :: Text
  , p75 :: Double
  , samples :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data VitalSample = VitalSample
  { metricName :: Text
  , value :: Double
  , samples :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


data RumQueryResult
  = PresenceResult Bool
  | PagesResult [RumPage]
  | ErrorsResult [RumError]
  | SessionsResult [RumSession]
  | ReplaySessionsResult [ReplaySession]
  | SessionDetailResult (Maybe RumSession)
  | VitalSamplesResult [VitalSample]
  | VitalsDetailResult [VitalTrendPoint] [PageVitalPoint]
  | ServicesResult [Text]
  | BreakdownResult [RumBreakdown]
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data RumQuery
  = PresenceQuery
  | PagesQuery
  | ErrorsQuery
  | SessionsQuery
  | ReplaySessionsQuery
  | SessionSearchQuery (Maybe Text) SessionFilter
  | SessionDetailQuery Text
  | VitalSamplesQuery
  | VitalsDetailQuery RumBucket
  | ServicesQuery
  | BreakdownQuery
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)


-- | @service@ is part of the key, not a filter applied after it. Without it a page scoped to
-- one service would be served another service's cached rows — the cache would hand back
-- exactly the cross-team mixing the scope exists to prevent.
data RumCacheKey = RumCacheKey
  { project :: Projects.ProjectId
  , query :: RumQuery
  , environment :: Maybe Text
  , service :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , since :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)


-- | Shared L2 for expensive panel/stats results, keyed by the hashed memory-cache key.
-- The in-memory caches are per replica; without this layer every replica paid each cold
-- scan once per TTL (the vitals-detail scan alone costs ~25s), and the first visitor
-- after every expiry always ate one. Generic over the payload so the API catalog's host
-- and endpoint stats share the table with the RUM panels.
rumPanelCacheGet :: (AE.FromJSON a, DB es, Typeable a) => Text -> Eff es (Maybe a)
rumPanelCacheGet key =
  fmap (\(HI.OneColumn (AesonText value)) -> value)
    . listToMaybe
    <$> Hasql.interp [HI.sql|SELECT payload FROM rum_panel_cache WHERE cache_key = #{key} AND expires_at > now()|]


-- | Memory-miss path in one step: read the shared table, else compute and publish for
-- the fleet. @rawKey@ is hashed, so callers pass a readable @show@n key.
withSharedCache :: (AE.FromJSON a, AE.ToJSON a, DB es, Typeable a) => Text -> Int64 -> Eff es a -> Eff es a
withSharedCache rawKey ttl compute = do
  let key = toXXHash rawKey
  rumPanelCacheGet key >>= flip maybe pure do
    fresh <- compute
    rumPanelCacheSet key ttl fresh
    pure fresh


rumPanelCacheSet :: (AE.ToJSON a, DB es) => Text -> Int64 -> a -> Eff es ()
rumPanelCacheSet key ttlSeconds value = do
  let payload = AesonText value
  Hasql.interpExecute_
    [HI.sql|INSERT INTO rum_panel_cache (cache_key, payload, expires_at)
      VALUES (#{key}, #{payload}, now() + make_interval(secs => #{ttlSeconds}::double precision))
      ON CONFLICT (cache_key) DO UPDATE SET payload = EXCLUDED.payload, expires_at = EXCLUDED.expires_at|]
  -- Expired rows are pruned on write: writes are rare (one per cold panel per TTL), and it
  -- keeps the table from needing its own cleanup job.
  Hasql.interpExecute_ [HI.sql|DELETE FROM rum_panel_cache WHERE expires_at < now() - interval '1 hour'|]
