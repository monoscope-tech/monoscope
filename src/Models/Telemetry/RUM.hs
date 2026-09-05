module Models.Telemetry.RUM (
  RumBucket (..),
  RumSummary (..),
  RumTrend (..),
  RumPage (..),
  RumError (..),
  RumBreakdown (..),
  ReplaySession (..),
  RumSession (..),
  VitalSample (..),
  VitalTrendPoint (..),
  PageVitalPoint (..),
  RumQueryResult (..),
  RumQuery (..),
  RumCacheKey (..),
) where

import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Relude


data RumBucket = FiveMinutes | OneHour | SixHours
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)


data RumSummary = RumSummary
  { sessions :: Int64
  , pageViews :: Int64
  , users :: Int64
  , errors :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumTrend = RumTrend
  { bucket :: UTCTime
  , pageViews :: Int64
  , errors :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumPage = RumPage
  { path :: Text
  , views :: Int64
  , p75LoadMs :: Maybe Double
  , lastSeen :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumError = RumError
  { timestamp :: UTCTime
  , errorType :: Text
  , message :: Text
  , sessionId :: Maybe Text
  , userId :: Maybe Text
  , path :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | One user agent's traffic. The user agent string is classified into browser, OS and
-- device family in the page layer; the query only groups and counts.
data RumBreakdown = RumBreakdown
  { userAgent :: Text
  , sessions :: Int64
  , views :: Int64
  , errors :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data ReplaySession = ReplaySession
  { id :: UUID.UUID
  , startedAt :: UTCTime
  , endedAt :: UTCTime
  , userId :: Maybe Text
  , userName :: Maybe Text
  , userEmail :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


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
  deriving anyclass (HI.DecodeRow)


data VitalTrendPoint = VitalTrendPoint
  { bucket :: UTCTime
  , metricName :: Text
  , p75 :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data PageVitalPoint = PageVitalPoint
  { page :: Text
  , metricName :: Text
  , p75 :: Double
  , samples :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data VitalSample = VitalSample
  { metricName :: Text
  , value :: Double
  , samples :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumQueryResult
  = PulseResult RumSummary [RumTrend]
  | PagesResult [RumPage]
  | ErrorsResult [RumError]
  | SessionsResult [RumSession]
  | ReplaySessionsResult [ReplaySession]
  | VitalSamplesResult [VitalSample]
  | VitalTrendResult [VitalTrendPoint]
  | PageVitalsResult [PageVitalPoint]
  | ServicesResult [Text]
  | BreakdownResult [RumBreakdown]


data RumQuery
  = PulseQuery RumBucket
  | PagesQuery
  | ErrorsQuery
  | SessionsQuery
  | ReplaySessionsQuery
  | VitalSamplesQuery
  | VitalTrendQuery RumBucket
  | PageVitalsQuery
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
