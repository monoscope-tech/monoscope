module Models.Telemetry.RUM (
  RumBucket (..),
  RumSummary (..),
  RumTrend (..),
  RumPage (..),
  RumError (..),
  ReplaySession (..),
  RumSession (..),
  VitalSample (..),
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


data VitalSample = VitalSample
  { metricName :: Text
  , value :: Double
  , samples :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data RumQueryResult
  = SummaryResult RumSummary
  | TrendResult [RumTrend]
  | PagesResult [RumPage]
  | ErrorsResult [RumError]
  | SessionsResult [RumSession]
  | ReplaySessionsResult [ReplaySession]
  | VitalSamplesResult [VitalSample]


data RumQuery
  = SummaryQuery
  | TrendQuery RumBucket
  | PagesQuery
  | ErrorsQuery
  | SessionsQuery
  | ReplaySessionsQuery
  | VitalSamplesQuery
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)


data RumCacheKey = RumCacheKey
  { project :: Projects.ProjectId
  , query :: RumQuery
  , environment :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  , since :: Maybe Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)
