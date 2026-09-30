module Models.Telemetry.RUM (
  RumBucket (..),
  RumPage (..),
  RumError (..),
  RumBreakdown (..),
  ReplaySession (..),
  RumSession (..),
  SessionFilter (..),
  ObservationCount,
  VitalEstimate,
  estimateValue,
  estimateRange,
  VitalCoverage (..),
  VitalMeasurement (..),
  measurementValue,
  measurementSamples,
  VitalGrouping (..),
  VitalPopulation (..),
  rumVitalPopulation,
  VitalTrendPoint (..),
  PageVitalPoint (..),
  RumQueryResult (..),
  RumQuery (..),
  RumCacheKey (..),
  rumPanelCacheGet,
  rumPanelCacheGetStale,
  withSharedCache,
  rumPanelCacheSet,
) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Effectful (Eff, (:>))
import Effectful.Labeled (Labeled)
import Hasql.Decoders qualified as D
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.DeriveUtils (AesonText (..), DB, WrappedEnumSC (..))
import Pkg.Parser (ScopedQuery (..))
import Relude
import Relude.Extra.Foldable1 (minimum1)
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
  , userAgent :: Maybe Text
  , hasReplay :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


-- $setup
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.Either (isLeft)
-- >>> import Relude (LByteString)


-- | Cache decoding preserves positive observation counts.
--
-- >>> let valid = AE.eitherDecode "3" :: Either String ObservationCount
-- >>> fmap (\count -> AE.eitherDecode (AE.encode count) == Right count) valid
-- Right True
-- >>> map (isLeft . (AE.eitherDecode :: LByteString -> Either String ObservationCount)) ["0", "-1"]
-- [True,True]
newtype ObservationCount = ObservationCount Natural
  deriving stock (Eq, Generic, Show)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON ObservationCount where
  parseJSON value = do
    count <- AE.parseJSON value
    maybe (fail "Observation count must be positive") pure (mkObservationCount count)


mkObservationCount :: Natural -> Maybe ObservationCount
mkObservationCount count = ObservationCount count <$ guard (count > 0)


-- | A cached quantile is finite and nonnegative, and an estimated value lies in its bucket.
--
-- >>> let valid = AE.eitherDecode "{\"tag\":\"Estimated\",\"contents\":[75,0,100]}" :: Either String VitalEstimate
-- >>> fmap (\estimate -> AE.eitherDecode (AE.encode estimate) == Right estimate) valid
-- Right True
-- >>> map (isLeft . (AE.eitherDecode :: LByteString -> Either String VitalEstimate)) ["{\"tag\":\"Estimated\",\"contents\":[120,0,100]}", "{\"tag\":\"Exact\",\"contents\":-1}", "{\"tag\":\"Exact\",\"contents\":1e400}"]
-- [True,True,True]
data VitalEstimate = Exact Double | Estimated Double Double Double
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.ToJSON)


instance AE.FromJSON VitalEstimate where
  parseJSON value = do
    estimate <- AE.genericParseJSON AE.defaultOptions value
    maybe (fail "Invalid vital quantile or bucket range") pure (mkEstimate estimate)


mkEstimate :: VitalEstimate -> Maybe VitalEstimate
mkEstimate estimate = estimate <$ guard valid
  where
    valid =
      finite (estimateValue estimate) && case estimateRange estimate of
        Nothing -> True
        Just (lower, upper) -> finite lower && finite upper && lower <= estimateValue estimate && estimateValue estimate <= upper
    finite value = value >= 0 && not (isInfinite value || isNaN value)


estimateValue :: VitalEstimate -> Double
estimateValue = \case
  Exact value -> value
  Estimated value _ _ -> value


estimateRange :: VitalEstimate -> Maybe (Double, Double)
estimateRange = \case
  Exact _ -> Nothing
  Estimated _ lower upper -> Just (lower, upper)


data VitalCoverage = MissingBaseline | UnknownStart | UnknownTemporality | UnsupportedPopulation | InvalidValue | InvalidBuckets | InvalidReset | OverlappingIntervals | UnboundedBucket | InterruptedSeries
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "" VitalCoverage


data VitalMeasurement = Unmeasured | Measured ObservationCount VitalEstimate | Unavailable Natural VitalCoverage
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


measurementValue :: VitalMeasurement -> Maybe Double
measurementValue = \case
  Measured _ estimate -> Just (estimateValue estimate)
  Unmeasured -> Nothing
  Unavailable _ _ -> Nothing


measurementSamples :: VitalMeasurement -> Natural
measurementSamples = \case
  Measured (ObservationCount count) _ -> count
  Unmeasured -> 0
  Unavailable count _ -> count


data VitalGrouping = FieldVital | TrendVital UTCTime | PageVital Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data VitalPopulation = VitalPopulation
  { grouping :: VitalGrouping
  , metricName :: Text
  , measurement :: VitalMeasurement
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


instance HI.DecodeRow VitalPopulation where
  decodeRow = D.column $ D.nonNullable $ HI.decodeValue @VitalPopulation


instance HI.DecodeValue VitalPopulation where
  decodeValue =
    D.refine decode
      $ D.record
      $ (,,,,,,,)
      <$> D.field (D.nullable D.timestamptz)
      <*> D.field (D.nullable D.text)
      <*> D.field (D.nonNullable D.text)
      <*> D.field (D.nonNullable D.int8)
      <*> D.field (D.nullable $ HI.decodeValue @VitalCoverage)
      <*> D.field (D.nullable D.float8)
      <*> D.field (D.nullable D.float8)
      <*> D.field (D.nullable D.float8)
    where
      decode (bucket, page, metric, count, coverage, value, lower, upper) = do
        grouping <- case (bucket, page) of
          (Nothing, Nothing) -> Right FieldVital
          (Just time, Nothing) -> Right (TrendVital time)
          (Nothing, Just url) -> Right (PageVital url)
          _ -> Left "Invalid vital population grouping"
        guardError "Negative vital observation count" (count >= 0)
        measurement <- case (coverage, count, value, lower, upper) of
          (Just issue, _, Nothing, _, _) -> Right $ Unavailable (fromIntegral count) issue
          (Nothing, 0, Nothing, _, _) -> Right Unmeasured
          (Nothing, _, Just measured, Nothing, Nothing) -> measuredEstimate count (Exact measured)
          (Nothing, _, Just measured, Just lo, Just hi) -> measuredEstimate count (Estimated measured lo hi)
          _ -> Left "Invalid vital population measurement"
        pure VitalPopulation{grouping, metricName = metric, measurement}
      measuredEstimate count estimate = do
        samples <- maybeToRight "Invalid measured vital population" (mkObservationCount $ fromIntegral count)
        Measured samples <$> maybeToRight "Invalid measured vital population" (mkEstimate estimate)
      guardError message condition = unless condition (Left message)


data VitalRead = PopulationRead VitalPopulation | EpochRead Text UTCTime


data VitalReadKind = Population | Epoch
  deriving stock (Read, Show)
  deriving (HI.DecodeValue, HI.EncodeValue) via WrappedEnumSC 'Nothing "" VitalReadKind


instance HI.DecodeRow VitalRead where
  decodeRow =
    D.column
      $ D.nonNullable
      $ D.refine decode
      $ D.record
      $ (,,,)
      <$> D.field (D.nonNullable $ HI.decodeValue @VitalReadKind)
      <*> D.field (D.nullable $ HI.decodeValue @VitalPopulation)
      <*> D.field (D.nullable D.text)
      <*> D.field (D.nullable D.timestamptz)
    where
      decode (Population, Just population, Nothing, Nothing) = Right $ PopulationRead population
      decode (Epoch, Nothing, Just series, Just start) = Right $ EpochRead series start
      decode _ = Left "Invalid vital read row"


data VitalTrendPoint = VitalTrendPoint
  { bucket :: UTCTime
  , metricName :: Text
  , measurement :: VitalMeasurement
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data PageVitalPoint = PageVitalPoint
  { page :: Text
  , metricName :: Text
  , measurement :: VitalMeasurement
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | A histogram contributes observations, not its export mean. All surfaces share the
-- same population, with exact scalar quantiles and estimates over common explicit bounds.
-- Reports are selected by end time; DELTA populations do not require a known start.
rumVitalPopulation :: (DB es, Labeled "timefusion" Hasql.Hasql :> es) => Bool -> ScopedQuery -> TimePicker.TimeWindow -> RumBucket -> Eff es [VitalPopulation]
rumVitalPopulation useTf scope window bucket = Hasql.withHasqlTimefusion useTf do
  let interval :: Text
      interval = case bucket of FiveMinutes -> "5 minutes"; OneHour -> "1 hour"; SixHours -> "6 hours"
      vitalScope =
        let names :: [Text]
            names = [prefix <> name | name <- ["lcp", "inp", "cls", "fcp", "ttfb"], prefix <- ["browser.web_vital.", "k6.browser_web_vital_"]]
         in [HI.sql|project_id=#{scope.projectId.toText} AND (#{scope.environment}::text IS NULL OR resource___deployment___environment___name = #{scope.environment})
        AND (#{scope.service}::text IS NULL OR resource___service___name = #{scope.service}) AND metric_name = ANY(#{names}::text[])
        AND (starts_with(metric_name,'browser.web_vital.') OR metric_type='GAUGE')|]
      columns =
        [HI.sql|timestamp,start_timestamp,series_id,metric_type,aggregation_temporality,flags,
        CASE WHEN metric_type IN ('GAUGE') THEN value END AS value,
        distribution_count,hist_bucket_counts,CASE WHEN hist_explicit_bounds IS NULL AND cardinality(hist_bucket_counts)=1 THEN ARRAY[]::float8[] ELSE hist_explicit_bounds END AS hist_explicit_bounds,
        COALESCE(attributes #>> '{page,url}',attributes->>'page.url',attributes->>'url') AS page,
        CASE WHEN starts_with(metric_name,'browser.web_vital.') THEN substring(metric_name FROM length('browser.web_vital.')+1)
          ELSE substring(metric_name FROM length('k6.browser_web_vital_')+1) END::text AS metric|]
      current = [HI.sql|SELECT |] <> columns <> [HI.sql| FROM otel_metrics WHERE |] <> vitalScope <> [HI.sql| AND timestamp>=#{window.fromTime} AND timestamp<=#{window.toTime}|]
      withHistory epochs = case nonEmpty epochs of
        Nothing -> current
        Just selected ->
          let oldest = minimum1 $ fmap snd selected
              seriesIds :: [Text]
              seriesIds = ordNub $ map fst epochs
              exactEpochs = mconcat $ intersperse [HI.sql| OR |] [[HI.sql|(series_id::text=#{seriesId} AND start_timestamp=#{start})|] | (seriesId, start) <- epochs]
           in current
                <> [HI.sql| UNION ALL SELECT |]
                <> columns
                <> [HI.sql| FROM (
                SELECT *,ROW_NUMBER() OVER(PARTITION BY series_id,CASE WHEN flags & 1!=0 THEN NULL ELSE start_timestamp END ORDER BY timestamp DESC) AS predecessor
                FROM otel_metrics WHERE |]
                <> vitalScope
                <> [HI.sql| AND series_id = ANY(#{seriesIds}::text[])|]
                <> [HI.sql| AND timestamp>=#{oldest} AND timestamp<#{window.fromTime} AND (flags & 1!=0 OR |]
                <> exactEpochs
                <> [HI.sql|)) AS history WHERE predecessor=1|]
      -- TimeFusion drops partitions when inheriting a named window with a new order/frame.
      readPopulations source discoverEpochs =
        Hasql.interpForTimefusion useTf
          $ [HI.sql|WITH source AS (|]
          <> source
          <> [HI.sql|
), previous AS (
 SELECT *, LAG(CASE WHEN flags & 1=0 THEN timestamp END) OVER(PARTITION BY series_id,start_timestamp ORDER BY timestamp) AS prior_timestamp,
   MAX(CASE WHEN flags & 1!=0 THEN timestamp END) OVER(PARTITION BY series_id ORDER BY timestamp ROWS UNBOUNDED PRECEDING) AS last_marker,
   LAG(CASE WHEN flags & 1=0 THEN timestamp END) OVER(PARTITION BY series_id ORDER BY timestamp) AS previous_series_timestamp,
   LAG(CASE WHEN flags & 1=0 THEN distribution_count END) OVER(PARTITION BY series_id,start_timestamp ORDER BY timestamp) AS prior_count,
   LAG(CASE WHEN flags & 1=0 THEN hist_bucket_counts END) OVER(PARTITION BY series_id,start_timestamp ORDER BY timestamp) AS prior_counts,
   LAG(CASE WHEN flags & 1=0 THEN hist_explicit_bounds END) OVER(PARTITION BY series_id,start_timestamp ORDER BY timestamp) AS prior_bounds
 FROM source
), points AS (
 SELECT *, time_bucket(#{interval}::interval,timestamp) AS bucket,
   CASE WHEN metric_type IN ('GAUGE') AND value IS NOT NULL THEN 1
     WHEN aggregation_temporality='DELTA' THEN distribution_count
     WHEN prior_count IS NOT NULL THEN distribution_count-prior_count
     WHEN start_timestamp=timestamp THEN 0
     WHEN start_timestamp >= #{window.fromTime} AND start_timestamp < timestamp THEN distribution_count
     ELSE 0 END AS observations,
   CASE WHEN metric_type='GAUGE' THEN CASE WHEN value>=0 AND value<'Infinity'::float8 THEN 'complete' ELSE #{InvalidValue} END
     WHEN metric_type!='HISTOGRAM' THEN #{UnsupportedPopulation}
     WHEN distribution_count IS NULL OR hist_bucket_counts IS NULL OR hist_explicit_bounds IS NULL OR cardinality(hist_bucket_counts)!=cardinality(hist_explicit_bounds)+1 OR distribution_count<0 THEN #{InvalidBuckets}
     WHEN start_timestamp>timestamp OR (start_timestamp IS NULL AND aggregation_temporality IS DISTINCT FROM 'DELTA') THEN #{UnknownStart}
     WHEN aggregation_temporality='DELTA' AND previous_series_timestamp>start_timestamp THEN #{OverlappingIntervals}
     WHEN aggregation_temporality='DELTA' THEN 'complete'
     WHEN aggregation_temporality IS NULL OR aggregation_temporality!='CUMULATIVE' THEN #{UnknownTemporality}
     WHEN last_marker>prior_timestamp OR (prior_timestamp IS NULL AND last_marker>=start_timestamp AND start_timestamp!=timestamp) THEN #{InterruptedSeries}
     WHEN prior_count IS NOT NULL AND (distribution_count<prior_count OR (prior_timestamp=timestamp AND distribution_count!=prior_count)) THEN #{InvalidReset}
     WHEN prior_count IS NULL AND previous_series_timestamp>start_timestamp THEN #{OverlappingIntervals}
     WHEN prior_count IS NOT NULL THEN 'complete'
     WHEN start_timestamp=timestamp THEN 'complete'
     WHEN start_timestamp<#{window.fromTime} THEN #{MissingBaseline}
     ELSE 'complete' END AS coverage
 FROM previous WHERE timestamp>=#{window.fromTime} AND flags & 1=0
), sides AS (
 SELECT points.*,unnest(ARRAY[0,1]) AS side FROM points
), expanded AS (
 SELECT sides.*,CASE WHEN value IS NOT NULL THEN 1 WHEN side=0 THEN distribution_count ELSE prior_count END AS expected_count,
   unnest(CASE WHEN value IS NOT NULL THEN ARRAY[value]::float8[]
      WHEN side=0 THEN hist_explicit_bounds||ARRAY[NULL]::float8[] ELSE prior_bounds||ARRAY[NULL]::float8[] END) AS bound,
   unnest(CASE WHEN value IS NOT NULL THEN ARRAY[1]::bigint[]
      WHEN side=0 THEN hist_bucket_counts ELSE prior_counts END) AS amount
 FROM sides
 WHERE side=0 OR (aggregation_temporality='CUMULATIVE' AND prior_count IS NOT NULL AND value IS NULL)
), indexed AS (
 SELECT *,CASE WHEN value IS NOT NULL THEN 1 ELSE COALESCE(array_position(CASE WHEN side=0 THEN hist_explicit_bounds ELSE prior_bounds END,bound),cardinality(CASE WHEN side=0 THEN hist_explicit_bounds ELSE prior_bounds END)+1) END AS position
 FROM expanded
), cumulative AS (
 SELECT *,SUM(amount) OVER(PARTITION BY timestamp,series_id,start_timestamp,side ORDER BY bound NULLS LAST ROWS UNBOUNDED PRECEDING) AS cumulative,
   SUM(amount) OVER(PARTITION BY timestamp,series_id,start_timestamp,side) AS bucket_count,
   LAG(bound) OVER(PARTITION BY timestamp,series_id,start_timestamp,side ORDER BY position) AS preceding_bound,
   ROW_NUMBER() OVER(PARTITION BY timestamp,series_id,start_timestamp,side ORDER BY position) AS bound_ordinal
 FROM indexed
), validated AS (
 SELECT *,MAX(CASE WHEN coverage IN (#{UnsupportedPopulation},#{InvalidValue}) THEN coverage WHEN amount IS NULL OR amount<0 OR bucket_count!=expected_count OR expected_count<0 OR (bound IS NOT NULL AND NOT(bound>=0 AND bound<'Infinity'::float8)) OR (bound_ordinal>1 AND bound IS NOT NULL AND (preceding_bound IS NULL OR bound<=preceding_bound)) THEN #{InvalidBuckets} ELSE coverage END) OVER(PARTITION BY timestamp,series_id,start_timestamp) AS point_coverage
 FROM cumulative
), paired AS (
 SELECT *, MAX(CASE WHEN side=1 THEN cumulative END) OVER(PARTITION BY timestamp,series_id,start_timestamp,bound) AS prior_cumulative
 FROM validated
), selected_raw AS (
 SELECT *,cumulative-COALESCE(prior_cumulative,0) AS cdf,
   ROW_NUMBER() OVER(PARTITION BY timestamp,series_id,start_timestamp ORDER BY bound NULLS LAST) AS ordinal
 FROM paired WHERE side=0 AND (value IS NOT NULL OR aggregation_temporality='DELTA' OR prior_count IS NULL OR prior_cumulative IS NOT NULL)
), selected AS (
 SELECT *,MAX(CASE WHEN point_coverage='complete' AND (cdf<0 OR cdf<LAG_CDF) THEN #{InvalidBuckets} ELSE point_coverage END) OVER(PARTITION BY timestamp,series_id,start_timestamp) AS effective_coverage
 FROM (SELECT *,LAG(cdf,1,0) OVER(PARTITION BY timestamp,series_id,start_timestamp ORDER BY bound NULLS LAST) AS LAG_CDF FROM selected_raw) AS ordered
), totals AS (
 SELECT CASE WHEN GROUPING(bucket)=0 THEN 1 WHEN GROUPING(page)=0 THEN 2 ELSE 0 END AS kind,
   bucket AS group_bucket,page AS group_page,metric,bound,
   SUM(CASE WHEN value IS NULL AND observations>0 AND effective_coverage='complete' THEN cdf ELSE 0 END)::bigint AS histogram_cdf,
   COUNT(*) FILTER(WHERE value IS NULL AND observations>0 AND effective_coverage='complete') AS histogram_points_at_bound,
   SUM(CASE WHEN value IS NOT NULL AND effective_coverage='complete' THEN observations ELSE 0 END)::bigint AS scalar_observations,
   SUM(CASE WHEN ordinal=1 AND effective_coverage='complete' THEN observations ELSE 0 END)::bigint AS observations,
   COUNT(*) FILTER(WHERE ordinal=1 AND value IS NULL AND observations>0 AND effective_coverage='complete') AS histogram_points,
   MAX(effective_coverage) AS coverage
 FROM selected GROUP BY GROUPING SETS((metric,bound),(bucket,metric,bound),(page,metric,bound))
), distribution AS (
 SELECT *,SUM(observations) OVER population AS total,
   SUM(histogram_points) OVER population AS total_histogram_points,
   MAX(coverage) OVER population AS population_coverage,
   SUM(scalar_observations) OVER scalar_population AS scalar_cdf
 FROM totals
 WINDOW population AS (PARTITION BY kind,group_bucket,group_page,metric),
   scalar_population AS (PARTITION BY kind,group_bucket,group_page,metric ORDER BY bound NULLS LAST ROWS UNBOUNDED PRECEDING)
), common AS (
 SELECT *,CASE WHEN total_histogram_points=0 THEN 1+0.75::float8*(total-1)::float8 ELSE 0.75::float8*total::float8 END AS quantile_position,
   MAX(CASE WHEN histogram_points_at_bound=total_histogram_points THEN bound END) OVER preceding_population AS histogram_lower_bound,
   MIN(CASE WHEN histogram_points_at_bound=total_histogram_points THEN bound END) OVER following_population AS histogram_upper_bound,
   MAX(CASE WHEN histogram_points_at_bound=total_histogram_points THEN histogram_cdf END) OVER cumulative_population AS histogram_lower_cdf,
   MIN(CASE WHEN histogram_points_at_bound=total_histogram_points THEN histogram_cdf END) OVER following_population AS histogram_upper_cdf
 FROM distribution WHERE total_histogram_points=0 OR histogram_points_at_bound=total_histogram_points OR scalar_observations>0
 WINDOW preceding_population AS (PARTITION BY kind,group_bucket,group_page,metric ORDER BY bound NULLS LAST ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING),
   following_population AS (PARTITION BY kind,group_bucket,group_page,metric ORDER BY bound NULLS LAST ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING),
   cumulative_population AS (PARTITION BY kind,group_bucket,group_page,metric ORDER BY bound NULLS LAST ROWS UNBOUNDED PRECEDING)
), interpolated AS (
 SELECT *,CASE WHEN histogram_points_at_bound=total_histogram_points THEN histogram_cdf::float8
     WHEN COALESCE(histogram_lower_cdf,0)=histogram_upper_cdf THEN histogram_upper_cdf::float8
     WHEN histogram_upper_bound IS NOT NULL THEN COALESCE(histogram_lower_cdf,0)::float8+
       (histogram_upper_cdf-COALESCE(histogram_lower_cdf,0))::float8*(bound-COALESCE(histogram_lower_bound,0))/(histogram_upper_bound-COALESCE(histogram_lower_bound,0)) END AS histogram_at_bound
 FROM common
), combined AS (
 SELECT *,COALESCE(histogram_at_bound,histogram_lower_cdf::float8,0)+scalar_cdf::float8 AS cdf FROM interpolated
), ranked AS (
 SELECT *,CASE WHEN total_histogram_points=0 THEN CEIL(quantile_position) ELSE quantile_position END AS quantile_target,
   LAG(bound) OVER population AS lower_bound,
   LAG(cdf,1,0) OVER population AS lower_cdf,
   LAG(histogram_at_bound,1,0) OVER population AS prior_histogram_cdf,
   ROW_NUMBER() OVER population AS quantile_ordinal
 FROM combined
 WINDOW population AS (PARTITION BY kind,group_bucket,group_page,metric ORDER BY bound NULLS LAST)
), quantiles AS (
 SELECT *,CASE WHEN population_coverage='complete' AND total>0 AND (bound IS NULL OR histogram_at_bound IS NULL) THEN #{UnboundedBucket}
    WHEN population_coverage!='complete' THEN population_coverage END AS issue,
   CASE WHEN population_coverage!='complete' OR total=0 OR bound IS NULL OR histogram_at_bound IS NULL THEN NULL
     WHEN total_histogram_points=0 THEN CASE WHEN lower_cdf<FLOOR(quantile_position) THEN bound
       ELSE lower_bound+(bound-lower_bound)*(quantile_position-FLOOR(quantile_position)) END
     WHEN quantile_position>cdf-scalar_observations::float8 THEN bound
     ELSE COALESCE(lower_bound,0)::float8+(bound-COALESCE(lower_bound,0))::float8*
       (quantile_position-lower_cdf::float8)/(histogram_at_bound-prior_histogram_cdf)::float8 END AS p75
 FROM ranked WHERE ((total=0 OR population_coverage!='complete') AND quantile_ordinal=1)
   OR (total>0 AND population_coverage='complete'
     AND cdf>=quantile_target
     AND lower_cdf<quantile_target)
), reads AS (
SELECT #{Population}::text AS read_kind,ROW(group_bucket,group_page,metric::text,total::bigint,issue::text,p75::float8,
  CASE WHEN issue IS NULL AND total>0 AND total_histogram_points>0 AND (histogram_points_at_bound=total_histogram_points OR COALESCE(histogram_lower_cdf,0)!=histogram_upper_cdf) THEN COALESCE(histogram_lower_bound,0)::float8 END,
  CASE WHEN issue IS NULL AND total>0 AND total_histogram_points>0 AND (histogram_points_at_bound=total_histogram_points OR COALESCE(histogram_lower_cdf,0)!=histogram_upper_cdf) THEN histogram_upper_bound::float8 END) AS population,NULL::text AS epoch_series,NULL::timestamptz AS epoch_start
FROM quantiles WHERE kind!=2 OR group_page IS NOT NULL
UNION ALL
SELECT #{Epoch}::text,NULL,series_id::text,start_timestamp FROM (
 SELECT DISTINCT series_id,start_timestamp FROM selected
 WHERE #{discoverEpochs} AND metric_type='HISTOGRAM' AND aggregation_temporality='CUMULATIVE' AND start_timestamp<#{window.fromTime}
) AS epochs
)
SELECT ROW(read_kind,population,epoch_series,epoch_start) FROM reads
|]
  initial <- readPopulations current True
  let epochs = [(series, start) | EpochRead series start <- initial]
  populationReads <- if null epochs then pure initial else readPopulations (withHistory epochs) False
  let populations = [population | PopulationRead population <- populationReads]
  -- Grouped pages are ordered/capped here to avoid native final-sort memory pressure.
  let (pages, fieldAndTrend) = partition (\population -> case population.grouping of PageVital{} -> True; FieldVital -> False; TrendVital{} -> False) populations
      pageGroups = Map.fromListWith (<>) [(url, [population]) | population@VitalPopulation{grouping = PageVital url} <- pages]
      orderedPages = sortWith (\(url, rows) -> (Down $ sum $ map (measurementSamples . (.measurement)) rows, url)) $ Map.toList pageGroups
  pure
    $ sortWith (\population -> (population.grouping, population.metricName)) fieldAndTrend
    <> concatMap (sortWith (.metricName) . snd) (take 150 orderedPages)


data RumQueryResult
  = PresenceResult Bool
  | PagesResult [RumPage]
  | ErrorsResult [RumError]
  | SessionsResult [RumSession]
  | ReplaySessionsResult [ReplaySession]
  | SessionDetailResult (Maybe RumSession)
  | VitalPopulationResult [VitalPopulation]
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
  | VitalPopulationQuery RumBucket
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
rumPanelCacheGet :: (AE.FromJSON a, DB es) => Text -> Eff es (Maybe a)
rumPanelCacheGet key =
  fmap (\(HI.OneColumn (AesonText value)) -> value)
    . listToMaybe
    <$> Hasql.interp [HI.sql|SELECT payload FROM rum_panel_cache WHERE cache_key = #{key} AND expires_at > now()|]


-- | Payload plus staleness: 'True' when the entry is past its freshness expiry but still
-- inside the prune horizon ('rumPanelCacheSet' deletes rows expired by over an hour).
-- The RUM page serves stale entries instantly and revalidates in the background, so a
-- panel never holds first paint hostage to a fresh scan of the window. Entries older than
-- the horizon are treated as absent — past that point the drift would mislead rather than
-- orient.
rumPanelCacheGetStale :: (AE.FromJSON a, DB es) => Text -> Eff es (Maybe (a, Bool))
rumPanelCacheGetStale key =
  fmap (\(AesonText value, isStale) -> (value, isStale))
    . listToMaybe
    <$> Hasql.interp [HI.sql|SELECT payload, expires_at <= now() FROM rum_panel_cache WHERE cache_key = #{key} AND expires_at > now() - interval '1 hour'|]


-- | Memory-miss path in one step: read the shared table, else compute and publish for
-- the fleet. @rawKey@ is hashed, so callers pass a readable @show@n key.
withSharedCache :: (AE.FromJSON a, AE.ToJSON a, DB es) => Text -> Int64 -> Eff es a -> Eff es a
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
