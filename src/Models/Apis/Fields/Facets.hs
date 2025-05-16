module Models.Apis.Fields.Facets (
  generateAndSaveFacets,
  getFacetSummary,
  facetColumns,
) where

import Control.Exception.Annotated (checkpoint)
import Data.Annotation (toAnnotation)
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert, Select), execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query)
import Effectful (Eff, (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Projects.Projects (ProjectId)
import Relude


-- | Centralized list of facet columns for OTLP data
facetColumns :: [Text]
facetColumns =
  [ "name"
  , "resource___service___name"
  , "resource___service___version"
  , "attributes___http___request___method"
  , "attributes___http___response___status_code"
  , "attributes___db___system___name"
  , "attributes___error___type"
  , "attributes___user___id"
  , "attributes___session___id"
  , "level"
  , "kind"
  , "status_code"
  ]


-- | Generate facets for a project from a specified table and save to database
generateAndSaveFacets
  :: (DB :> es, UUID.UUIDEff :> es)
  => ProjectId
  -> Text
  -> [Text]
  -> Int
  -> UTCTime
  -> Eff es FacetSummary
generateAndSaveFacets pid tableName columns maxValues timestamp = do
  -- Truncate timestamp to hour
  let unixTime = utcTimeToPOSIXSeconds timestamp
      hourTruncated = fromIntegral @Int @Int64 (floor unixTime) `div` 3600 * 3600
      hourStart = posixSecondsToUTCTime (fromIntegral hourTruncated)
      hourEnd = addUTCTime 3600 hourStart

  -- Check if a summary already exists for this hour to avoid redundant computation
  existingSummary <- dbtToEff $ do
    results <-
      query
        Select
        [sql|
        SELECT id, project_id, table_name, timestamp, facet_json
        FROM apis.facet_summaries
        WHERE project_id = ?
          AND table_name = ?
          AND timestamp = ?
        LIMIT 1
        |]
        (pid.toText, tableName, hourStart)
    pure $ if V.null results then Nothing else Just (V.head results)

  -- If summary exists for this hour, return it
  case existingSummary of
    Just summary -> pure summary
    Nothing -> do
      -- Generate facets using an optimized query for all columns
      facetMap <- do
        let facetQuery = buildOptimizedFacetQuery tableName (length columns)
        -- Prepare columns for the optimized query
        values <-
          checkpoint (toAnnotation facetQuery)
            $ dbtToEff
            $ query
              Select
              facetQuery
              (V.fromList columns, pid.toText, hourStart, hourEnd, maxValues)
        pure $ processQueryResults values

      -- Create and save the new facet summary
      facetId <- UUID.genUUID
      let summary =
            FacetSummary
              { id = facetId
              , projectId = pid.toText
              , tableName = tableName
              , timestamp = hourStart
              , facetJson = FacetData facetMap
              }

      _ <-
        dbtToEff
          $ execute
            Insert
            [sql|
          INSERT INTO apis.facet_summaries 
            (id, project_id, table_name, timestamp, facet_json) 
          VALUES (?, ?, ?, ?, ?)
          ON CONFLICT (project_id, table_name, timestamp) DO NOTHING
        |]
            (summary.id, pid.toText, summary.tableName, summary.timestamp, summary.facetJson)

      pure summary


-- | Process query results directly into the final map format
processQueryResults :: V.Vector (Text, Text, Int) -> HM.HashMap Text [FacetValue]
processQueryResults results =
  V.foldr' addResult HM.empty results
  where
    addResult (colName, valText, count) acc =
      let
        currentVals = HM.lookupDefault [] colName acc
        -- Add new value and keep the list sorted by count (desc)
        newVal = FacetValue valText count
        updatedVals = insertSorted newVal currentVals
       in
        HM.insert colName (take 10 updatedVals) acc

    -- Insert value into sorted list (by count, descending)
    insertSorted newVal [] = [newVal]
    insertSorted newVal@(FacetValue _ count) (x@(FacetValue _ xcount) : xs)
      | count > xcount = newVal : x : xs
      | otherwise = x : insertSorted newVal xs


-- | Build a SQL fragment for a single facet column
buildFacetColumnQuery :: Text -> Text
buildFacetColumnQuery colName =
  "SELECT '" <> colName <> "' as column_name, " <> colName <> "::text as value, COUNT(*) as count FROM filtered_data WHERE " <> colName <> " IS NOT NULL GROUP BY value"


-- | Build an optimized facet query that reduces data processing
buildOptimizedFacetQuery :: Text -> Int -> Query
buildOptimizedFacetQuery tableName _ =
  let
    -- Generate the UNION ALL query from the centralized facet columns
    unionAllQueries = T.intercalate "\n      UNION ALL\n      " $ map buildFacetColumnQuery facetColumns
   in
    [sql|
      WITH columns_list AS (SELECT unnest(?::text[]) as column_name),
      filtered_data AS (
        SELECT * FROM |]
      <> (" " <> fromString (toString tableName) <> " ")
      <> [sql| 
        WHERE project_id = ?::text AND timestamp >= ? AND timestamp < ?
      ),
      combined_facets AS (
        -- All facet variants as a UNION ALL with one-line queries for better scannability
        |]
      <> fromString (toString unionAllQueries)
      <> [sql|
      ),
      filtered_facets AS (
        -- Join with requested columns to filter out unrequested ones
        SELECT cf.* FROM combined_facets cf JOIN columns_list cl ON cf.column_name = cl.column_name
      ),
      ranked_facets AS (
        -- Get top N values for each column
        SELECT column_name, value, count, ROW_NUMBER() OVER (PARTITION BY column_name ORDER BY count DESC) as rn
        FROM filtered_facets
      )
      SELECT column_name, value, count 
      FROM ranked_facets
      WHERE rn <= ?
      ORDER BY column_name, count DESC
    |]


-- | Get a facet summary for a project/table with time range extrapolation
getFacetSummary
  :: DB :> es => ProjectId -> Text -> UTCTime -> UTCTime -> Eff es (Maybe FacetSummary)
getFacetSummary projectId tableName fromTime toTime = checkpoint "getFacetSummary" $ do
  -- Calculate the time span in hours (rounded up)
  let projectIdText = projectId.toText
      timeSpanSeconds = max 1 $ floor $ diffUTCTime toTime fromTime
      timeSpanHours = (timeSpanSeconds + 3599) `div` 3600 -- Round up to nearest hour

      -- Calculate appropriate sampling to avoid excessive data
      -- For longer time ranges, we don't need every hour's data
      -- Strategy: Use all summaries for short ranges, sample evenly for longer ranges
      sampleLimit =
        if timeSpanHours <= 24
          then timeSpanHours + 1 -- Get all hours for up to 24 hour range
          else min 48 (timeSpanHours `div` 4 + 12) -- Sample reasonably for longer ranges
  let qury =
        [sql|
      WITH in_range AS (
        SELECT id, project_id, table_name, timestamp, facet_json
        FROM apis.facet_summaries
        WHERE project_id = ? AND table_name = ? AND timestamp >= ? AND timestamp <= ?
        ORDER BY timestamp DESC LIMIT ?
      ),
      before_range AS (
        SELECT id, project_id, table_name, timestamp, facet_json
        FROM apis.facet_summaries
        WHERE project_id = ? AND table_name = ? AND timestamp < ? AND NOT EXISTS (SELECT 1 FROM in_range)
        ORDER BY timestamp DESC LIMIT 1
      )
      SELECT * FROM in_range
      UNION ALL
      SELECT * FROM before_range
      |]

  -- Fetch facet summaries in the time range, plus a fallback if needed
  summariesVec <-
    checkpoint (toAnnotation qury)
      $ dbtToEff
      $ query
        Select
        qury
        (projectIdText, tableName, fromTime, toTime, sampleLimit, projectIdText, tableName, fromTime)

  let summaries = V.toList summariesVec
  case summaries of
    [] -> pure Nothing
    [summary] -> pure $ fromSingleSummary timeSpanHours timeSpanSeconds summary
    _ -> pure $ mergeSummariesEfficient timeSpanHours summaries
  where
    -- Handle a single summary (scale based on time span)
    fromSingleSummary :: Int -> Int -> FacetSummary -> Maybe FacetSummary
    fromSingleSummary timeSpanHours timeSpanSeconds summary =
      let ratio =
            if timeSpanHours < 1
              then fromIntegral timeSpanSeconds / 3600.0 -- For sub-hour spans
              else fromIntegral timeSpanHours -- For spans >= 1 hour
          (FacetData facetMap) = summary.facetJson
          -- Skip scaling if ratio is close to 1 to save CPU
          scaledMap =
            if abs (ratio - 1.0) < 0.1
              then facetMap
              else HM.map (map (\(FacetValue v c) -> FacetValue v (ceiling $ fromIntegral c * ratio))) facetMap
       in Just summary{facetJson = FacetData scaledMap}

    -- Optimized version of merge summaries
    mergeSummariesEfficient :: Int -> [FacetSummary] -> Maybe FacetSummary
    mergeSummariesEfficient timeSpanHours summaries =
      summaries & viaNonEmpty \nonEmptySummaries ->
        let
          latestSummary = head nonEmptySummaries
          numSummaries = length $ toList nonEmptySummaries
          scaleFactor = max 1.0 (fromIntegral timeSpanHours / fromIntegral numSummaries)
          mergedMap = buildMergedFacetMap (toList nonEmptySummaries) scaleFactor
         in
          latestSummary{facetJson = FacetData mergedMap}

    -- Build merged facet map in a more efficient way
    buildMergedFacetMap :: [FacetSummary] -> Double -> HM.HashMap Text [FacetValue]
    buildMergedFacetMap summaries scaleFactor =
      -- Step 1: Extract and combine all facet maps
      let allMaps = map (.facetJson) summaries
          combinedMap = foldl' combineIntoAccumulator HM.empty allMaps
       in combinedMap
      where
        -- Helper to combine facet data into accumulator
        combineIntoAccumulator :: HM.HashMap Text [FacetValue] -> FacetData -> HM.HashMap Text [FacetValue]
        combineIntoAccumulator acc (FacetData facetMap) =
          HM.foldrWithKey
            ( \k values result ->
                let
                  -- Get existing values for this key or empty list
                  existing = HM.lookupDefault [] k result
                  -- Merge with new values
                  allValues = existing ++ values
                  -- Group by value, sum counts, scale, and sort
                  grouped = HM.fromListWith (+) [(v, c) | FacetValue v c <- allValues]
                  merged =
                    grouped
                      & HM.toList
                      & map (\(v, c) -> FacetValue v (ceiling $ fromIntegral c * scaleFactor))
                      & sortOn (negate . (.count))
                 in
                  HM.insert k merged result
            )
            acc
            facetMap
