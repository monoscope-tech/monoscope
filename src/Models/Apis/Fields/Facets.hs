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
import Database.PostgreSQL.Entity.DBT (execute, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Only (..), Query)
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
  -- Calculate 24-hour window
  let dayEnd = timestamp
      dayStart = addUTCTime (-86400) dayEnd

  -- Generate facets for the last 24 hours
  facetMap <- do
    values <-
      checkpoint (toAnnotation (buildOptimizedFacetQuery tableName (length columns)))
        $ dbtToEff
        $ query
          (buildOptimizedFacetQuery tableName (length columns))
          (V.fromList columns, pid.toText, dayStart, dayEnd, maxValues)
    pure $ processQueryResults values

  -- Get existing summary ID (if any) to preserve it
  existingIdM <-
    dbtToEff
      $ query
        [sql| SELECT id FROM apis.facet_summaries
              WHERE project_id = ? AND table_name = ?
              LIMIT 1 |]
        (pid.toText, tableName)
        <&> \case
          v
            | V.null v -> Nothing
            | otherwise -> Just (V.head v)

  -- Create a summary object with either existing or new ID
  facetId <- case existingIdM of
    Just (Only uuid) -> pure uuid
    Nothing -> UUID.genUUID

  let summary =
        FacetSummary
          { id = facetId
          , projectId = pid.toText
          , tableName = tableName
          , facetJson = FacetData facetMap
          }

  -- Upsert the summary - replace existing data
  _ <-
    dbtToEff
      $ execute
        [sql| INSERT INTO apis.facet_summaries (id, project_id, table_name, facet_json)
              VALUES (?, ?, ?, ?)
              ON CONFLICT (project_id, table_name) 
              DO UPDATE SET facet_json = EXCLUDED.facet_json |]
        (summary.id, pid.toText, summary.tableName, summary.facetJson)

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
        -- Use 24-hour window (dayStart to dayEnd) for data aggregation
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
  -- Calculate time span in minutes for more precise scaling
  let projectIdText = projectId.toText
      timeSpanSeconds = max 1 $ floor $ diffUTCTime toTime fromTime
      timeSpanMinutes = (timeSpanSeconds + 59) `div` 60 -- Round up to nearest minute

  -- Fetch the single facet summary for this project/table
  summaryVec <-
    checkpoint "Fetching facet summary"
      $ dbtToEff
      $ query
        [sql| SELECT id, project_id, table_name, facet_json
              FROM apis.facet_summaries
              WHERE project_id = ? AND table_name = ?
              LIMIT 1 |]
        (projectIdText, tableName)

  -- Scale facet data based on requested time range
  pure $ case V.toList summaryVec of
    [] -> Nothing
    [summary] -> Just $ scaleFacetSummary summary timeSpanMinutes
    _ -> error "Multiple facet summaries found for same project/table (should be impossible)"
  where
    -- Scale facet counts based on time window
    scaleFacetSummary :: FacetSummary -> Int -> FacetSummary
    scaleFacetSummary summary timeSpanMinutes =
      let
        -- Calculate scaling factor based on minutes
        -- Our baseline is 24 hours = 1440 minutes
        scaleFactor = fromIntegral timeSpanMinutes / 1440.0

        -- Scale each facet count
        (FacetData facetMap) = summary.facetJson
        scaledMap =
          if abs (scaleFactor - 1.0) < 0.01 -- Skip scaling if very close to 1
            then facetMap
            else HM.map (scaleFacetValues scaleFactor) facetMap
       in
        summary{facetJson = FacetData scaledMap}

    -- Scale individual facet values, preserving at least 1 count
    scaleFacetValues :: Double -> [FacetValue] -> [FacetValue]
    scaleFacetValues factor =
      map (\(FacetValue v c) -> FacetValue v (max 1 $ ceiling $ fromIntegral c * factor))
