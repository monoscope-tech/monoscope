module Models.Apis.Fields.Facets (
  generateAndSaveFacets,
  getFacetSummary,
) where

import Control.Exception.Annotated (checkpoint)
import Control.Monad (foldM)
import Data.Aeson qualified as AE
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert, Select), execute, query)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query)
import Database.PostgreSQL.Transact (DBT)
import Effectful (Eff, (:>))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time qualified as Time
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Projects.Projects (ProjectId)
import Relude


-- | Generate facets for a project from a specified table and save to database
generateAndSaveFacets
  :: (DB :> es, UUID.UUIDEff :> es, Time.Time :> es)
  => ProjectId -> Text -> [Text] -> Int -> Eff es FacetSummary
generateAndSaveFacets projectId tableName columns maxValues = do
  -- Get current time and truncate to hour
  currentTime <- Time.currentTime
  let unixTime = utcTimeToPOSIXSeconds currentTime
      hourTruncated = fromIntegral @Int @Int64 (floor unixTime) `div` 3600 * 3600
      truncatedTime = posixSecondsToUTCTime (fromIntegral hourTruncated)
      hourEnd = addUTCTime 3600 truncatedTime

  -- Generate facets for all columns in one DB call
  facetMap <- dbtToEff $ do
    -- Process each column and build a map of column -> facet values
    facetEntries <- forM columns $ \column -> do
      values <-
        query
          Select
          (buildFacetQuery tableName column)
          (projectId, truncatedTime, hourEnd, maxValues)
      let facetValues = V.toList $ V.map (\(val, count) -> FacetValue val count) values
      pure (column, facetValues)

    -- Filter out empty entries and create the map
    let nonEmptyEntries = filter (not . null . snd) facetEntries
    pure $ HM.fromList nonEmptyEntries

  -- Create and save the facet summary
  facetId <- UUID.genUUID
  let summary =
        FacetSummary
          { id = facetId
          , projectId = projectId
          , tableName = tableName
          , timestamp = truncatedTime
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
    |]
        summary

  pure summary


-- | Build SQL query for facet generation for a specific column
buildFacetQuery :: Text -> Text -> Query
buildFacetQuery tableName columnName =
  [sql|
    SELECT 
      COALESCE(|]
    <> ""
    <> fromString (toString columnName)
    <> " "
    <> [sql|::text, 'null') as value, 
      COUNT(*) as count
    FROM  |]
    <> " "
    <> fromString (toString tableName)
    <> " "
    <> [sql|
     WHERE project_id = ?
      AND timestamp >= ?
      AND timestamp < ?
      AND |]
    <> " "
    <> fromString (toString columnName)
    <> " "
    <> [sql| IS NOT NULL
    GROUP BY value
    ORDER BY count DESC
    LIMIT ?
  |]


-- | Get a facet summary for a project/table/timestamp
getFacetSummary
  :: DB :> es => ProjectId -> Text -> UTCTime -> Eff es (Maybe FacetSummary)
getFacetSummary projectId tableName timestamp = checkpoint "getFacetSummary" $ dbtToEff $ do
  results <-
    query
      Select
      [sql|
      SELECT id, project_id::uuid, table_name, timestamp, facet_json
      FROM apis.facet_summaries
      WHERE project_id = ?
        AND table_name = ?
        order by timestamp desc
        limit 1 |]
      (projectId.toText, tableName)

  case results of
    [summary] -> pure $ Just summary
    _ -> pure Nothing
