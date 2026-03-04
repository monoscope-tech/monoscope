module Models.Apis.PatternMerge (
  -- Error pattern operations
  getUnembeddedErrorPatterns,
  getCanonicalErrorPatterns,
  updateErrorEmbeddings,
  assignErrorsToCanonical,
  unmergeErrorPattern,
  getErrorPatternGroupMembers,
  getErrorPatternMemberCount,
  -- Log pattern operations
  getUnembeddedLogPatterns,
  getCanonicalLogPatterns,
  updateLogEmbeddings,
  assignLogsToCanonical,
  unmergeLogPattern,
  getLogPatternGroupMembers,
  getLogPatternMemberCount,
)
where

import Data.Vector qualified as V
import Database.PostgreSQL.Entity (_selectWhere)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.ErrorPatterns (ErrorPattern, ErrorPatternId (..))
import Models.Apis.LogPatterns (LogPattern, LogPatternId)
import Models.Projects.Projects qualified as Projects
import Relude hiding (id)
import System.Types (DB)


getUnembeddedErrorPatterns :: DB es => Projects.ProjectId -> Eff es [(ErrorPatternId, Text, Text)]
getUnembeddedErrorPatterns pid =
  PG.query
    [sql| SELECT id, error_type, message FROM apis.error_patterns
        WHERE project_id = ? AND embedding IS NULL AND merge_override = FALSE
        LIMIT 500 |]
    (Only pid)


getUnembeddedLogPatterns :: DB es => Projects.ProjectId -> Eff es [(LogPatternId, Text)]
getUnembeddedLogPatterns pid =
  PG.query
    [sql| SELECT id, log_pattern FROM apis.log_patterns
        WHERE project_id = ? AND embedding IS NULL AND merge_override = FALSE
        LIMIT 500 |]
    (Only pid)


updateErrorEmbeddings :: DB es => [(ErrorPatternId, [Float])] -> Eff es Int64
updateErrorEmbeddings [] = pure 0
updateErrorEmbeddings pairs =
  PG.execute
    [sql| UPDATE apis.error_patterns SET
          embedding = u.emb::float4[],
          embedding_at = NOW()
        FROM (SELECT unnest(?::uuid[]) AS id, unnest(?::float4[][]) AS emb) u
        WHERE apis.error_patterns.id = u.id |]
    (V.fromList ids, V.fromList embs)
  where
    (ids, embs) = unzip $ map (\(eid, e) -> (eid, PGArray e)) pairs


updateLogEmbeddings :: DB es => [(LogPatternId, [Float])] -> Eff es Int64
updateLogEmbeddings [] = pure 0
updateLogEmbeddings pairs =
  PG.execute
    [sql| UPDATE apis.log_patterns SET
          embedding = u.emb::float4[],
          embedding_at = NOW()
        FROM (SELECT unnest(?::bigint[]) AS id, unnest(?::float4[][]) AS emb) u
        WHERE apis.log_patterns.id = u.id |]
    (V.fromList ids, V.fromList embs)
  where
    (ids, embs) = unzip $ map (\(lid, e) -> (lid, PGArray e)) pairs


getCanonicalErrorPatterns :: DB es => Projects.ProjectId -> Eff es [(ErrorPatternId, [Float])]
getCanonicalErrorPatterns pid =
  map (\(eid, PGArray e) -> (eid, e))
    <$> PG.query
      [sql| SELECT id, embedding FROM apis.error_patterns
        WHERE project_id = ? AND canonical_id IS NULL
          AND embedding IS NOT NULL AND merge_override = FALSE |]
      (Only pid)


getCanonicalLogPatterns :: DB es => Projects.ProjectId -> Eff es [(LogPatternId, [Float])]
getCanonicalLogPatterns pid =
  map (\(lid, PGArray e) -> (lid, e))
    <$> PG.query
      [sql| SELECT id, embedding FROM apis.log_patterns
        WHERE project_id = ? AND canonical_id IS NULL
          AND embedding IS NOT NULL AND merge_override = FALSE |]
      (Only pid)


assignErrorsToCanonical :: DB es => [(ErrorPatternId, ErrorPatternId)] -> Eff es Int64
assignErrorsToCanonical [] = pure 0
assignErrorsToCanonical pairs =
  PG.execute
    [sql| UPDATE apis.error_patterns SET canonical_id = u.canonical
        FROM (SELECT unnest(?::uuid[]) AS id, unnest(?::uuid[]) AS canonical) u
        WHERE apis.error_patterns.id = u.id |]
    (V.fromList pids, V.fromList cids)
  where
    (pids, cids) = unzip pairs


assignLogsToCanonical :: DB es => [(LogPatternId, LogPatternId)] -> Eff es Int64
assignLogsToCanonical [] = pure 0
assignLogsToCanonical pairs =
  PG.execute
    [sql| UPDATE apis.log_patterns SET canonical_id = u.canonical
        FROM (SELECT unnest(?::bigint[]) AS id, unnest(?::bigint[]) AS canonical) u
        WHERE apis.log_patterns.id = u.id |]
    (V.fromList pids, V.fromList cids)
  where
    (pids, cids) = unzip pairs


unmergeErrorPattern :: DB es => ErrorPatternId -> Eff es Int64
unmergeErrorPattern eid =
  PG.execute
    [sql| UPDATE apis.error_patterns SET merge_override = TRUE, canonical_id = NULL WHERE id = ? |]
    (Only eid)


unmergeLogPattern :: DB es => LogPatternId -> Eff es Int64
unmergeLogPattern lid =
  PG.execute
    [sql| UPDATE apis.log_patterns SET merge_override = TRUE, canonical_id = NULL WHERE id = ? |]
    (Only lid)


getErrorPatternGroupMembers :: DB es => ErrorPatternId -> Eff es [ErrorPattern]
getErrorPatternGroupMembers eid =
  PG.query (_selectWhere @ErrorPattern [[field| canonical_id |]] <> " ORDER BY updated_at DESC") (Only eid)


getLogPatternGroupMembers :: DB es => LogPatternId -> Eff es [LogPattern]
getLogPatternGroupMembers lid =
  PG.query (_selectWhere @LogPattern [[field| canonical_id |]] <> " ORDER BY last_seen_at DESC") (Only lid)


getErrorPatternMemberCount :: DB es => ErrorPatternId -> Eff es Int
getErrorPatternMemberCount eid =
  fromMaybe 0
    . fmap fromOnly
    . listToMaybe
    <$> PG.query
      [sql| SELECT COUNT(*)::int FROM apis.error_patterns WHERE canonical_id = ? |]
      (Only eid)


getLogPatternMemberCount :: DB es => LogPatternId -> Eff es Int
getLogPatternMemberCount lid =
  fromMaybe 0
    . fmap fromOnly
    . listToMaybe
    <$> PG.query
      [sql| SELECT COUNT(*)::int FROM apis.log_patterns WHERE canonical_id = ? |]
      (Only lid)
