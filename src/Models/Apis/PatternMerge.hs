module Models.Apis.PatternMerge (
  -- Error pattern operations
  getUnembeddedErrorPatterns,
  getCanonicalErrorPatterns,
  updateErrorEmbeddings,
  assignErrorsToCanonical,
  unmergeErrorPattern,
  getErrorPatternGroupMembers,
  fetchErrorTexts,
  setCanonicalId,
  -- Log pattern operations
  getUnembeddedLogPatterns,
  getCanonicalLogPatterns,
  updateLogEmbeddings,
  assignLogsToCanonical,
  unmergeLogPattern,
  getLogPatternGroupMembers,
  fetchLogTexts,
  fetchLogSamples,
)
where

import Data.Effectful.Hasql qualified as Hasql
import Data.Map.Lazy qualified as Map
import Data.Vector qualified as V
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Apis.ErrorPatterns (ErrorPattern, ErrorPatternId (..))
import Models.Apis.LogPatterns (LogPattern, LogPatternId)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (showPGFloatArray)
import Pkg.PatternMerge (embeddingTextForError)
import Relude hiding (id)
import System.Types (DB)


-- Error pattern operations

getUnembeddedErrorPatterns :: DB es => Projects.ProjectId -> Eff es [(ErrorPatternId, Text, Text)]
getUnembeddedErrorPatterns pid =
  Hasql.interp
    [HI.sql| SELECT id, error_type, message FROM apis.error_patterns
        WHERE project_id = #{pid} AND embedding IS NULL AND merge_override = FALSE
        LIMIT 500 |]


updateErrorEmbeddings :: DB es => [(ErrorPatternId, [Float])] -> Eff es Int64
updateErrorEmbeddings [] = pure 0
updateErrorEmbeddings pairs = do
  let (ids, embs) = unzip $ map (second showPGFloatArray) pairs
      vIds = V.fromList ids
      vEmbs = V.fromList embs
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET embedding = u.emb::float4[], embedding_at = NOW()
        FROM ROWS FROM (unnest(#{vIds}::uuid[]), unnest(#{vEmbs}::text[])) AS u(id, emb)
        WHERE apis.error_patterns.id = u.id |]


getCanonicalErrorPatterns :: DB es => Projects.ProjectId -> Eff es [(ErrorPatternId, [Float])]
getCanonicalErrorPatterns pid =
  map (second V.toList)
    <$> Hasql.interp
      [HI.sql| SELECT id, embedding FROM apis.error_patterns
        WHERE project_id = #{pid} AND canonical_id IS NULL
          AND embedding IS NOT NULL AND merge_override = FALSE
        LIMIT 10000 |]


assignErrorsToCanonical :: DB es => [(ErrorPatternId, ErrorPatternId)] -> Eff es Int64
assignErrorsToCanonical [] = pure 0
assignErrorsToCanonical pairs = do
  let (pids, cids) = unzip pairs
      vPids = V.fromList pids
      vCids = V.fromList cids
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET canonical_id = u.canonical
        FROM (SELECT unnest(#{vPids}::uuid[]) AS id, unnest(#{vCids}::uuid[]) AS canonical) u
        WHERE apis.error_patterns.id = u.id |]


setCanonicalId :: DB es => ErrorPatternId -> ErrorPatternId -> Eff es Int64
setCanonicalId patternId canonicalId =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET canonical_id = #{canonicalId} WHERE id = #{patternId} AND merge_override = FALSE |]


unmergeErrorPattern :: DB es => ErrorPatternId -> Eff es Int64
unmergeErrorPattern pid =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET merge_override = TRUE, canonical_id = NULL WHERE id = #{pid} |]


getErrorPatternGroupMembers :: DB es => ErrorPatternId -> Eff es [ErrorPattern]
getErrorPatternGroupMembers eid =
  Hasql.interp [HI.sql| SELECT * FROM apis.error_patterns WHERE canonical_id = #{eid} ORDER BY updated_at DESC |]


fetchErrorTexts :: DB es => [ErrorPatternId] -> Eff es (Map ErrorPatternId Text)
fetchErrorTexts [] = pure mempty
fetchErrorTexts ids = do
  let vIds = V.fromList ids
  Map.fromList
    . map (\(eid, et, msg) -> (eid, embeddingTextForError et msg))
    <$> Hasql.interp [HI.sql| SELECT id, error_type, message FROM apis.error_patterns WHERE id = ANY(#{vIds}) |]


-- Log pattern operations

getUnembeddedLogPatterns :: DB es => Projects.ProjectId -> Eff es [(LogPatternId, Text)]
getUnembeddedLogPatterns pid =
  Hasql.interp
    [HI.sql| SELECT id, log_pattern FROM apis.log_patterns
        WHERE project_id = #{pid} AND embedding IS NULL AND merge_override = FALSE
        ORDER BY id LIMIT 500 |]


updateLogEmbeddings :: DB es => [(LogPatternId, [Float])] -> Eff es Int64
updateLogEmbeddings [] = pure 0
updateLogEmbeddings pairs = do
  let (ids, embs) = unzip $ map (second showPGFloatArray) pairs
      vIds = V.fromList ids
      vEmbs = V.fromList embs
  Hasql.interpExecute
    [HI.sql| UPDATE apis.log_patterns SET embedding = u.emb::float4[], embedding_at = NOW()
        FROM ROWS FROM (unnest(#{vIds}::bigint[]), unnest(#{vEmbs}::text[])) AS u(id, emb)
        WHERE apis.log_patterns.id = u.id |]


getCanonicalLogPatterns :: DB es => Projects.ProjectId -> Eff es [(LogPatternId, [Float])]
getCanonicalLogPatterns pid =
  map (second V.toList)
    <$> Hasql.interp
      [HI.sql| SELECT id, embedding FROM apis.log_patterns
        WHERE project_id = #{pid} AND canonical_id IS NULL
          AND embedding IS NOT NULL AND merge_override = FALSE
        LIMIT 10000 |]


assignLogsToCanonical :: DB es => [(LogPatternId, LogPatternId)] -> Eff es Int64
assignLogsToCanonical [] = pure 0
assignLogsToCanonical pairs = do
  let (pids, cids) = unzip pairs
      vPids = V.fromList pids
      vCids = V.fromList cids
  Hasql.interpExecute
    [HI.sql| UPDATE apis.log_patterns SET canonical_id = u.canonical
        FROM (SELECT unnest(#{vPids}::bigint[]) AS id, unnest(#{vCids}::bigint[]) AS canonical) u
        WHERE apis.log_patterns.id = u.id |]


unmergeLogPattern :: DB es => LogPatternId -> Eff es Int64
unmergeLogPattern lid =
  Hasql.interpExecute [HI.sql| UPDATE apis.log_patterns SET merge_override = TRUE, canonical_id = NULL WHERE id = #{lid} |]


getLogPatternGroupMembers :: DB es => LogPatternId -> Eff es [LogPattern]
getLogPatternGroupMembers lid =
  Hasql.interp [HI.sql| SELECT * FROM apis.log_patterns WHERE canonical_id = #{lid} ORDER BY last_seen_at DESC |]


fetchLogTexts :: DB es => [LogPatternId] -> Eff es (Map LogPatternId Text)
fetchLogTexts [] = pure mempty
fetchLogTexts ids = do
  let vIds = V.fromList ids
  Map.fromList
    <$> Hasql.interp [HI.sql| SELECT id, log_pattern FROM apis.log_patterns WHERE id = ANY(#{vIds}) |]


fetchLogSamples :: DB es => [LogPatternId] -> Eff es (Map LogPatternId Text)
fetchLogSamples [] = pure mempty
fetchLogSamples ids = do
  let vIds = V.fromList ids
  Map.fromList
    . mapMaybe (\(pid, mSample) -> (pid,) <$> mSample)
    <$> Hasql.interp [HI.sql| SELECT id, sample_message FROM apis.log_patterns WHERE id = ANY(#{vIds}) |]
