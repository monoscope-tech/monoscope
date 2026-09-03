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
  -- Shape-keyed review loop
  ErrorGroupReview (..),
  ErrorShapeGroup (..),
  getErrorShapeGroups,
  recordErrorGroupReviews,
  getErrorGroupReviews,
  markErrorGroupRefuted,
  markErrorGroupApplied,
  revertErrorGroupApply,
  getQuarantinedErrorMerges,
  getReviewCursor,
  canonicalForAppliedShape,
  errorPatternsMissingShape,
  setErrorShapeHashes,
  learnedErrorMasks,
  insertLearnedErrorMask,
  errorMessagesForMaskCheck,
  rekeyErrorReviewShape,
  setReviewCursor,
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
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.ErrorPatterns (ErrorPattern, ErrorPatternId)
import Models.Apis.LogPatterns (LogPattern, LogPatternId)
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (selectFrom, showPGFloatArray)
import Pkg.ErrorFingerprint qualified as EF
import Pkg.PatternMerge (embeddingTextForError)
import Relude
import System.Types (DB)


getUnembeddedErrorPatterns :: DB es => Projects.ProjectId -> Eff es [(ErrorPatternId, Text, Text)]
getUnembeddedErrorPatterns pid =
  Hasql.interp
    [HI.sql| SELECT id, error_type, message FROM apis.error_patterns
        WHERE project_id = #{pid} AND embedding IS NULL AND merge_override = FALSE
        LIMIT 500 |]


updateErrorEmbeddings :: DB es => [(ErrorPatternId, [Float])] -> Eff es Int64
updateErrorEmbeddings [] = pure 0
updateErrorEmbeddings pairs =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET embedding = u.emb::float4[], embedding_at = NOW()
        FROM ROWS FROM (unnest(#{ids}::uuid[]), unnest(#{embs}::text[])) AS u(id, emb)
        WHERE apis.error_patterns.id = u.id |]
  where
    (ids, embs) = second (map showPGFloatArray) $ unzip pairs


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
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET canonical_id = u.canonical
        FROM (SELECT unnest(#{pids}::uuid[]) AS id, unnest(#{cids}::uuid[]) AS canonical) u
        WHERE apis.error_patterns.id = u.id |]


setCanonicalId :: DB es => ErrorPatternId -> ErrorPatternId -> Eff es Int64
setCanonicalId patternId canonicalId =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET canonical_id = #{canonicalId} WHERE id = #{patternId} AND merge_override = FALSE |]


unmergeErrorPattern :: DB es => ErrorPatternId -> Eff es Int64
unmergeErrorPattern pid =
  Hasql.interpExecute [HI.sql| UPDATE apis.error_patterns SET merge_override = TRUE, canonical_id = NULL WHERE id = #{pid} |]


getErrorPatternGroupMembers :: DB es => ErrorPatternId -> Eff es [ErrorPattern]
getErrorPatternGroupMembers eid =
  Hasql.interp (selectFrom @ErrorPattern <> [HI.sql| WHERE canonical_id = #{eid} ORDER BY updated_at DESC |])


fetchErrorTexts :: DB es => [ErrorPatternId] -> Eff es (Map ErrorPatternId Text)
fetchErrorTexts [] = pure mempty
fetchErrorTexts ids =
  Map.fromList
    . map (\(eid, et, msg) -> (eid, embeddingTextForError et msg))
    <$> Hasql.interp [HI.sql| SELECT id, error_type, message FROM apis.error_patterns WHERE id = ANY(#{ids}) |]


getUnembeddedLogPatterns :: DB es => Projects.ProjectId -> Eff es [(LogPatternId, Text)]
getUnembeddedLogPatterns pid =
  Hasql.interp
    [HI.sql| SELECT id, log_pattern FROM apis.log_patterns
        WHERE project_id = #{pid} AND embedding IS NULL AND merge_override = FALSE
        ORDER BY id LIMIT 500 |]


updateLogEmbeddings :: DB es => [(LogPatternId, [Float])] -> Eff es Int64
updateLogEmbeddings [] = pure 0
updateLogEmbeddings pairs =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.log_patterns SET embedding = u.emb::float4[], embedding_at = NOW()
        FROM ROWS FROM (unnest(#{ids}::bigint[]), unnest(#{embs}::text[])) AS u(id, emb)
        WHERE apis.log_patterns.id = u.id |]
  where
    (ids, embs) = second (map showPGFloatArray) $ unzip pairs


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
  Hasql.interpExecute
    [HI.sql| UPDATE apis.log_patterns SET canonical_id = u.canonical
        FROM (SELECT unnest(#{pids}::bigint[]) AS id, unnest(#{cids}::bigint[]) AS canonical) u
        WHERE apis.log_patterns.id = u.id |]


unmergeLogPattern :: DB es => LogPatternId -> Eff es Int64
unmergeLogPattern lid =
  Hasql.interpExecute [HI.sql| UPDATE apis.log_patterns SET merge_override = TRUE, canonical_id = NULL WHERE id = #{lid} |]


getLogPatternGroupMembers :: DB es => LogPatternId -> Eff es [LogPattern]
getLogPatternGroupMembers lid =
  Hasql.interp (selectFrom @LogPattern <> [HI.sql| WHERE canonical_id = #{lid} ORDER BY last_seen_at DESC |])


fetchLogTexts :: DB es => [LogPatternId] -> Eff es (Map LogPatternId Text)
fetchLogTexts [] = pure mempty
fetchLogTexts ids =
  Map.fromList
    <$> Hasql.interp [HI.sql| SELECT id, log_pattern FROM apis.log_patterns WHERE id = ANY(#{ids}) |]


fetchLogSamples :: DB es => [LogPatternId] -> Eff es (Map LogPatternId Text)
fetchLogSamples [] = pure mempty
fetchLogSamples ids =
  Map.fromList
    . mapMaybe sequenceA
    <$> Hasql.interp [HI.sql| SELECT id, sample_message FROM apis.log_patterns WHERE id = ANY(#{ids}) |]


-- | One shape's worth of error patterns, as the review loop sees it.
--
-- The @sample@ is a normalised message, never the raw one: raw error text carries
-- whatever the customer's code put in it, and this is the value that ends up in an
-- LLM prompt.
data ErrorShapeGroup = ErrorShapeGroup
  { shapeKey :: Text
  , errorType :: Text
  , sample :: Text
  -- ^ RAW, as stored. Kept for logging and for choosing a representative; it must
  -- be run through 'EF.normalizeMessage' before it goes anywhere near a prompt,
  -- which the review job does at the point of the call.
  , memberIds :: V.Vector ErrorPatternId
  , memberCount :: Int64
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | A stored verdict plus the evidence accumulated for it.
data ErrorGroupReview = ErrorGroupReview
  { groupKey :: Text
  , membersHash :: Text
  , memberCount :: Int64
  , firstMemberCount :: Int64
  , verdict :: Text
  , confirmations :: Int64
  , survivedRefute :: Bool
  , appliedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


-- | Candidate groups: unmerged patterns sharing a shape, biggest first.
--
-- @shape_hash@ is populated write-once by the upsert, so a pattern joins the pool
-- the next time it occurs. Rows that never recur are not candidates and do not need
-- to be — there is nothing left for them to suppress.
getErrorShapeGroups :: DB es => Projects.ProjectId -> Text -> Int -> Eff es [ErrorShapeGroup]
getErrorShapeGroups pid afterKey lim =
  Hasql.interp
    [HI.sql| SELECT shape_hash, MIN(error_type), MIN(message), ARRAY_AGG(id ORDER BY created_at), COUNT(*)::int8
             FROM apis.error_patterns
             WHERE project_id = #{pid} AND shape_hash IS NOT NULL
               AND canonical_id IS NULL AND merge_override = FALSE
               AND shape_hash > #{afterKey}
             GROUP BY shape_hash
             ORDER BY shape_hash
             LIMIT #{lim} |]


-- | Record this pass's verdicts. Agreement with the previous pass is what counts as
-- confirmation; any disagreement resets the evidence rather than averaging it.
recordErrorGroupReviews :: DB es => Projects.ProjectId -> [(Text, Text, Int64, Text, Text)] -> Eff es ()
recordErrorGroupReviews _ [] = pass
recordErrorGroupReviews pid rows =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.error_group_reviews (project_id, group_key, members_hash, member_count, verdict, shape, confirmations, first_member_count)
             SELECT #{pid}, k, h, n, v, s, 1, n
             FROM unnest(#{keys}::text[], #{hashes}::text[], #{counts}::int8[], #{verdicts}::text[], #{shapes}::text[]) AS t(k, h, n, v, s)
             ON CONFLICT (project_id, group_key)
             DO UPDATE SET members_hash = EXCLUDED.members_hash, member_count = EXCLUDED.member_count,
                           verdict = EXCLUDED.verdict, shape = EXCLUDED.shape, created_at = NOW(),
                           confirmations = CASE
                             WHEN apis.error_group_reviews.verdict = EXCLUDED.verdict
                               THEN apis.error_group_reviews.confirmations + 1
                             ELSE 1 END,
                           first_member_count = CASE
                             WHEN apis.error_group_reviews.verdict = EXCLUDED.verdict
                               THEN apis.error_group_reviews.first_member_count
                               ELSE EXCLUDED.member_count END,
                           -- A changed verdict invalidates a refutation that was run
                           -- against the old one.
                           survived_refute = CASE
                             WHEN apis.error_group_reviews.verdict = EXCLUDED.verdict
                               THEN apis.error_group_reviews.survived_refute
                             ELSE FALSE END
             WHERE apis.error_group_reviews.applied_at IS NULL |]
  where
    keys = V.fromList [k | (k, _, _, _, _) <- rows]
    hashes = V.fromList [h | (_, h, _, _, _) <- rows]
    counts = V.fromList [n | (_, _, n, _, _) <- rows]
    verdicts = V.fromList [v | (_, _, _, v, _) <- rows]
    shapes = V.fromList [s | (_, _, _, _, s) <- rows]


getErrorGroupReviews :: DB es => Projects.ProjectId -> Eff es [ErrorGroupReview]
getErrorGroupReviews pid =
  Hasql.interp
    [HI.sql| SELECT group_key, members_hash, member_count::int8, first_member_count::int8,
                    verdict, confirmations::int8, survived_refute, applied_at
             FROM apis.error_group_reviews
             WHERE project_id = #{pid} AND reverted_at IS NULL |]


-- | Stamp the outcome of the refute pass. Survivors are eligible to apply; the rest
-- keep their row as the record that the model argued both ways.
markErrorGroupRefuted :: (DB es, Time.Time :> es) => Projects.ProjectId -> [Text] -> Bool -> Eff es ()
markErrorGroupRefuted _ [] _ = pass
markErrorGroupRefuted pid keys survived = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| UPDATE apis.error_group_reviews SET refuted_at = #{now}, survived_refute = #{survived}
             WHERE project_id = #{pid} AND group_key = ANY(#{V.fromList keys}) AND applied_at IS NULL |]


markErrorGroupApplied :: (DB es, Time.Time :> es) => Projects.ProjectId -> Text -> V.Vector ErrorPatternId -> Eff es ()
markErrorGroupApplied pid gkey cids = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| UPDATE apis.error_group_reviews SET applied_at = #{now}, applied_canonical_ids = #{cids}
             WHERE project_id = #{pid} AND group_key = #{gkey} |]


-- | Undo an applied merge the quarantine re-check refuted.
--
-- Nothing was deleted — errors have no destructive cleanup — so this only has to
-- un-assign. @merge_override@ then keeps the group out of every future pass: one the
-- model has both merged and disowned is not one to keep re-litigating.
revertErrorGroupApply :: (DB es, Time.Time :> es) => Projects.ProjectId -> Text -> V.Vector ErrorPatternId -> Eff es Int64
revertErrorGroupApply pid gkey cids = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| UPDATE apis.error_group_reviews SET reverted_at = #{now}
             WHERE project_id = #{pid} AND group_key = #{gkey} |]
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET canonical_id = NULL, merge_override = TRUE
             WHERE project_id = #{pid} AND canonical_id = ANY(#{cids}) |]


-- | Applied merges still inside their 24h quarantine, with the member text the
-- challenge prompt needs.
getQuarantinedErrorMerges :: DB es => Projects.ProjectId -> Eff es [(Text, Text, V.Vector Text, V.Vector ErrorPatternId)]
getQuarantinedErrorMerges pid =
  Hasql.interp
    [HI.sql| SELECT r.group_key, MIN(e.error_type), ARRAY_AGG(DISTINCT LEFT(e.message, 300)), r.applied_canonical_ids
             FROM apis.error_group_reviews r
             JOIN apis.error_patterns e
               ON e.project_id = r.project_id AND e.canonical_id = ANY(r.applied_canonical_ids)
             WHERE r.project_id = #{pid}
               AND r.applied_at IS NOT NULL AND r.reverted_at IS NULL
               AND r.applied_at > NOW() - INTERVAL '24 hours'
             GROUP BY r.group_key, r.applied_canonical_ids |]


getReviewCursor :: DB es => Projects.ProjectId -> Eff es Text
getReviewCursor pid =
  fromMaybe ""
    <$> Hasql.interpOne [HI.sql| SELECT last_shape_key FROM apis.error_review_cursor WHERE project_id = #{pid} |]


-- | Advance the sweep. An empty key means the corpus was walked to the end and the
-- next run starts over — the sweep is a loop, not a one-shot, so a shape that gains
-- members later is re-examined.
setReviewCursor :: (DB es, Time.Time :> es) => Projects.ProjectId -> Text -> Eff es ()
setReviewCursor pid k = do
  now <- Time.currentTime
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.error_review_cursor (project_id, last_shape_key, updated_at, swept_at)
             VALUES (#{pid}, #{k}, #{now}, CASE WHEN #{k} = '' THEN #{now} ELSE NULL END)
             ON CONFLICT (project_id) DO UPDATE
               SET last_shape_key = EXCLUDED.last_shape_key, updated_at = EXCLUDED.updated_at,
                   swept_at = COALESCE(EXCLUDED.swept_at, apis.error_review_cursor.swept_at) |]


-- | The canonical an arriving pattern should be folded into, if a review has
-- already been applied for its shape.
--
-- This is the half of the loop that runs at ingest, and the only one that stops a
-- notification rather than tidying up after it: everything else merges patterns
-- whose issues already exist, and an issue that exists has already notified.
--
-- Requires the review to be applied and un-reverted. A recorded verdict is not
-- authority — only one that cleared the evidence gate and survived the refutation
-- reached @applied_at@.
canonicalForAppliedShape :: DB es => Projects.ProjectId -> Text -> Maybe Text -> Eff es (Maybe ErrorPatternId)
canonicalForAppliedShape pid shapeH service =
  Hasql.interpOne
    [HI.sql| SELECT e.id
             FROM apis.error_group_reviews r
             JOIN apis.error_patterns e ON e.id = ANY(r.applied_canonical_ids)
             WHERE r.project_id = #{pid} AND r.group_key = #{shapeH}
               AND r.applied_at IS NOT NULL AND r.reverted_at IS NULL
               AND e.canonical_id IS NULL AND e.merge_override = FALSE
               AND e.service IS NOT DISTINCT FROM #{service}
             LIMIT 1 |]


-- | Patterns still missing a shape, oldest first.
--
-- The upsert fills @shape_hash@ write-once, which covers a row the next time it
-- occurs — enough for ingest-time suppression, and useless for review: a shape needs
-- two populated members to be a candidate, so a corpus that predates the column is
-- invisible to the loop. On prod that was 89 rows of 23,389.
errorPatternsMissingShape :: DB es => Projects.ProjectId -> Int -> Eff es [(ErrorPatternId, Text, Text)]
errorPatternsMissingShape pid lim =
  Hasql.interp
    [HI.sql| SELECT id, error_type, message FROM apis.error_patterns
             WHERE project_id = #{pid} AND shape_hash IS NULL
             ORDER BY created_at
             LIMIT #{lim} |]


setErrorShapeHashes :: DB es => [(ErrorPatternId, Text)] -> Eff es Int64
setErrorShapeHashes [] = pure 0
setErrorShapeHashes pairs =
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_patterns SET shape_hash = u.sh
             FROM (SELECT unnest(#{V.fromList ids}::uuid[]) AS id, unnest(#{V.fromList shapes}::text[]) AS sh) u
             WHERE apis.error_patterns.id = u.id |]
  where
    (ids, shapes) = unzip pairs


-- | Live masks for a project, newest first.
learnedErrorMasks :: DB es => Projects.ProjectId -> Eff es [(Text, EF.ErrorMask)]
learnedErrorMasks pid =
  map (\(et, p, sfx, n) -> (et, EF.ErrorMask p sfx (fromIntegral @Int64 n)))
    <$> Hasql.interp
      [HI.sql| SELECT error_type, prefix, suffix, min_var_len::int8 FROM apis.learned_error_masks
               WHERE project_id = #{pid} AND retired_at IS NULL |]


insertLearnedErrorMask :: DB es => Projects.ProjectId -> Text -> EF.ErrorMask -> Text -> Eff es Int64
insertLearnedErrorMask pid etype mask gkey =
  Hasql.interpExecute
    [HI.sql| INSERT INTO apis.learned_error_masks (project_id, error_type, prefix, suffix, min_var_len, group_key)
             VALUES (#{pid}, #{etype}, #{mask.prefix}, #{mask.suffix}, #{fromIntegral @Int @Int64 mask.minVarLen}, #{gkey})
             ON CONFLICT (project_id, error_type, prefix, suffix) DO NOTHING |]


-- | Every (error_type, message) a mask candidate would have to survive, and the
-- shape the review loop currently files it under.
--
-- The safety test needs both: a mask is refused when applying it would give two
-- messages the same shape that the judge deliberately kept apart.
errorMessagesForMaskCheck :: DB es => Projects.ProjectId -> Text -> Eff es [(Text, Text)]
errorMessagesForMaskCheck pid etype =
  Hasql.interp
    [HI.sql| SELECT e.message, COALESCE(e.shape_hash, '')
             FROM apis.error_patterns e
             WHERE e.project_id = #{pid} AND e.error_type = #{etype}
               AND e.created_at > NOW() - INTERVAL '30 days'
             LIMIT 5000 |]


-- | Re-file a review and its canonical under a new shape.
--
-- A promoted mask changes what the shape of its members is, so the review that
-- authorised it and the row the ingest gate resolves against both have to move with
-- it. Without this the gate looks up arrivals under the post-mask shape, finds a
-- review filed under the pre-mask one, and the rule silently does nothing — which is
-- the failure mode this codebase specialises in.
rekeyErrorReviewShape :: DB es => Projects.ProjectId -> Text -> Text -> Eff es Int64
rekeyErrorReviewShape pid oldKey newKey = do
  void
    $ Hasql.interpExecute
      [HI.sql| UPDATE apis.error_patterns SET shape_hash = #{newKey}
               WHERE project_id = #{pid} AND shape_hash = #{oldKey} |]
  Hasql.interpExecute
    [HI.sql| UPDATE apis.error_group_reviews SET group_key = #{newKey}
             WHERE project_id = #{pid} AND group_key = #{oldKey} |]
