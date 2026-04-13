-- | In-memory cache for propagating session/user identity across all spans in a trace.
module Pkg.TraceSessionCache (
  TraceSessionInfo (..),
  TraceSessionKey,
  TraceSessionCache,
  newTraceSessionCache,
  lookupAndStamp,
  evictStaleEntries,
  backfillSessionAttributes,
) where

import Data.Effectful.Hasql qualified as Hasql
import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, IOE, (:>))
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Relude

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), atMapText)
import Pkg.DeriveUtils (AesonText (..), DB, unAesonTextMaybe)


data TraceSessionInfo = TraceSessionInfo
  { sessionId :: !(Maybe Text)
  , userId :: !(Maybe Text)
  , userEmail :: !(Maybe Text)
  , userName :: !(Maybe Text)
  , userFullName :: !(Maybe Text)
  , lastAccessedAt :: !UTCTime
  }


-- | Merge: keep first non-Nothing per field, latest access time.
--
-- >>> import Data.Time.Clock (UTCTime(..))
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> let t = UTCTime (fromGregorian 2025 1 1) 0
-- >>> let a = TraceSessionInfo (Just "s1") Nothing Nothing Nothing Nothing t
-- >>> let b = TraceSessionInfo Nothing (Just "u1") Nothing Nothing Nothing t
-- >>> (a <> b).sessionId
-- Just "s1"
-- >>> (a <> b).userId
-- Just "u1"
instance Semigroup TraceSessionInfo where
  a <> b =
    TraceSessionInfo
      { sessionId = a.sessionId <|> b.sessionId
      , userId = a.userId <|> b.userId
      , userEmail = a.userEmail <|> b.userEmail
      , userName = a.userName <|> b.userName
      , userFullName = a.userFullName <|> b.userFullName
      , lastAccessedAt = max a.lastAccessedAt b.lastAccessedAt
      }


type TraceSessionKey = (Text, Text) -- (project_id, trace_id)
type TraceSessionCache = IORef (HM.HashMap TraceSessionKey TraceSessionInfo)


newTraceSessionCache :: IO TraceSessionCache
newTraceSessionCache = newIORef HM.empty


extractSessionInfo :: UTCTime -> OtelLogsAndSpans -> Maybe TraceSessionInfo
extractSessionInfo now span_ =
  let am = unAesonTextMaybe span_.attributes
      sId = atMapText "session.id" am
      uId = atMapText "user.id" am
      uEmail = atMapText "user.email" am
      uName = atMapText "user.name" am
      uFull = atMapText "user.full_name" am
      info = TraceSessionInfo{sessionId = sId, userId = uId, userEmail = uEmail, userName = uName, userFullName = uFull, lastAccessedAt = now}
   in info <$ (sId <|> uId <|> uEmail <|> uName <|> uFull)


traceKey :: OtelLogsAndSpans -> Maybe TraceSessionKey
traceKey span_ = (span_.project_id,) <$> (span_.context >>= (.trace_id))


-- | Insert session/user attributes into the span's attribute Map without overwriting existing keys.
stampSpan :: TraceSessionInfo -> OtelLogsAndSpans -> OtelLogsAndSpans
stampSpan info span_ =
  let am = fromMaybe Map.empty (unAesonTextMaybe span_.attributes)
      inserts =
        Map.fromList
          $ catMaybes
            [ ("session.id",) . AE.String <$> info.sessionId
            , ("user.id",) . AE.String <$> info.userId
            , ("user.email",) . AE.String <$> info.userEmail
            , ("user.name",) . AE.String <$> info.userName
            , ("user.full_name",) . AE.String <$> info.userFullName
            ]
   in span_{attributes = Just (AesonText (am `Map.union` inserts))}


-- | Two-pass: (1) collect session info from spans, upsert cache; (2) stamp spans missing info.
lookupAndStamp :: (IOE :> es, Time.Time :> es) => TraceSessionCache -> V.Vector OtelLogsAndSpans -> Eff es (V.Vector OtelLogsAndSpans)
lookupAndStamp cache spans = do
  now <- Time.currentTime
  -- Pass 1: collect and upsert
  let infos = V.mapMaybe (\s -> (,) <$> traceKey s <*> extractSessionInfo now s) spans
  unless (V.null infos)
    $ atomicModifyIORef' cache \m ->
      (V.foldl' (\acc (k, v) -> HM.insertWith (<>) k v acc) m infos, ())
  -- Pass 2: stamp from cache
  cached <- readIORef cache
  let stamp s = case traceKey s >>= (`HM.lookup` cached) of
        Nothing -> s
        Just info -> stampSpan info s
  pure $ V.map stamp spans


-- | Evict entries idle > maxIdleSecs, then LRU down to maxEntries cap. Returns eviction count.
evictStaleEntries :: TraceSessionCache -> Int -> Int -> UTCTime -> IO Int
evictStaleEntries cache maxIdleSecs maxEntries now =
  atomicModifyIORef' cache \m ->
    let threshold = fromIntegral maxIdleSecs :: NominalDiffTime
        fresh = HM.filter (\info -> diffUTCTime now info.lastAccessedAt < threshold) m
        before = HM.size m
     in if HM.size fresh <= maxEntries
          then (fresh, before - HM.size fresh)
          else
            let sorted = sortOn ((.lastAccessedAt) . snd) (HM.toList fresh)
                kept = HM.fromList $ drop (HM.size fresh - maxEntries) sorted
             in (kept, before - HM.size kept)


-- | Backfill session/user attrs from sibling spans within recent traces (catches cross-batch edge cases).
backfillSessionAttributes :: DB es => Eff es Int
backfillSessionAttributes = do
  rows :: [Int] <-
    Hasql.interp
      [HI.sql|
        WITH session_sources AS (
          SELECT context___trace_id, project_id,
                 MAX(attributes___session___id)     FILTER (WHERE attributes___session___id IS NOT NULL) AS sid,
                 MAX(attributes___user___id)        FILTER (WHERE attributes___user___id IS NOT NULL) AS uid,
                 MAX(attributes___user___email)     FILTER (WHERE attributes___user___email IS NOT NULL) AS uemail,
                 MAX(attributes___user___name)      FILTER (WHERE attributes___user___name IS NOT NULL) AS uname,
                 MAX(attributes___user___full_name) FILTER (WHERE attributes___user___full_name IS NOT NULL) AS ufull
          FROM otel_logs_and_spans
          WHERE timestamp >= NOW() - INTERVAL '15 minutes'
            AND (attributes___session___id IS NOT NULL OR attributes___user___id IS NOT NULL)
          GROUP BY context___trace_id, project_id
        )
        UPDATE otel_logs_and_spans o
        SET attributes___session___id     = COALESCE(o.attributes___session___id, s.sid),
            attributes___user___id        = COALESCE(o.attributes___user___id, s.uid),
            attributes___user___email     = COALESCE(o.attributes___user___email, s.uemail),
            attributes___user___name      = COALESCE(o.attributes___user___name, s.uname),
            attributes___user___full_name = COALESCE(o.attributes___user___full_name, s.ufull)
        FROM session_sources s
        WHERE o.context___trace_id = s.context___trace_id
          AND o.project_id = s.project_id
          AND o.timestamp >= NOW() - INTERVAL '15 minutes'
          AND (o.attributes___session___id IS NULL OR o.attributes___user___id IS NULL
            OR o.attributes___user___email IS NULL OR o.attributes___user___name IS NULL
            OR o.attributes___user___full_name IS NULL)
          AND (s.sid IS NOT NULL OR s.uid IS NOT NULL)
        RETURNING 1
      |]
  pure $ length rows
