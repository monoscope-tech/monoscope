-- | In-memory cache for propagating session/user identity across all spans in a trace.
module Pkg.TraceSessionCache (
  TraceSessionInfo (..),
  TraceSessionKey,
  TraceSessionCache,
  newTraceSessionCache,
  lookupAndStamp,
  evictStaleEntries,
) where

import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, IOE, (:>))
import Effectful.Time qualified as Time
import Relude

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), atMapText)
import Pkg.DeriveUtils (AesonText (..), unAesonTextMaybe)
import Utils (jsonToMap, nestedJsonFromDotNotation)


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
-- Uses nestedJsonFromDotNotation so dotted keys (e.g. "session.id") become nested JSON
-- matching the structure produced by OTLP ingestion, which atMapText expects for lookup.
stampSpan :: TraceSessionInfo -> OtelLogsAndSpans -> OtelLogsAndSpans
stampSpan info span_ =
  let am = fromMaybe Map.empty (unAesonTextMaybe span_.attributes)
      pairs =
        catMaybes
          [ ("session.id",) . AE.String <$> info.sessionId
          , ("user.id",) . AE.String <$> info.userId
          , ("user.email",) . AE.String <$> info.userEmail
          , ("user.name",) . AE.String <$> info.userName
          , ("user.full_name",) . AE.String <$> info.userFullName
          ]
      inserts = fromMaybe Map.empty $ jsonToMap $ nestedJsonFromDotNotation pairs
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
        fresh = HM.filter (\info -> diffUTCTime now info.lastAccessedAt <= threshold) m
        before = HM.size m
     in if HM.size fresh <= maxEntries
          then (fresh, before - HM.size fresh)
          else
            let sorted = sortOn ((.lastAccessedAt) . snd) (HM.toList fresh)
                kept = HM.fromList $ drop (HM.size fresh - maxEntries) sorted
             in (kept, before - HM.size kept)
