-- | The Live Tail tab: page, subscription routes, and the SSE stream itself.
--
-- Live Tail is the Explorer's first tab but not its landing page — Events stays the default,
-- because "what happened" is the question people arrive with and "what is happening" is the
-- one they choose.
--
-- The stream is Server-Sent Events rather than a WebSocket on purpose: the traffic is
-- one-directional (the browser's only input is the subscription it registered over plain
-- HTTP), SSE reconnects on its own, and it survives proxies and corporate networks that a
-- WebSocket upgrade does not. See "Pkg.LiveTail" for how a row gets from the ingest pod into
-- the queue this handler drains.
module Pages.LogExplorer.LiveTail (
  EventStream,
  SseHeaders,
  liveTailGetH,
  liveTailRegisterH,
  liveTailStreamH,
  liveTailRenewH,
  liveTailDeleteH,
  LiveTailGet (..),
  LiveTailPageData (..),
  RegisterResponse (..),
) where

import Control.Exception.Safe (finally)
import Data.Aeson qualified as AE
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Time (UTCTime, addUTCTime)
import Deriving.Aeson.Stock qualified as DAE
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Lucid
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Media qualified as M
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pkg.DeriveUtils (idFromText)
import Pkg.LiveTail qualified as LT
import Pkg.Metrics qualified as Metrics
import Relude
import Servant qualified
import Servant.API (Accept (..), Headers, MimeRender (..), addHeader)
import Servant.Types.SourceT (SourceT (..), StepT (..))
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (explorerNavTabs_)


-- ---------------------------------------------------------------------------------------
-- SSE content type
-- ---------------------------------------------------------------------------------------

-- | @text/event-stream@ as a Servant content type.
--
-- Hand-rolled rather than pulled from @servant-event-stream@: the entire surface is one
-- 'Accept' instance and one 'MimeRender', and a dependency whose whole value is those few
-- lines is a dependency that will one day block a Servant upgrade.
data EventStream


instance Accept EventStream where
  contentType _ = "text" M.// "event-stream"


instance MimeRender EventStream Builder where
  mimeRender _ = toLazyByteString


-- | The headers that decide whether an SSE response actually streams.
--
-- Every hop between this handler and the browser would rather buffer than forward: a CDN caches
-- it, nginx holds it in a proxy buffer, a "helpful" transforming proxy re-chunks it. The result
-- is the failure that looks like nothing at all — the tab says @live@, the connection is open,
-- and rows arrive in a clump minutes later or never.
--
-- @no-transform@ is the half people forget: @no-cache@ alone still permits an intermediary to
-- re-encode the body, which is what re-introduces buffering. @X-Accel-Buffering@ is nginx's
-- (and several ingress controllers') opt-out, ignored harmlessly everywhere else.
--
-- Heartbeats every ten seconds mask most of this, which is precisely why it went unnoticed:
-- they keep the connection alive but do nothing about a hop that batches what it forwards.
-- See also the @text/event-stream@ exclusion in "System.Server"'s gzip settings — compression
-- is the one buffering hop headers cannot talk us out of.
type SseHeaders =
  Headers
    '[ Servant.Header "Cache-Control" Text
     , Servant.Header "X-Accel-Buffering" Text
     ]


-- | One SSE frame.
--
-- Every line of the payload needs its own @data:@ prefix — the detail that silently truncates
-- a message at the first newline when this is written inline at a call site.
sseFrame :: Text -> AE.Value -> Builder
sseFrame event payload =
  byteString (encodeUtf8 ("event: " <> event <> "\n"))
    <> foldMap dataLine (LBS.split 10 (AE.encode payload))
    <> byteString "\n"
  where
    dataLine l = byteString "data: " <> byteString (toStrict l) <> byteString "\n"


-- | A comment frame. Proxies buffer a stream that has said nothing for a while; this keeps the
-- connection observably alive without being an event the browser has to filter out.
sseComment :: Text -> Builder
sseComment t = byteString (encodeUtf8 (": " <> t <> "\n\n"))


-- ---------------------------------------------------------------------------------------
-- Subscription routes
-- ---------------------------------------------------------------------------------------

data RegisterResponse = RegisterResponse
  { subscriptionId :: Text
  , streamUrl :: Text
  , expiresAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake RegisterResponse


-- | Register a lease.
liveTailRegisterH :: Projects.ProjectId -> LT.NewSubscription -> ATAuthCtx (RespHeaders RegisterResponse)
liveTailRegisterH pid body = do
  sess <- Projects.getSession
  appCtx <- Reader.ask @AuthContext
  let scope = LT.scopeFor body
  whenLeft_ (LT.compileQuery (fromMaybe "" body.query)) refuse
  now <- Time.currentTime
  let expiresAt = addUTCTime (fromIntegral LT.leaseSecs) now
  -- The limits are checked inside the INSERT, not before it: a separate count would leave a
  -- window for other registrations to land between the check and the write.
  sid <-
    LT.insertSubscription pid sess.user.id body scope LT.maxPerUser LT.maxPerProject expiresAt
      >>= (`whenNothing` (refuse =<< limitHit pid sess.user.id appCtx))
  addRespHeaders
    RegisterResponse
      { subscriptionId = sid.toText
      , streamUrl = "/p/" <> pid.toText <> "/live_tail/subscriptions/" <> sid.toText <> "/stream"
      , expiresAt
      }
  where
    refuse :: LT.RegisterError -> ATAuthCtx a
    refuse e = Error.throwError (jsonError (status e) (LT.registerErrorMessage e))
    status = \case
      LT.TooManySubscriptionsForUser _ -> Servant.err409
      LT.TooManySubscriptionsForProject _ -> Servant.err409
      _ -> Servant.err400


-- | Which cap refused the insert. Read only on the failure path — the numbers are for the
-- message, so slight staleness costs nothing and the success path stays one query.
limitHit :: Projects.ProjectId -> Projects.UserId -> AuthContext -> ATAuthCtx LT.RegisterError
limitHit pid uid appCtx = do
  perUser <- LT.countActiveForUser pid uid
  if perUser >= LT.maxPerUser
    then pure (LT.TooManySubscriptionsForUser perUser)
    else LT.TooManySubscriptionsForProject <$> LT.countActiveForProject pid


jsonError :: Servant.ServerError -> Text -> Servant.ServerError
jsonError e msg =
  e
    { Servant.errBody = AE.encode (AE.object ["error" AE..= msg])
    , Servant.errHeaders = [("Content-Type", "application/json")]
    }


-- | Push the lease out.
--
-- The /browser/ renews, not the stream handler. That looks backwards until you ask what a
-- lease is for: it exists so a subscription dies when nobody is watching it, and the only
-- component that actually knows whether someone is watching is the tab itself. A server-side
-- renewal loop would happily keep a lease alive against a browser that crashed, an SSE
-- connection wedged open by a proxy, or a laptop that went to sleep mid-incident — the exact
-- cases the lease exists to clean up. It also keeps DB access inside the effect stack instead
-- of threading a pool into the raw-'IO' streaming body.
--
-- 404 rather than a silent re-create when the lease has already lapsed: the browser must open
-- a new subscription and know it has a gap, not carry on believing it never missed anything.
liveTailRenewH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders AE.Value)
liveTailRenewH pid rawSid = do
  sess <- Projects.getSession
  appCtx <- Reader.ask @AuthContext
  sid <- maybe (Error.throwError (jsonError Servant.err404 "No such live tail.")) pure (idFromText rawSid)
  -- Scoped to the caller before renewing, so a leaked id cannot be used to keep someone
  -- else's tail alive.
  _ <-
    maybe (Error.throwError (jsonError Servant.err404 "This live tail has expired. Start a new one.")) pure
      =<< LT.activeSubscriptionFor pid sess.user.id sid
  now <- Time.currentTime
  let expiresAt = addUTCTime (fromIntegral LT.leaseSecs) now
  renewed <- LT.renewLease sid expiresAt
  maybe
    (Error.throwError (jsonError Servant.err404 "This live tail has expired. Start a new one."))
    (\e -> addRespHeaders (AE.object ["expires_at" AE..= e]))
    renewed


-- | Drop a lease. Idempotent, so the browser's @beforeunload@ and the expiry reaper cannot
-- race into a spurious error.
liveTailDeleteH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders AE.Value)
liveTailDeleteH pid rawSid = do
  sess <- Projects.getSession
  forM_ (idFromText rawSid) (LT.deleteSubscription pid sess.user.id)
  addRespHeaders (AE.object ["ok" AE..= True])


-- ---------------------------------------------------------------------------------------
-- The stream
-- ---------------------------------------------------------------------------------------

-- | Open the SSE stream for a subscription.
--
-- Authorisation happens here, in the effect stack, before a single byte is written:
-- 'LT.activeSubscriptionFor' scopes the lookup to the caller's project /and/ user, so a
-- subscription id belonging to someone else simply is not found. The streaming body that
-- follows runs in plain 'IO', outside the effect stack, so it touches nothing but the queue
-- captured here — the lease is renewed over a separate route by the browser, precisely so this
-- body never needs a database.
liveTailStreamH :: Projects.ProjectId -> Text -> ATAuthCtx (SseHeaders (Servant.SourceIO Builder))
liveTailStreamH pid rawSid = do
  sess <- Projects.getSession
  appCtx <- Reader.ask @AuthContext
  sid <- maybe (Error.throwError (jsonError Servant.err404 "No such live tail.")) pure (idFromText rawSid)
  sub <-
    maybe (Error.throwError (jsonError Servant.err404 "This live tail has expired. Start a new one.")) pure
      =<< LT.activeSubscriptionFor pid sess.user.id sid
  conn <- LT.newConn LT.queueCapacity
  detach <- LT.attachConn appCtx.liveTail.hub sid conn
  -- Teardown drops the lease as well as the local queue. The server learns a browser is gone
  -- the moment the response body fails, which beats waiting out the lease: without this, a
  -- crashed or slept tab leaves ingest matching and publishing rows nobody will read for the
  -- full lease period. Safe against reconnects — the client registers a new subscription when
  -- it reconnects, so this id is always one nothing returns to.
  let release = detach >> LT.deleteSubscriptionIO appCtx.hasqlPool pid sess.user.id sid
  pure $ addHeader "no-cache, no-transform" $ addHeader "no" $ streamFrom sub conn release


-- | The response body: @ready@, then a @log@ frame per batch, forever.
--
-- 'Effect' wraps each step in 'IO' so the generator can block on the queue; the @detach@ in
-- the terminal step is what stops an abandoned connection leaking its queue for the lifetime
-- of the pod, and it runs whether the client disconnected or the stream ended on its own.
streamFrom :: LT.Subscription -> LT.Conn -> IO () -> Servant.SourceIO Builder
streamFrom sub conn detach = SourceT \k -> do
  -- The drop count on the connection is cumulative — that is what the browser needs, since it
  -- renders a total. A counter needs the opposite, so the last reported value is held here and
  -- only the delta is added; feeding the running total to 'Metrics.count' would count every
  -- drop again on every batch.
  reported <- newIORef (0 :: Int)
  k (start reported) `finally` detach
  where
    start reported =
      Yield
        (sseFrame "ready" (AE.object ["subscription_id" AE..= sub.id.toText, "expires_at" AE..= sub.expiresAt]))
        (loop reported)

    -- Waking on a timeout rather than blocking outright is what lets an idle tail emit a
    -- heartbeat. A tail on a quiet service is the normal case, and a proxy that has seen no
    -- bytes will close the connection — without this, "idle" and "dead" look the same to
    -- every hop between here and the browser.
    loop reported = Effect do
      batchM <- LT.takeBatchWithin heartbeatMicros conn
      whenJust batchM \(_, dropped) ->
        atomicModifyIORef' reported (dropped,) >>= \was -> Metrics.count Metrics.liveTailBrowserDropped (dropped - was) []
      pure case batchM of
        Nothing -> Yield (sseComment "keep-alive") (loop reported)
        Just (batch, dropped) -> do
          -- Notices ride the row queue because that is the only path addressed to this
          -- connection, but they are not rows and must not reach the table renderer.
          let (notices, rows) = partitionEithers (map asNotice batch)
          -- @notice@, not @error@ as the plan named it. EventSource dispatches on the frame's
          -- event name, and @error@ is already the name of its own transport-failure event —
          -- a frame called @error@ arrives at the client's @onerror@ handler, which retries.
          -- The one event that means "retrying will not help" would have been the one event
          -- guaranteed to trigger a retry.
          foldr (\m -> Yield (sseFrame "notice" (AE.object ["message" AE..= m]))) (rowsThen reported rows dropped) notices

    -- A batch of only notices must not yield an empty @log@ frame: the client appends what it
    -- receives, and an empty append is a re-render for nothing.
    rowsThen reported rows dropped
      | null rows = continue
      | otherwise = Yield (sseFrame "log" (AE.toJSON rows)) continue
      where
        -- Cumulative, so re-sending it alongside every batch is harmless: the browser renders
        -- a total ("4,812 dropped — narrow your filter"), not a delta.
        continue = if dropped > 0 then Yield (sseFrame "dropped" (AE.object ["count" AE..= dropped])) (loop reported) else loop reported

    asNotice = \case
      LT.Notice msg -> Left msg
      row -> Right row

    heartbeatMicros = 10_000_000


-- ---------------------------------------------------------------------------------------
-- The page
-- ---------------------------------------------------------------------------------------

newtype LiveTailGet = LiveTailPage (PageCtx LiveTailPageData)


data LiveTailPageData = LiveTailPageData
  { pid :: Projects.ProjectId
  , leaseSecs :: Int
  }


instance ToHtml LiveTailGet where
  toHtml (LiveTailPage (PageCtx conf pd)) = toHtml $ PageCtx conf $ liveTailPage_ pd
  toHtmlRaw = toHtml


liveTailGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders LiveTailGet)
liveTailGetH pid = do
  (_, _, bw) <- mkPageCtx pid
  appCtx <- Reader.ask @AuthContext
  let bwconf =
        bw
          { prePageTitle = Just "Explorer"
          , pageTitle = "Live Tail"
          , menuItem = Just "Explorer"
          , navTabs = Just $ explorerNavTabs_ pid "Live Tail"
          }
  addRespHeaders
    $ LiveTailPage
    $ PageCtx
      bwconf
      LiveTailPageData
        { pid
        , leaseSecs = LT.leaseSecs
        }


liveTailPage_ :: LiveTailPageData -> Html ()
liveTailPage_ pd =
  -- All interaction lives in the web component: it owns the EventSource, the row buffer and
  -- the reconnect state machine, none of which the server can hold on the client's behalf.
  -- There is no unavailable state to render — every deployment has a working transport.
  term
    "live-tail"
    [ class_ "w-full h-full"
    , term "data-project-id" pd.pid.toText
    , term "data-lease-secs" (show pd.leaseSecs)
    ]
    mempty
