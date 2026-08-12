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
import Data.Text qualified as T
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
import Relude
import Servant qualified
import Servant.API (Accept (..), MimeRender (..))
import Servant.Types.SourceT (SourceT (..), StepT (..))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (explorerNavTabs_, faSprite_)


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
--
-- Ordering is deliberate: the service gate is checked before the query is parsed and before
-- any count query runs, because it is the only reason Live Tail's volume is bounded and it is
-- also the cheapest thing to reject.
liveTailRegisterH :: Projects.ProjectId -> LT.NewSubscription -> ATAuthCtx (RespHeaders RegisterResponse)
liveTailRegisterH pid body = do
  sess <- Projects.getSession
  appCtx <- Reader.ask @AuthContext
  case appCtx.liveTail.transport of
    LT.Unavailable why -> Error.throwError (jsonError Servant.err503 why)
    _ -> pass
  when (T.null (T.strip body.service)) $ refuse LT.ServiceRequired
  whenLeft_ (LT.compileQuery (fromMaybe "" body.query)) refuse
  perUser <- LT.countActiveForUser pid sess.user.id
  when (perUser >= appCtx.config.liveTailMaxPerUser) $ refuse (LT.TooManySubscriptionsForUser perUser)
  perProject <- LT.countActiveForProject pid
  when (perProject >= appCtx.config.liveTailMaxPerProject) $ refuse (LT.TooManySubscriptionsForProject perProject)
  now <- Time.currentTime
  let expiresAt = addUTCTime (fromIntegral appCtx.config.liveTailLeaseSecs) now
  sid <- LT.insertSubscription pid sess.user.id body expiresAt
  addRespHeaders
    RegisterResponse
      { subscriptionId = sid.toText
      , streamUrl = "/p/" <> pid.toText <> "/live_tail/subscriptions/" <> sid.toText <> "/stream"
      , expiresAt
      }
  where
    refuse e = Error.throwError (jsonError (status e) (LT.registerErrorMessage e))
    status = \case
      LT.TooManySubscriptionsForUser _ -> Servant.err409
      LT.TooManySubscriptionsForProject _ -> Servant.err409
      _ -> Servant.err400


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
  let expiresAt = addUTCTime (fromIntegral appCtx.config.liveTailLeaseSecs) now
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
liveTailStreamH :: Projects.ProjectId -> Text -> ATAuthCtx (Servant.SourceIO Builder)
liveTailStreamH pid rawSid = do
  sess <- Projects.getSession
  appCtx <- Reader.ask @AuthContext
  sid <- maybe (Error.throwError (jsonError Servant.err404 "No such live tail.")) pure (idFromText rawSid)
  sub <-
    maybe (Error.throwError (jsonError Servant.err404 "This live tail has expired. Start a new one.")) pure
      =<< LT.activeSubscriptionFor pid sess.user.id sid
  conn <- LT.newConn (fromIntegral (max 1 appCtx.config.liveTailQueueCapacity))
  detach <- LT.attachConn appCtx.liveTail.hub sid conn
  pure (streamFrom sub conn detach)


-- | The response body: @ready@, then a @log@ frame per batch, forever.
--
-- 'Effect' wraps each step in 'IO' so the generator can block on the queue; the @detach@ in
-- the terminal step is what stops an abandoned connection leaking its queue for the lifetime
-- of the pod, and it runs whether the client disconnected or the stream ended on its own.
streamFrom :: LT.Subscription -> LT.Conn -> IO () -> Servant.SourceIO Builder
streamFrom sub conn detach = SourceT \k -> k start `finally` detach
  where
    start =
      Yield
        (sseFrame "ready" (AE.object ["subscription_id" AE..= sub.id.toText, "expires_at" AE..= sub.expiresAt]))
        loop

    -- Waking on a timeout rather than blocking outright is what lets an idle tail emit a
    -- heartbeat. A tail on a quiet service is the normal case, and a proxy that has seen no
    -- bytes will close the connection — without this, "idle" and "dead" look the same to
    -- every hop between here and the browser.
    loop = Effect do
      LT.takeBatchWithin heartbeatMicros conn <&> \case
        Nothing -> Yield (sseComment "keep-alive") loop
        Just (rows, dropped) ->
          Yield (sseFrame "log" (AE.toJSON rows))
            -- Cumulative, so re-sending it alongside every batch is harmless: the browser
            -- renders a total ("4,812 dropped — narrow your filter"), not a delta.
            $ if dropped > 0 then Yield (sseFrame "dropped" (AE.object ["count" AE..= dropped])) loop else loop

    heartbeatMicros = 10_000_000


-- ---------------------------------------------------------------------------------------
-- The page
-- ---------------------------------------------------------------------------------------

newtype LiveTailGet = LiveTailPage (PageCtx LiveTailPageData)


data LiveTailPageData = LiveTailPageData
  { pid :: Projects.ProjectId
  , unavailable :: Maybe Text
  -- ^ Why the tail cannot run, when it cannot. Shown instead of the controls, because a
  -- start button that can only ever fail is worse than an explanation.
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
        , unavailable = case appCtx.liveTail.transport of LT.Unavailable why -> Just why; _ -> Nothing
        , leaseSecs = appCtx.config.liveTailLeaseSecs
        }


liveTailPage_ :: LiveTailPageData -> Html ()
liveTailPage_ pd = case pd.unavailable of
  Just why -> div_ [class_ "w-full h-full flex items-center justify-center p-8"] do
    div_ [class_ "max-w-md flex flex-col items-center gap-3 text-center"] do
      faSprite_ "signal-stream" "regular" "w-8 h-8 text-iconNeutral"
      h2_ [class_ "text-base font-medium text-textStrong"] "Live Tail is unavailable"
      p_ [class_ "text-sm text-textWeak"] (toHtml why)
  Nothing ->
    -- All interaction lives in the web component: it owns the EventSource, the row buffer and
    -- the reconnect state machine, none of which the server can hold on the client's behalf.
    term
      "live-tail"
      [ class_ "w-full h-full"
      , term "data-project-id" pd.pid.toText
      , term "data-lease-secs" (show pd.leaseSecs)
      ]
      mempty
