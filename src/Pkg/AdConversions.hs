module Pkg.AdConversions (
  attributionMiddleware,
  attributionFromCookies,
  recordSignup,
  sendActivations,
) where

import Control.Lens ((.~))
import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Effectful (Eff, IOE, (:>))
import Effectful.Log (Log)
import Effectful.Time (Time, currentTime)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (parseSimpleQuery, renderSimpleQuery, urlDecode, urlEncode)
import Network.Wai (Middleware, mapResponseHeaders, queryString)
import Pkg.DeriveUtils (DB)
import Relude
import System.Config (EnvConfig (..))
import System.Logging qualified as Logging
import UnliftIO.Exception (tryAny)
import Web.Cookie (SetCookie (..), defaultSetCookie, renderSetCookieBS, sameSiteLax)
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)


data Conversion = Signup | Activated
  deriving stock (Eq, Show)


attributionKeys :: [ByteString]
attributionKeys = ["gclid", "gbraid", "wbraid", "fbclid", "rdt_cid", "li_fat_id", "twclid", "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content"]


attributionCookie :: ByteString
attributionCookie = "monoscope_attribution"


-- | The latest ad click wins: any request carrying a click ID or UTM rewrites a 90-day cookie, so
-- the params survive the Auth0 round trip and a detour through the public demo before signup.
attributionMiddleware :: Middleware
attributionMiddleware app req respond = case [(k, BS.take 512 v) | (k, Just v) <- queryString req, k `elem` attributionKeys, not (BS.null v)] of
  [] -> app req respond
  params -> do
    now <- getPOSIXTime
    let cookie = defaultSetCookie{setCookieName = attributionCookie, setCookieValue = urlEncode True $ renderSimpleQuery False (("clicked_at", show @ByteString @Int $ floor now) : params), setCookiePath = Just "/", setCookieMaxAge = Just (90 * 86400), setCookieHttpOnly = True, setCookieSecure = True, setCookieSameSite = Just sameSiteLax}
    app req $ respond . mapResponseHeaders (("Set-Cookie", renderSetCookieBS cookie) :)


-- | What is stored on a new user: the stashed click, Meta's browser IDs, and the user agent the
-- Conversions API needs for website events. @fbc@ is rebuilt from @fbclid@ when the pixel set none.
--
-- >>> attributionFromCookies [("monoscope_attribution", "clicked_at%3D1700000000%26gclid%3DG1%26fbclid%3DF1%26evil%3Dx"), ("_fbp", "fb.1.1.2")] (Just "UA")
-- fromList [("clicked_at","1700000000"),("fbc","fb.1.1700000000000.F1"),("fbclid","F1"),("fbp","fb.1.1.2"),("gclid","G1"),("user_agent","UA")]
-- >>> attributionFromCookies [("_fbc", "fb.1.5.X"), ("monoscope_attribution", "clicked_at%3D1%26fbclid%3DF1")] Nothing
-- fromList [("clicked_at","1"),("fbc","fb.1.5.X"),("fbclid","F1")]
-- >>> attributionFromCookies [] Nothing
-- fromList []
attributionFromCookies :: [(ByteString, ByteString)] -> Maybe Text -> Map Text Text
attributionFromCookies cookies userAgent = attrs <> M.fromList [("fbc", "fb.1." <> ts <> "000." <> fbclid) | Just ts <- [M.lookup "clicked_at" attrs], Just fbclid <- [M.lookup "fbclid" attrs]]
  where
    stashed = [(decodeUtf8 k, decodeUtf8 v) | (k, v) <- foldMap (parseSimpleQuery . urlDecode True) (L.lookup attributionCookie cookies), k `elem` ("clicked_at" : attributionKeys)]
    attrs = M.fromList $ stashed <> [(k, decodeUtf8 v) | (k, c) <- [("fbc", "_fbc"), ("fbp", "_fbp")], Just v <- [L.lookup c cookies]] <> [("user_agent", ua) | Just ua <- [userAgent]]


-- | Store a new user's attribution and send the signup to Meta and Reddit. The returned cookie
-- lets the next rendered page fire the browser-side conversions once, under the same event id.
recordSignup :: (DB es, HTTP :> es, IOE :> es, Log :> es, Time :> es) => EnvConfig -> Projects.UserId -> Text -> Map Text Text -> Eff es SetCookie
recordSignup cfg uid email attrs = do
  unless (null attrs) $ Hasql.interpExecute_ [HI.sql| UPDATE users.users SET attribution = #{HI.AsJsonb attrs} WHERE id = #{uid} |]
  sendMeta cfg Signup uid.toText uid email attrs
  sendReddit cfg uid email attrs
  pure defaultSetCookie{setCookieName = "monoscope_signup", setCookieValue = encodeUtf8 uid.toText, setCookiePath = Just "/", setCookieMaxAge = Just 86400, setCookieSecure = True, setCookieSameSite = Just sameSiteLax}


-- | Send the activation of each project to Meta, matched to the member who created it.
sendActivations :: (DB es, HTTP :> es, IOE :> es, Log :> es, Time :> es) => EnvConfig -> [Projects.ProjectId] -> Eff es ()
sendActivations cfg = traverse_ \pid ->
  Hasql.interpOne [HI.sql| SELECT u.id, u.email::text, u.attribution FROM projects.project_members pm JOIN users.users u ON u.id = pm.user_id WHERE pm.project_id = #{pid} ORDER BY pm.created_at, pm.id LIMIT 1 |]
    >>= traverse_ \(uid, email, attrs :: Maybe (HI.AsJsonb (Map Text Text))) -> sendMeta cfg Activated pid.toText uid email (maybe mempty coerce attrs)


-- | @signup-<user id>@ / @activated-<project id>@: the id every channel (pixel, CAPI, gtag
-- transaction, offline-upload order) shares for one conversion, so each platform counts it once.
eventId :: Conversion -> Text -> Text
eventId = \case
  Signup -> ("signup-" <>)
  Activated -> ("activated-" <>)


hashedEmail :: Text -> Text
hashedEmail email = show (hash (encodeUtf8 @Text @ByteString $ T.toLower $ T.strip email) :: Digest SHA256)


-- | Meta Conversions API event. No-op without a CAPI token and dataset (pixel) id.
sendMeta :: (HTTP :> es, IOE :> es, Log :> es, Time :> es) => EnvConfig -> Conversion -> Text -> Projects.UserId -> Text -> Map Text Text -> Eff es ()
sendMeta cfg conv subject uid email attrs = whenJust ((,) <$> cfg.metaCapiAccessToken <*> cfg.metaCapiPixelId) \(token, pixelId) -> do
  now <- currentTime
  let userData = AE.object $ ["em" .= one @[Text] (hashedEmail email), "external_id" .= one @[Text] (hashedEmail uid.toText)] <> [k .= v | (k, a) <- [("fbc", "fbc"), ("fbp", "fbp"), ("client_user_agent", "user_agent")], Just v <- [M.lookup a attrs]]
      event = AE.object ["event_name" .= bool ("Activated" :: Text) "CompleteRegistration" (conv == Signup), "event_time" .= (floor (utcTimeToPOSIXSeconds now) :: Int), "event_id" .= eventId conv subject, "action_source" .= ("website" :: Text), "user_data" .= userData]
  -- The token rides in the body, not the URL, so a logged HttpException cannot carry it.
  postConversion "Meta" W.defaults ("https://graph.facebook.com/v21.0/" <> pixelId <> "/events")
    $ AE.object
    $ ["data" .= one @[AE.Value] event, "access_token" .= token]
    <> ["test_event_code" .= c | Just c <- [cfg.metaCapiTestEventCode]]


-- | Reddit Conversions API SignUp, matched on the stored @rdt_cid@ click and hashed email. No-op
-- without a token and ad account.
sendReddit :: (HTTP :> es, IOE :> es, Log :> es, Time :> es) => EnvConfig -> Projects.UserId -> Text -> Map Text Text -> Eff es ()
sendReddit cfg uid email attrs = whenJust ((,) <$> cfg.redditConversionsToken <*> cfg.redditAdAccountId) \(token, account) -> do
  now <- currentTime
  let user = AE.object $ ("email" .= hashedEmail email) : ["user_agent" .= ua | Just ua <- [M.lookup "user_agent" attrs]]
      event = AE.object $ ["event_at" .= now, "event_type" .= AE.object ["tracking_type" .= ("SignUp" :: Text)], "event_metadata" .= AE.object ["conversion_id" .= eventId Signup uid.toText], "user" .= user] <> ["click_id" .= c | Just c <- [M.lookup "rdt_cid" attrs]]
  postConversion "Reddit" (W.defaults & W.header "Authorization" .~ ["Bearer " <> encodeUtf8 token]) ("https://ads-api.reddit.com/api/v2.0/conversions/events/" <> account)
    $ AE.object ["test_mode" .= False, "events" .= one @[AE.Value] event]


-- | A failed conversion is logged, never raised: an ad platform outage must not fail a login.
postConversion :: (HTTP :> es, IOE :> es, Log :> es) => Text -> W.Options -> Text -> AE.Value -> Eff es ()
postConversion platform opts url body =
  whenLeftM_ (tryAny $ W.postWith opts (toString url) body) \e ->
    Logging.logAttention (platform <> " conversion request failed") (AE.object ["error" .= show @Text e])
