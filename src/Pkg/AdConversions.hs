module Pkg.AdConversions (
  attributionMiddleware,
  recordSignup,
  sendConversion,
) where

import BackgroundJobs.Types (BgJobs (AdConversion), Conversion (..), insertJob)
import Control.Lens ((.~))
import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.ByteString.Char8 qualified as BS8
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
import Network.HTTP.Types (RequestHeaders, hCookie, hUserAgent, parseQuery, renderSimpleQuery, urlDecode, urlEncode)
import Network.Wai (Middleware, mapResponseHeaders, queryString, requestHeaders)
import Pkg.DeriveUtils (DB)
import Relude
import Relude.Extra.Bifunctor (bimapF)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import UnliftIO.Exception (tryAny)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookieBS, sameSiteLax)
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)


attributionCookie :: ByteString
attributionCookie = "monoscope_attribution"


baseCookie :: SetCookie
baseCookie = defaultSetCookie{setCookiePath = Just "/", setCookieSecure = True, setCookieSameSite = Just sameSiteLax}


-- | The ad click IDs and UTMs among query params, as a landing URL and the stashed cookie both carry them.
clickParams :: [(ByteString, Maybe ByteString)] -> [(ByteString, ByteString)]
clickParams q = [(k, BS8.take 512 v) | (k, Just v) <- q, k `elem` keys, not (BS8.null v)]
  where
    keys = BS8.words "gclid gbraid wbraid fbclid rdt_cid li_fat_id twclid utm_source utm_medium utm_campaign utm_term utm_content"


-- | The landing site's banner choice (@ms_consent@, set on the shared parent domain) wins. Without
-- one, consent holds where the landing assumes it: outside the EEA, UK and Switzerland, by
-- Cloudflare's country header. No header or an unknown country counts as inside.
--
-- >>> map consented [[("Cookie", "a=b; ms_consent=granted"), ("CF-IPCountry", "DE")], [("Cookie", "ms_consent=denied"), ("CF-IPCountry", "US")], [("CF-IPCountry", "US")], [("CF-IPCountry", "GB")], [("CF-IPCountry", "XX")], []]
-- [True,False,True,False,False,False]
consented :: RequestHeaders -> Bool
consented hs = maybe (any (`notElem` gdpr) $ L.lookup "CF-IPCountry" hs) (== "granted") (L.lookup "ms_consent" $ foldMap parseCookies $ L.lookup hCookie hs)
  where
    gdpr = BS8.words "AT BE BG HR CY CZ DK EE FI FR DE GR HU IE IT LV LT LU MT NL PL PT RO SK SI ES SE IS LI NO GB CH XX T1"


-- | The latest ad click wins: any consenting request carrying a click ID or UTM rewrites a 90-day
-- cookie, so the params survive the Auth0 round trip and a detour through the public demo.
attributionMiddleware :: Middleware
attributionMiddleware app req respond
  | null params || not (consented $ requestHeaders req) = app req respond
  | otherwise = do
      now <- getPOSIXTime
      let cookie = baseCookie{setCookieName = attributionCookie, setCookieValue = urlEncode True $ renderSimpleQuery False (("clicked_at", show @ByteString @Int $ floor now) : params), setCookieMaxAge = Just (90 * 86400), setCookieHttpOnly = True}
      app req $ respond . mapResponseHeaders (("Set-Cookie", renderSetCookieBS cookie) :)
  where
    params = clickParams $ queryString req


-- | What a consenting new user's row stores: the stashed click, Meta's browser IDs, and the user
-- agent the Conversions API wants for website events. @fbc@ is rebuilt from @fbclid@ when the pixel set none.
--
-- >>> attributionFrom [("Cookie", "ms_consent=granted; monoscope_attribution=clicked_at%3D1700000000%26gclid%3DG1%26fbclid%3DF1%26evil%3Dx; _fbp=fb.1.1.2"), ("User-Agent", "UA")]
-- Just (fromList [("clicked_at","1700000000"),("fbc","fb.1.1700000000000.F1"),("fbclid","F1"),("fbp","fb.1.1.2"),("gclid","G1"),("user_agent","UA")])
-- >>> attributionFrom [("Cookie", "_fbc=fb.1.5.X; monoscope_attribution=clicked_at%3D1%26fbclid%3DF1"), ("CF-IPCountry", "US")]
-- Just (fromList [("clicked_at","1"),("fbc","fb.1.5.X"),("fbclid","F1")])
-- >>> attributionFrom [("Cookie", "monoscope_attribution=clicked_at%3D1%26gclid%3DG1"), ("CF-IPCountry", "FR")]
-- Nothing
attributionFrom :: RequestHeaders -> Maybe (Map Text Text)
attributionFrom hs = (attrs <> M.fromList [("fbc", "fb.1." <> ts <> "000." <> fbclid) | Just ts <- [M.lookup "clicked_at" attrs], Just fbclid <- [M.lookup "fbclid" attrs]]) <$ guard (consented hs)
  where
    cookies = foldMap parseCookies $ L.lookup hCookie hs
    stashed = foldMap (parseQuery . urlDecode True) $ L.lookup attributionCookie cookies
    attrs =
      M.fromList
        $ bimapF decodeUtf8 decodeUtf8
        $ [("clicked_at", t) | Just (Just t) <- [L.lookup "clicked_at" stashed]]
        <> clickParams stashed
        <> [(k, v) | (k, c) <- [("fbc", "_fbc"), ("fbp", "_fbp")], Just v <- [L.lookup c cookies]]
        <> [("user_agent", ua) | Just ua <- [L.lookup hUserAgent hs]]


-- | With consent, store a new user's attribution and queue the signup for Meta and Reddit; the
-- returned cookie lets the next page fire the browser-side conversions under the same event id.
-- A failure is logged, not raised: the account exists already, so a failed login would lose it for good.
recordSignup :: (DB es, IOE :> es, Log :> es) => Projects.UserId -> RequestHeaders -> Eff es (Maybe SetCookie)
recordSignup uid hs = forM (attributionFrom hs) \attrs -> do
  whenLeftM_ (tryAny $ Hasql.interpExecute_ [HI.sql| UPDATE users.users SET attribution = #{HI.AsJsonb attrs} WHERE id = #{uid} |] >> insertJob (AdConversion $ Signup uid)) \e ->
    Logging.logAttention "Signup ad attribution not recorded" (AE.object ["user_id" .= uid, "error" .= show @Text e])
  pure baseCookie{setCookieName = "monoscope_signup", setCookieValue = encodeUtf8 uid.toText, setCookieMaxAge = Just 86400}


-- | The 'AdConversion' job. It sends only for a user whose row carries attribution, which
-- 'recordSignup' writes only with consent; an activation is credited to the project's first member.
-- A failed request raises, so odd-jobs retries the job, and the platforms dedupe a resend by event id
-- (@signup-<user id>@ / @activated-<project id>@, shared with the pixel, the gtag transaction and the
-- Google offline-upload order).
sendConversion :: (DB es, HTTP :> es, Time :> es) => AuthContext -> Conversion -> Eff es ()
sendConversion ctx conv = whenJustM (Hasql.interpOne subject) \(uid :: Projects.UserId, email, HI.AsJsonb (attrs :: Map Text Text)) -> do
  now <- utcTimeToPOSIXSeconds <$> currentTime
  let attr = flip M.lookup attrs
      sha t = show @Text (hash (encodeUtf8 @Text @ByteString $ T.toLower $ T.strip t) :: Digest SHA256)
      post opts url = void . W.postWith opts (toString url) . AE.object
  -- The token rides in the body, not the URL, so a logged HttpException cannot carry it.
  whenJust ((,) <$> ctx.config.metaCapiAccessToken <*> ctx.config.metaCapiPixelId) \(token, pixelId) ->
    post W.defaults ("https://graph.facebook.com/v21.0/" <> pixelId <> "/events")
      $ [ "access_token" .= token
        , "data"
            .= one @[AE.Value]
              ( AE.object
                  [ "event_name" .= eventName
                  , "event_time" .= (floor now :: Int)
                  , "event_id" .= eventId
                  , "action_source" .= ("website" :: Text)
                  , "event_source_url" .= ctx.env.hostUrl
                  , "user_data" .= AE.object (["em" .= one @[Text] (sha email), "external_id" .= one @[Text] (sha uid.toText)] <> [k .= v | (k, a) <- [("fbc", "fbc"), ("fbp", "fbp"), ("client_user_agent", "user_agent")], Just v <- [attr a]])
                  ]
              )
        ]
      <> ["test_event_code" .= c | Just c <- [ctx.config.metaCapiTestEventCode]]
  case conv of
    Activated _ -> pass
    Signup _ -> whenJust ((,) <$> ctx.config.redditConversionsToken <*> ctx.config.redditPixelId) \(token, pixelId) ->
      post (W.defaults & W.header "Authorization" .~ ["Bearer " <> encodeUtf8 token]) ("https://ads-api.reddit.com/api/v3/pixels/" <> pixelId <> "/conversion_events")
        $ one
        $ "data"
        .= AE.object
          [ "events"
              .= one @[AE.Value]
                ( AE.object
                    $ [ "event_at" .= (floor (now * 1000) :: Int)
                      , "action_source" .= ("WEBSITE" :: Text)
                      , "type" .= AE.object ["tracking_type" .= ("SIGN_UP" :: Text)]
                      , "user" .= AE.object (("email" .= sha email) : ["user_agent" .= ua | Just ua <- [attr "user_agent"]])
                      , "metadata" .= AE.object ["conversion_id" .= eventId]
                      ]
                    <> ["click_id" .= c | Just c <- [attr "rdt_cid"]]
                )
          ]
  where
    (subject, eventName, eventId) = case conv of
      Signup uid -> ([HI.sql| SELECT id, email::text, attribution FROM users.users WHERE id = #{uid} AND attribution IS NOT NULL |], "CompleteRegistration" :: Text, "signup-" <> uid.toText)
      Activated pid -> ([HI.sql| SELECT id, email::text, attribution FROM users.users WHERE attribution IS NOT NULL AND id = (SELECT user_id FROM projects.project_members WHERE project_id = #{pid} ORDER BY created_at, id LIMIT 1) |], "Activated", "activated-" <> pid.toText)
