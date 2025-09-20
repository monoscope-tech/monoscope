module Pkg.Components.ExternalHeadScripts (externalHeadScripts_) where

import Lucid
import PyF
import Relude
import System.Config (EnvConfig (..))


externalHeadScripts_ :: EnvConfig -> Html ()
externalHeadScripts_ config = do
  -- Google Ads
  whenJust config.googleAdsConversionId $ \conversionId -> do
    script_ [async_ "true", src_ $ "https://www.googletagmanager.com/gtag/js?id=" <> conversionId] ("" :: Text)
    script_
      [fmt|
            window.dataLayer = window.dataLayer || [];
            function gtag(){{dataLayer.push(arguments);}}
            gtag('js', new Date());
            gtag('config', '{conversionId}');

            function gtag_report_conversion(url) {{
              var callback = function () {{
                if (typeof(url) != 'undefined') {{
                  window.location = url;
                }}
              }};
              gtag('event', 'conversion', {{
                  'send_to': '{conversionId}/IUBqCKOA-8sYEIvoroUq',
                  'event_callback': callback
              }});
              return false;
            }} |]

  -- Facebook Pixel Code
  when (isJust config.facebookPixelId1 || isJust config.facebookPixelId2) $ do
    let pixelInitScript = mconcat $ catMaybes
          [ config.facebookPixelId1 <&> \pixelId -> [fmt|fbq('init', '{pixelId}'); fbq('track', 'PageView');|]
          , config.facebookPixelId2 <&> \pixelId -> [fmt|fbq('init', '{pixelId}'); fbq('track', 'PageView');|]
          ]
    script_
      [fmt|
          setTimeout(function(){{
      !function(f,b,e,v,n,t,s)
    {{if(f.fbq)return;n=f.fbq=function(){{n.callMethod?
    n.callMethod.apply(n,arguments):n.queue.push(arguments)}};
    if(!f._fbq)f._fbq=n;n.push=n;n.loaded=!0;n.version='2.0';
    n.queue=[];t=b.createElement(e);t.async=!0;
    t.src=v;s=b.getElementsByTagName(e)[0];
    s.parentNode.insertBefore(t,s)}}(window,document,'script',
    'https://connect.facebook.net/en_US/fbevents.js');
      {pixelInitScript}
      }},3000);
      |]
    whenJust config.facebookPixelId2 $ \pixelId ->
      noscript_ $ img_ [height_ "1", width_ "1", src_ $ "https://www.facebook.com/tr?id=" <> pixelId <> "&ev=PageView&noscript=1"]
  -- End Facebook Pixel Code

  -- Google Tag Manager
  whenJust config.googleTagManagerId $ \gtmId -> do
    script_
      [fmt|
    (function(w,d,s,l,i){{w[l]=w[l]||[];w[l].push({{'gtm.start':
    new Date().getTime(),event:'gtm.js'}});var f=d.getElementsByTagName(s)[0],
    j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
    'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
    }})(window,document,'script','dataLayer','{gtmId}');
      |]
    noscript_ $ iframe_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ $ "https://www.googletagmanager.com/ns.html?id=" <> gtmId] ""
  -- End Google Tag Manager

  -- LinkedIn pixel
  whenJust config.linkedInPartnerId $ \partnerId -> do
    script_
      [fmt|
    _linkedin_partner_id = "{partnerId}"; window._linkedin_data_partner_ids = window._linkedin_data_partner_ids || [];
    window._linkedin_data_partner_ids.push(_linkedin_partner_id);      
      |]
    script_
      [raw|
    setTimeout(function(){
    (function(l) { if (!l){window.lintrk = function(a,b){window.lintrk.q.push([a,b])}; window.lintrk.q=[]} var s = document.getElementsByTagName("script")[0]; var b = document.createElement("script"); b.type = "text/javascript";b.async = true; b.src = "https://snap.licdn.com/li.lms-analytics/insight.min.js"; s.parentNode.insertBefore(b, s);})(window.lintrk);
                  },3000);
      |]
    noscript_ $ img_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ $ "https://px.ads.linkedin.com/collect/?pid=" <> partnerId <> "&fmt=gif"]
  -- End LinkedIn

  -- PostHog
  whenJust config.postHogApiKey $ \apiKey -> do
    let apiHost = fromMaybe "https://eu.i.posthog.com" config.postHogApiHost
    script_
      [fmt|
(function() {{
    !function(t,e){{var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){{function g(t,e){{var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){{t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}}}(p=t.createElement("script")).type="text/javascript",p.crossOrigin="anonymous",p.async=!0,p.src=s.api_host.replace(".i.posthog.com","-assets.i.posthog.com")+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){{var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e}},u.people.toString=function(){{return u.toString(1)+".people (stub)"}},o="init Ce Ls Ns Te As js capture Xe calculateEventProperties qs register register_once register_for_session unregister unregister_for_session Gs getFeatureFlag getFeatureFlagPayload isFeatureEnabled reloadFeatureFlags updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures on onFeatureFlags onSurveysLoaded onSessionId getSurveys getActiveMatchingSurveys renderSurvey canRenderSurvey canRenderSurveyAsync identify setPersonProperties group resetGroups setPersonPropertiesForFlags resetPersonPropertiesForFlags setGroupPropertiesForFlags resetGroupPropertiesForFlags reset get_distinct_id getGroups get_session_id get_session_replay_url alias set_config startSessionRecording stopSessionRecording sessionRecordingStarted captureException loadToolbar get_property getSessionProperty Hs Us createPersonProfile Ws Os Js opt_in_capturing opt_out_capturing has_opted_in_capturing has_opted_out_capturing get_explicit_consent_status is_capturing clear_opt_in_out_capturing zs debug L Bs getPageViewId captureTraceFeedback captureTraceMetric".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])}},e.__SV=1)}}(document,window.posthog||[]);
    posthog.init('{apiKey}', {{
        api_host: '{apiHost}',
        defaults: '2025-05-24',
        person_profiles: 'identified_only'
    }})
}})();
        |]
  -- Crisp chat
  whenJust config.crispWebsiteId $ \websiteId ->
    script_
      [fmt|window.$crisp = []; window.CRISP_WEBSITE_ID = "{websiteId}"; (function () {{ d = document; s = d.createElement("script"); s.src = "https://client.crisp.chat/l.js"; s.async = 1; d.getElementsByTagName("head")[0].appendChild(s); }})();|]
