module Pkg.Components.ExternalHeadScripts (externalHeadScripts_) where

import Lucid
import PyF
import Relude


externalHeadScripts_ :: Html ()
externalHeadScripts_ = do
  script_ [async_ "true", src_ "https://www.googletagmanager.com/gtag/js?id=AW-11285541899"] ("" :: Text)
  script_
    [raw|
          window.dataLayer = window.dataLayer || [];
          function gtag(){dataLayer.push(arguments);}
          gtag('js', new Date());
          gtag('config', 'AW-11285541899');

          function gtag_report_conversion(url) {
            var callback = function () {
              if (typeof(url) != 'undefined') {
                window.location = url;
              }
            };
            gtag('event', 'conversion', {
                'send_to': 'AW-11285541899/IUBqCKOA-8sYEIvoroUq',
                'event_callback': callback
            });
            return false;
          } |]

  -- Facebook Pixel Code --
  script_
    [raw|
        setTimeout(function(){
    !function(f,b,e,v,n,t,s)
  {if(f.fbq)return;n=f.fbq=function(){n.callMethod?
  n.callMethod.apply(n,arguments):n.queue.push(arguments)};
  if(!f._fbq)f._fbq=n;n.push=n;n.loaded=!0;n.version='2.0';
  n.queue=[];t=b.createElement(e);t.async=!0;
  t.src=v;s=b.getElementsByTagName(e)[0];
  s.parentNode.insertBefore(t,s)}(window,document,'script',
  'https://connect.facebook.net/en_US/fbevents.js');
  fbq('init', '1135987380886994'); 
  fbq('track', 'PageView')

  !function(f,b,e,v,n,t,s){if(f.fbq)return;n=f.fbq=function(){n.callMethod?
  n.callMethod.apply(n,arguments):n.queue.push(arguments)};if(!f._fbq)f._fbq=n;
  n.push=n;n.loaded=!0;n.version='2.0';n.queue=[];t=b.createElement(e);t.async=!0;
  t.src=v;s=b.getElementsByTagName(e)[0];s.parentNode.insertBefore(t,s)}(window,
  document,'script','https://connect.facebook.net/en_US/fbevents.js');
  fbq('init', '3674513372787915');
  fbq('track', 'PageView')
                   },3000);
    |]
  noscript_ $ img_ [height_ "1", width_ "1", src_ "https://www.facebook.com/tr?id=3674513372787915&ev=PageView&noscript=1"]
  -- End Facebook Pixel Code

  -- Google pixel
  script_
    [raw|
  (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
  new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
  j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
  'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
  })(window,document,'script','dataLayer','GTM-TF4BQQ3D');
    |]
  noscript_ $ iframe_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ "https://www.googletagmanager.com/ns.html?id=GTM-TF4BQQ3D"] ""
  -- End GOOGLE NO SCRIPT

  -- Linkedin pixel
  script_
    [raw|
  _linkedin_partner_id = "5779626"; window._linkedin_data_partner_ids = window._linkedin_data_partner_ids || [];
  window._linkedin_data_partner_ids.push(_linkedin_partner_id);      
    |]
  script_
    [raw|
  setTimeout(function(){
  (function(l) { if (!l){window.lintrk = function(a,b){window.lintrk.q.push([a,b])}; window.lintrk.q=[]} var s = document.getElementsByTagName("script")[0]; var b = document.createElement("script"); b.type = "text/javascript";b.async = true; b.src = "https://snap.licdn.com/li.lms-analytics/insight.min.js"; s.parentNode.insertBefore(b, s);})(window.lintrk);
                },3000);
    |]

  noscript_ $ img_ [height_ "0", width_ "0", style_ "display:none;visibility:hidden", src_ "https://px.ads.linkedin.com/collect/?pid=5779626&fmt=gif"]
  -- End Linkedin NO SCRIPT

  script_
    [raw|
(function() {
    !function(t,e){var o,n,p,r;e.__SV||(window.posthog=e,e._i=[],e.init=function(i,s,a){function g(t,e){var o=e.split(".");2==o.length&&(t=t[o[0]],e=o[1]),t[e]=function(){t.push([e].concat(Array.prototype.slice.call(arguments,0)))}}(p=t.createElement("script")).type="text/javascript",p.async=!0,p.src=s.api_host+"/static/array.js",(r=t.getElementsByTagName("script")[0]).parentNode.insertBefore(p,r);var u=e;for(void 0!==a?u=e[a]=[]:a="posthog",u.people=u.people||[],u.toString=function(t){var e="posthog";return"posthog"!==a&&(e+="."+a),t||(e+=" (stub)"),e},u.people.toString=function(){return u.toString(1)+".people (stub)"},o="capture identify alias people.set people.set_once set_config register register_once unregister opt_out_capturing has_opted_out_capturing opt_in_capturing reset isFeatureEnabled onFeatureFlags getFeatureFlag getFeatureFlagPayload reloadFeatureFlags group updateEarlyAccessFeatureEnrollment getEarlyAccessFeatures getActiveMatchingSurveys getSurveys onSessionId".split(" "),n=0;n<o.length;n++)g(u,o[n]);e._i.push([i,s,a])},e.__SV=1)}(document,window.posthog||[]);
    posthog.init('phc_XrCcXiHIfUzWzwXz0dMWSw19iGZsfk5VQ0VyJoiAIuA',{api_host:'https://app.posthog.com'})
})();
        |]
  script_
    [raw|window.$crisp = []; window.CRISP_WEBSITE_ID = "9e7374e9-5572-4131-b1fa-86164ffeca01"; (function () { d = document; s = d.createElement("script"); s.src = "https://client.crisp.chat/l.js"; s.async = 1; d.getElementsByTagName("head")[0].appendChild(s); })();|]
