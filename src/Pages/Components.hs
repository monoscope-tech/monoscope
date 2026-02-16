module Pages.Components (statBox, drawer_, statBox_, emptyState_, emptyStateFiltered_, resizer_, dateTime, paymentPlanPicker, navBar, modal_, modalCloseButton_, tableSkeleton_, chartSkeleton_, cardSkeleton_, statBoxSkeleton_, FieldSize (..), FieldCfg (..), formField_, formSelectField_, formCheckbox_, PanelCfg (..), panel_, tagInput_, formActionsModal_, connectionBadge_, confirmModal_, BadgeColor (..), iconBadge_, iconBadgeSq_, iconBadgeLg_, iconBadgeXs_, iconBadgeWith_, ModalCfg (..), modalWith_) where

import Data.Default (Default (..))
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Fmt (commaizeF, fmt, (+|))
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxSwap_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects (ProjectId)
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Relude
import Utils (LoadingSize (..), LoadingType (..), faSprite_, loadingIndicator_, onpointerdown_)


statBox :: Maybe ProjectId -> Text -> Text -> Int -> Maybe Int -> Html ()
statBox pid title helpInfo val bckupValM = wrapper do
  div_ do
    div_ [class_ "inline-block flex flex-row content-between"] do
      span_ [class_ "font-bold text-textStrong text-2xl tabular-nums stat-value", term "data-value" (show val)] $ toHtml @Text $ fmt (commaizeF val)
      maybe "" (\bVal -> small_ [class_ "tabular-nums"] $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
    span_ [class_ "text-textWeak"] $ toHtml title
  span_ [class_ "inline-block tooltip tap-target", Aria.label_ "More info", term "data-tippy-content" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 h-4"
  where
    tl = getTargetPage title
    pidT = maybe "" (.toText) pid
    wrapper =
      if T.null tl
        then div_ [class_ "col-span-1 p-5 surface-raised rounded-2xl col-span-1 flex flex-row content-between justify-between"]
        else a_ [href_ $ "/p/" <> pidT <> tl, class_ "col-span-1 p-5 surface-raised rounded-2xl flex flex-row content-between justify-between"]


statBox_ :: Maybe ProjectId -> Maybe (Text, Text, Text) -> Text -> Text -> Text -> Maybe Int -> Maybe Text -> Html ()
statBox_ pid iconM title helpInfo val bckupValM valClsM = do
  div_ [class_ "bg-fillWeaker rounded-3xl flex flex-col gap-3 p-5 border border-strokeWeak"] do
    whenJust iconM $ \(icon, kind, color) -> do
      div_ [class_ "flex items-center justify-center h-10 w-10 bg-fillWeaker rounded-xl"] do
        faSprite_ icon kind $ "w-4 h-4 " <> color
    div_ [class_ "flex flex-col gap-1"] do
      let fsiz = if isJust iconM then "text-2xl " else "text-4xl "
      span_ [class_ $ "font-bold tabular-nums stat-value " <> fsiz <> fromMaybe "text-textStrong" valClsM, term "data-value" val] $ toHtml val
      div_ [class_ "flex gap-2 items-center text-sm text-textWeak"] do
        p_ [] $ toHtml title
        span_ [class_ "tap-target", Aria.label_ "More info", term "data-tippy-content" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 mt-[-2px]"


-- | Empty state component with optional contextual icon
-- Usage: emptyState_ (Just "chart-line") "No data" "Description" (Just "/setup") "Get Started"
-- Pass Nothing for icon to use default, or Just "icon-name" for custom icon
emptyState_ :: Maybe Text -> Text -> Text -> Maybe Text -> Text -> Html ()
emptyState_ iconM title subTxt urlM btnText =
  section_ [class_ "w-max mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4 empty-state"] do
    div_ [] $ faSprite_ (fromMaybe "empty" iconM) "regular" "h-24 w-24 stroke-strokeBrand-strong fill-fillBrand-strong"
    div_ [class_ "flex flex-col gap-2"] do
      h2_ [class_ "text-xl text-textStrong font-bold"] $ toHtml title
      p_ [class_ "text-sm font-medium text-textWeak"] $ toHtml subTxt
      whenJust urlM \u ->
        unless (T.null btnText)
          $ let attrs =
                  [href_ u, class_ "btn text-sm w-max mx-auto btn-primary"]
                    ++ if "https://" `T.isPrefixOf` u then [target_ "_blank", rel_ "noopener noreferrer"] else []
             in a_ attrs $ toHtml btnText


-- | Filtered empty state - for when search/filters return no results
-- Shows a different visual treatment to indicate filters are active
emptyStateFiltered_ :: Text -> Text -> Html () -> Html ()
emptyStateFiltered_ title subTxt actionHtml =
  section_ [class_ "w-max mx-auto my-8 text-center p-5 sm:py-10 sm:px-16 flex flex-col gap-4 rounded-xl empty-state-filtered"] do
    div_ [] $ faSprite_ "filter-slash" "regular" "h-16 w-16 stroke-strokeBrand-strong fill-fillBrand-weak"
    div_ [class_ "flex flex-col gap-2"] do
      h2_ [class_ "text-lg text-textStrong font-semibold"] $ toHtml title
      p_ [class_ "text-sm text-textWeak max-w-sm"] $ toHtml subTxt
      actionHtml


getTargetPage :: Text -> Text
getTargetPage "Requests" = "/log_explorer"
getTargetPage "Anomalies" = "/anomalies"
getTargetPage "Endpoints" = "/endpoints"
getTargetPage _ = ""


drawer_ :: Text -> Maybe Text -> Maybe (Html ()) -> Html () -> Html ()
drawer_ drawerId urlM content trigger = div_ [class_ "drawer drawer-end inline-block w-auto"] do
  input_
    [ id_ drawerId
    , type_ "checkbox"
    , class_ "drawer-toggle"
    , Aria.label_ "Toggle drawer"
    , [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup end
          on change
            if my.checked then
              add .overflow-hidden to <body/>
              set my._focusTrapCleanup to window.createFocusTrap(my.closest('.drawer').querySelector('.drawer-side > div:last-child'))
              wait 100ms then
              set :closeBtn to my.closest('.drawer').querySelector('[aria-label="Close drawer"]')
              if :closeBtn then call :closeBtn.focus() end
            else
              remove .overflow-hidden from <body/>
              if my._focusTrapCleanup then call my._focusTrapCleanup() end
              js { const url = new URL(window.location); if (url.searchParams.has('expand')) { url.searchParams.delete('expand'); history.replaceState({}, '', url); } } end
            end
      |]
    ]
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block", Aria.label_ "Open drawer"] trigger
  div_ [class_ "drawer-side top-0 left-0 w-full h-full flex z-10000 overflow-y-scroll "] do
    label_ [Lucid.for_ drawerId, Aria.label_ "Close drawer", class_ "w-full drawer-overlay grow flex-1"] ""
    div_ [style_ "width: min(90vw, 1200px)", class_ "bg-bgRaised h-full overflow-y-scroll overflow-x-hidden w-full"] do
      div_
        [id_ $ drawerId <> "-content", class_ "py-4 px-8 h-full flex flex-col gap-8", hxSwap_ "innerHTML"]
        $ div_ (maybe [] (\url -> [hxGet_ url, hxTrigger_ "intersect once"]) urlM)
        $ fromMaybe (loadingIndicator_ LdMD LdDots) content


dateTime :: UTCTime -> Maybe UTCTime -> Html ()
dateTime t endTM = do
  span_ [class_ "flex items-center rounded-lg px-2 py-1.5 text-xs gap-2 border border-strokeWeak bg-fillWeaker text-textStrong"] do
    faSprite_ "calendar" "regular" "w-4 h-4 fill-none"
    toHtml $ formatTime defaultTimeLocale "%b. %d, %I:%M:%S %p" t
    whenJust endTM $ \endT -> do
      toHtml $ " - " <> formatTime defaultTimeLocale "%b. %d, %I:%M:%S %p" endT


paymentPlanPicker :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Html ()
paymentPlanPicker pid lemonUrl criticalUrl currentPlan freePricingEnabled basicAuthEnabled = do
  let gridCols
        | basicAuthEnabled = "grid-cols-2"
        | freePricingEnabled = "grid-cols-3"
        | otherwise = "grid-cols-2"
  div_ [class_ "flex flex-col gap-8 w-full"] do
    unless basicAuthEnabled $ div_ [class_ "flex flex-col gap-2 w-full"] do
      div_ [class_ "flex items-center justify-between w-full gap-4"] do
        p_ [class_ " text-textStrong"] "Total events"
        p_ [class_ " text-textWeak", id_ "num_requests"] "25 Million"
      input_ [type_ "range", min_ "20000000", max_ "500000000", step_ "10000000", value_ "20000000", class_ "range range-primary range-sm w-full", id_ "price_range"]
    div_ [class_ "flex flex-col gap-8 mt-6 w-full"] do
      div_ [class_ $ "grid gap-8 w-full " <> gridCols] do
        when basicAuthEnabled $ openSourcePricing pid (currentPlan == "Open Source")
        when basicAuthEnabled enterprisePricing
        when (freePricingEnabled && not basicAuthEnabled) $ freePricing pid (currentPlan == "Free")
        unless basicAuthEnabled $ popularPricing pid lemonUrl (currentPlan == "Bring nothing") freePricingEnabled
        unless basicAuthEnabled $ systemsPricing pid criticalUrl (currentPlan == "Bring your own storage")
    script_ [src_ "https://assets.lemonsqueezy.com/lemon.js"] ("" :: Text)
    script_
      [text|

             window.payLemon = function(plan, url) {
             LemonSqueezy.Setup({
               eventHandler: ({event, data}) => {
                 if(event === "Checkout.Success") {
                     let inputs = document.querySelectorAll(".orderId")
                     for (let input of inputs)  {
                      input.value = data.order.data.id
                     }
                     LemonSqueezy.Url.Close()
                     gtag('event', 'conversion', {
                         'send_to': 'AW-11285541899/rf7NCKzf_9YYEIvoroUq',
                         'value': 20.0,
                         'currency': 'EUR',
                         'transaction_id': '',
                     });
                     htmx.trigger("#"+ plan, "click")
                 }
               }
             })
              LemonSqueezy.Url.Open(url);
             };

               const price_indicator = document.querySelector("#price_range");
               const priceContainer = document.querySelector("#price")
               const criticalContainer = document.querySelector("#critical_price")
               const reqsContainer = document.querySelector("#num_requests")

               function priceChange() {
                 const value = price_indicator.value
                 let num_reqs = Math.floor(value/1000000)
                 let calculatedPrice = value <= 20_000_000 ? 29 : 29 + ((value- 20_000_000)/500_000)
                 let calculatedPriceCritical = value <= 100_000_000 ? 199 : 199 + ((value - 100_000_000)/500_000)
                 priceContainer.innerText = calculatedPrice
                 criticalContainer.innerText = calculatedPriceCritical
                 reqsContainer.innerText = num_reqs + " Million"
               }

               price_indicator.addEventListener('input', priceChange)

               function handlePaymentPlanSelect(event, id) {
                event.stopPropagation()
                const target = event.currentTarget
                if(id === 'popularPlan') {
                  window.paymentPlanUrl = "$lemonUrl"
                } else if(id=== 'systemsPlan') {
                  window.paymentPlanUrl = "$criticalUrl"
                }
               }
            |]


freePricing :: Projects.ProjectId -> Bool -> Html ()
freePricing pid isCurrent = do
  div_
    [ class_ "relative bg-bgRaised rounded-2xl py-11 px-4 outline outline-strokeWeak overflow-hidden"
    , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
    , id_ "freePricing"
    , hxSwap_ "none"
    , hxIndicator_ "#loadingIndicator"
    ]
    $ do
      div_
        [ class_ "flex flex-col gap-2 h-full relative"
        , onpointerdown_ "handlePaymentPlanSelect(event, 'freePlan')"
        , id_ "popularPlan"
        ]
        do
          div_ [class_ "w-full h-36 right-[-20px] top-[-45px] absolute bg-gradient-to-bl from-slate-500/10 to-slate-500/0"] pass
          div_ [class_ "flex-col justify-start items-start gap-1 flex"] $ do
            div_ [class_ "text-xl font-semibold text-textStrong"] "Free tier"
            div_ [class_ "text-textStrong text-sm"] "Free forever"
          div_ [class_ "flex items-center gap-1 mt-4"] do
            div_ [class_ "flex items-end"] do
              span_ [class_ "text-textStrong text-xl"] "$"
              span_ [class_ "text-4xl text-textStrong"] "0"
            div_ [class_ "flex flex-col text-textWeak text-sm"] do
              span_ [class_ ""] "/per month"
          div_ [[__|on click halt|]] do
            button_
              [ class_ $ "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg " <> if isCurrent then "bg-fillDisabled cursor-not-allowed border-0 text-textInverse-strong" else "bg-fillStrong text-white "
              , [__| on click htmx.trigger("#freePricing", "click")|]
              , type_ "button"
              ]
              do
                if isCurrent then "Current plan" else "Start free"
          included features "What's included:"
  where
    features =
      [ "10K events per day"
      , "1 team member"
      , "Opentelemetry Logs, Traces and Metrics"
      , "Last 3 days data retention"
      ]


popularPricing :: Projects.ProjectId -> Text -> Bool -> Bool -> Html ()
popularPricing pid lemonUrl isCurrent freeTierEnabled = do
  div_ [class_ "relative"] do
    div_
      [ class_ "relative bg-bgRaised rounded-2xl py-11 px-4 outline overflow-hidden  outline-strokeWeak shadow-[0px_3px_3px_-1.5px_rgba(10,13,18,0.04)] shadow-[0px_8px_8px_-4px_rgba(10,13,18,0.03)] shadow-[0px_20px_24px_-4px_rgba(10,13,18,0.08)]"
      , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
      , id_ "GraduatedPricing"
      , hxIndicator_ "#loadingIndicator"
      , hxSwap_ "none"
      , hxVals_ "js:{orderIdM: document.querySelector('#popularPricing').value}"
      ]
      $ do
        div_ [class_ "w-[500px] h-36 right-0 top-0 rotate-y-15 rotate-z-15 top-[-55px] right-[-40px] absolute bg-gradient-to-bl from-slate-500/10 to-slate-transparent"] pass
        div_
          [ class_ "relative flex flex-col gap-2 overflow-hidden"
          , onpointerdown_ "handlePaymentPlanSelect(event, 'popularPlan')"
          , id_ "popularPlan"
          ]
          do
            input_ [type_ "hidden", class_ "orderId", id_ "popularPricing", name_ "ord", value_ ""]
            div_ [class_ "flex-col justify-start items-start gap-1 flex"] $ do
              div_ [class_ "text-xl font-semibold text-textStrong"] "Bring nothing"
              div_ [class_ "text-textStrong text-sm"] "This plan can be adjusted"
            div_ [class_ "flex items-center gap-1 mt-4"] do
              div_ [class_ "flex items-end"] do
                span_ [class_ "text-textStrong text-xl"] "$"
                span_ [class_ "text-4xl text-textStrong", id_ "price"] "29"
              div_ [class_ "flex flex-col text-textWeak text-sm"] do
                span_ [class_ ""] "/per month"
            div_ [[__|on click halt|]] do
              button_
                [ class_ $ "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold " <> if isCurrent then "bg-fillDisabled cursor-not-allowed border-0 text-textInverse-strong" else "btn-primary"
                , term "_" [text|on click call window.payLemon("GraduatedPricing","$lemonUrl") |]
                , type_ "button"
                ]
                do
                  if isCurrent then "Current plan" else "Start 30 day free trial"
            included features $ span_ [] do
              when freeTierEnabled do
                "Everything in "
                span_ [class_ "text-textBrand"] "free"
                " plus..."
  where
    features =
      [ "Fully managed cloud service"
      , "Predictable usage-based pricing"
      , "Intelligent incident alerts"
      , "Query your data in english"
      , "30 days data retention included"
      ]


systemsPricing :: Projects.ProjectId -> Text -> Bool -> Html ()
systemsPricing pid critical isCurrent = do
  div_ [class_ "relative"] do
    div_
      [ class_ "relative bg-bgRaised rounded-2xl py-11 px-4 outline outline-strokeBrand-strong overflow-hidden"
      , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
      , id_ "SystemsPricing"
      , hxIndicator_ "#loadingIndicator"
      , hxSwap_ "none"
      , hxVals_ "js:{orderIdM: document.querySelector('#systemsPricing').value}"
      ]
      $ do
        div_
          [ class_ "flex flex-col gap-2"
          , onpointerdown_ "handlePaymentPlanSelect(event, 'systemsPlan')"
          , id_ "systemsPlan"
          ]
          do
            input_ [type_ "hidden", class_ "orderId", id_ "systemsPricing", name_ "ord", value_ ""]
            div_ [class_ "w-[500px] h-36 right-0 top-0 rotate-y-15 rotate-z-15 top-[-55px] right-[-40px] rounded-t-2xl absolute bg-gradient-to-b from-fillBrand-weak to-transparent"] pass
            div_ [class_ "flex-col justify-start items-start gap-1 flex"] $ do
              div_ [class_ "text-xl font-semibold text-textStrong"] "Bring your own storage"
              div_ [class_ "text-textStrong text-sm"] "Business plan"
            div_ [class_ "flex items-center gap-1 mt-4"] do
              div_ [class_ "flex items-end"] do
                span_ [class_ "text-textStrong text-xl"] "$"
                span_ [class_ "text-4xl text-textStrong", id_ "critical_price"] "199"
              div_ [class_ "flex flex-col text-textWeak text-sm"] do
                span_ [class_ ""] "/per month"
            div_ [[__|on click halt|]] do
              button_
                [ class_ $ "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg " <> if isCurrent then "bg-fillDisabled cursor-not-allowed border-0 text-textInverse-strong" else "bg-fillStrong text-textInverse-strong"
                , term "_" [text|on click call window.payLemon("SystemsPricing", "$critical") |]
                , type_ "button"
                ]
                do
                  if isCurrent then "Current plan" else "Start 30 day free trial"
            included features $ span_ [] do
              "Everything in "
              span_ [class_ "text-textBrand"] "bring nothing"
              " plus..."

    div_ [class_ "px-3 py-1.5 bg-fillBrand-strong absolute top-0 left-1/2 -translate-x-1/2 -translate-y-1/2 rounded-lg inline-flex justify-center items-center gap-2"] do
      div_ [class_ "justify-start text-white"] do
        span_ [class_ "text-sm font-medium leading-tight"] "🌟"
        span_ [class_ "leading-tight text-sm"] "MOST POPULAR"
  where
    features =
      [ "Own and control all your data"
      , "Save all your data to any S3-compatible bucket"
      , "Unlimited data retention period"
      , "Query years of data via monoscope"
      , "No extra cost for data retention"
      ]


openSourcePricing :: Projects.ProjectId -> Bool -> Html ()
openSourcePricing pid isCurrent = do
  div_
    [ class_ "relative bg-bgRaised rounded-2xl py-11 px-4 outline outline-strokeWeak overflow-hidden"
    , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
    , id_ "openSourcePricing"
    , hxSwap_ "none"
    , hxIndicator_ "#loadingIndicator"
    , hxVals_ "{\"plan\": \"Open Source\"}"
    ]
    $ do
      div_
        [ class_ "flex flex-col gap-2 h-full relative"
        ]
        do
          div_ [class_ "w-full h-36 right-[-20px] top-[-45px] absolute bg-gradient-to-bl from-green-500/10 to-green-500/0"] pass
          div_ [class_ "flex-col justify-start items-start gap-1 flex"] $ do
            div_ [class_ "text-xl font-semibold text-textStrong"] "Open Source Community Edition"
            div_ [class_ "text-textStrong text-sm"] "Self-hosted deployment"
          div_ [class_ "flex items-center gap-1 mt-4"] do
            div_ [class_ "flex items-end"] do
              span_ [class_ "text-textStrong text-xl"] "$"
              span_ [class_ "text-4xl text-textStrong"] "0"
            div_ [class_ "flex flex-col text-textWeak text-sm"] do
              span_ [class_ ""] "Free forever"
          div_ do
            button_
              ( [ class_ $ "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg " <> if isCurrent then "bg-fillDisabled cursor-not-allowed border-0 text-textInverse-strong" else "bg-green-700 hover:bg-green-600 text-white"
                , type_ "submit"
                ]
                  <> [disabled_ "disabled" | isCurrent]
              )
              do
                if isCurrent then "Current plan" else "Continue with Open Source"
          included features "What's included:"
  where
    features =
      [ "Unlimited events"
      , "Unlimited team members"
      , "Self-hosted deployment"
      , "Full control over your data"
      , "Community support"
      , "All APItoolkit features"
      ]


enterprisePricing :: Html ()
enterprisePricing = do
  div_ [class_ "relative"] do
    div_
      [ class_ "relative bg-bgRaised rounded-2xl py-11 px-4 outline outline-strokeBrand-strong overflow-hidden"
      ]
      $ do
        div_
          [ class_ "flex flex-col gap-2"
          ]
          do
            div_ [class_ "w-[500px] h-36 right-0 top-0 rotate-y-15 rotate-z-15 top-[-55px] right-[-40px] rounded-t-2xl absolute bg-gradient-to-b from-fillBrand-weak to-transparent"] pass
            div_ [class_ "flex-col justify-start items-start gap-1 flex"] $ do
              div_ [class_ "text-xl font-semibold text-textStrong"] "Enterprise"
              div_ [class_ "text-textStrong text-sm"] "Custom plan for your team"
            div_ [class_ "flex items-center gap-1 mt-4"] do
              div_ [class_ "flex items-end"] do
                span_ [class_ "text-4xl text-textStrong"] "Custom"
              div_ [class_ "flex flex-col text-textWeak text-sm"] do
                span_ [class_ ""] "pricing"
            div_ do
              a_
                [ class_ "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg btn-primary flex items-center justify-center"
                , href_ "https://monoscope.tech/pricing"
                , target_ "_blank"
                ]
                "Contact us"
            included features $ span_ [] do
              "Everything in "
              span_ [class_ "text-textBrand"] "open source"
              " plus..."

    div_ [class_ "px-3 py-1.5 bg-fillBrand-strong absolute top-0 left-1/2 -translate-x-1/2 -translate-y-1/2 rounded-lg inline-flex justify-center items-center gap-2"] do
      div_ [class_ "justify-start text-white"] do
        span_ [class_ "leading-tight text-sm"] "RECOMMENDED FOR TEAMS"
  where
    features =
      [ "Premium features & integrations"
      , "SSO & advanced auth"
      , "Priority support & SLA"
      , "Advanced security & compliance"
      ]


included :: [Text] -> Html () -> Html ()
included features title =
  div_ [class_ "flex-col justify-start items-start gap-3 flex"] $ do
    div_ [class_ "text-textStrong text-sm h-6 font-medium italic"] $ toHtml title
    mapM_ featureRow features


featureRow :: Text -> Html ()
featureRow feature =
  div_ [class_ "flex items-center gap-3"] $ do
    faSprite_ "feature-check" "regular" "h-4 text-textBrand shrink-0"
    p_ [class_ "text-sm text-textStrong leading-tight"] (toHtml feature)


navBar :: Html ()
navBar = do
  nav_ [id_ "main-navbar", class_ "fixed z-20 top-0 w-full w-full px-4 py-4 bg-base-100 flex flex-row justify-between"] do
    div_ [class_ "flex justify-between items-center gap-4 w-[1000px] mx-auto"] do
      a_ [href_ "https://monoscope.tech", class_ "flex items-center text-textWeak hover:text-textStrong"] do
        -- Only show full logos (no mini version needed for navbar)
        img_
          [ class_ "h-12 dark:hidden"
          , src_ "/public/assets/svgs/logo_black.svg"
          ]
        img_
          [ class_ "h-12 hidden dark:block"
          , src_ "/public/assets/svgs/logo_white.svg"
          ]


modal_ :: T.Text -> Html () -> Html () -> Html ()
modal_ modalId btnTrigger = modalWith_ modalId def (Just btnTrigger)


modalCloseButton_ :: Monad m => Text -> HtmlT m ()
modalCloseButton_ modalId = label_ [Lucid.for_ modalId, Aria.label_ "Close modal", class_ "btn btn-sm btn-circle btn-ghost !absolute right-2 top-2 tap-target"] "✕"


resizer_ :: Text -> Text -> Bool -> Html ()
resizer_ targetId urlParam increasingDirection =
  div_
    [ class_ "group px-r relative shrink-0 h-full flex items-center justify-center cursor-ew-resize overflow-visible select-none touch-none"
    , role_ "separator"
    , Aria.label_ "Resize panel"
    , term "data-resize-target" targetId
    , term "data-resize-direction" (if increasingDirection then "increase" else "decrease")
    , term "data-url-param" urlParam
    , [__|
        js 
          let rafId = null;
          let currentWidth = null;
          
          function applyMove(el, newWidth){
            currentWidth = newWidth;
            if (rafId) cancelAnimationFrame(rafId);
            rafId = requestAnimationFrame(() => {
              el.style.width = newWidth + 'px';
              rafId = null;
            });
          }
          return {applyMove}
        end
        on pointerdown
          add .select-none to body then
          set :startX to event.clientX then
          set :target to #{@data-resize-target} then
          set :startWidth to the :target's offsetWidth then
          set :urlParam to @data-url-param then
          set :isRightPanel to (@data-resize-direction == 'decrease') then
          set :lastWidth to :startWidth then
          call me.setPointerCapture(event.pointerId)
        end

        on pointermove from body
            if :startX is not null
                set deltaX to (event.clientX - :startX) then
                if :isRightPanel
                then set newWidth to :startWidth - deltaX
                else set newWidth to :startWidth + deltaX end
                if newWidth < 0 set newWidth to 0 end
                set :lastWidth to newWidth then
                call applyMove(:target, newWidth)
                then send "loglist-resize" to <body/>
            end
        end

        on pointerup from body or pointercancel
          if :startX is not null
            set finalWidth to :lastWidth then
            remove .select-none from body then

            call updateUrlState(:urlParam, finalWidth) then
            call localStorage.setItem('resizer-'+:urlParam, finalWidth + 'px') then
            set :startX to null
          end
        end
      |]
    ]
    $ div_ [class_ "h-full border-l hover:border-strokeBrand-strong"]
    $ div_
      [ id_ $ "resizer-" <> urlParam
      , class_ "absolute left-1/2 top-1/2 z-10 -translate-x-1/2 leading-none py-1 -translate-y-1/2 bg-bgBase rounded-sm border border-strokeBrand-weak group-hover:border-strokeBrand-strong text-iconNeutral group-hover:text-iconBrand"
      ]
    $ faSprite_ "grip-dots-vertical" "regular" "w-4 h-5"


-- Skeleton loaders for lazy-loaded content (dimensions match actual components to prevent layout shift)
-- Uses CSS grid with single shimmer overlay instead of rows*cols DOM elements
tableSkeleton_ :: Int -> Int -> Html ()
tableSkeleton_ rows cols = div_ [class_ "skeleton-table w-full", style_ $ "--skeleton-rows:" <> show rows <> ";--skeleton-cols:" <> show cols] ""


chartSkeleton_ :: Html ()
chartSkeleton_ = div_ [class_ "h-64 rounded-lg relative overflow-hidden bg-fillWeaker"] do
  -- Y-axis hint
  div_ [class_ "absolute left-0 top-4 bottom-8 w-px bg-strokeWeak"] ""
  -- X-axis hint
  div_ [class_ "absolute left-4 right-4 bottom-8 h-px bg-strokeWeak"] ""
  -- Shimmer bars representing data
  div_ [class_ "absolute left-8 right-4 top-8 bottom-12 flex items-end gap-2"] do
    div_ [class_ "flex-1 h-3/5 skeleton-shimmer rounded-t", style_ "animation-delay: 0s"] ""
    div_ [class_ "flex-1 h-2/5 skeleton-shimmer rounded-t", style_ "animation-delay: 0.1s"] ""
    div_ [class_ "flex-1 h-4/5 skeleton-shimmer rounded-t", style_ "animation-delay: 0.2s"] ""
    div_ [class_ "flex-1 h-1/2 skeleton-shimmer rounded-t", style_ "animation-delay: 0.3s"] ""
    div_ [class_ "flex-1 h-3/4 skeleton-shimmer rounded-t", style_ "animation-delay: 0.4s"] ""
    div_ [class_ "flex-1 h-2/5 skeleton-shimmer rounded-t", style_ "animation-delay: 0.5s"] ""
  -- Y-axis labels hint
  div_ [class_ "absolute left-1 top-6 w-3 h-2 skeleton-shimmer rounded"] ""
  div_ [class_ "absolute left-1 top-1/2 w-4 h-2 skeleton-shimmer rounded"] ""
  -- X-axis labels hint
  div_ [class_ "absolute left-8 bottom-3 w-6 h-2 skeleton-shimmer rounded"] ""
  div_ [class_ "absolute left-1/2 bottom-3 w-6 h-2 skeleton-shimmer rounded"] ""
  div_ [class_ "absolute right-4 bottom-3 w-6 h-2 skeleton-shimmer rounded"] ""


cardSkeleton_ :: Html ()
cardSkeleton_ = div_ [class_ "p-4 rounded-lg skeleton-shimmer"] do
  div_ [class_ "h-4 w-3/4 bg-fillWeak rounded mb-3"] ""
  div_ [class_ "h-3 w-1/2 bg-fillWeak rounded"] ""


-- Matches statBox_ dimensions (with icon: ~140px, without: ~120px)
statBoxSkeleton_ :: Bool -> Html ()
statBoxSkeleton_ withIcon = div_ [class_ "bg-fillWeaker rounded-3xl flex flex-col gap-3 p-5 border border-strokeWeak"] do
  when withIcon $ div_ [class_ "h-10 w-10 skeleton-shimmer rounded-xl"] ""
  div_ [class_ "flex flex-col gap-1"] do
    div_ [class_ $ "skeleton-shimmer rounded " <> if withIcon then "h-8 w-24" else "h-12 w-32"] ""
    div_ [class_ "flex gap-2 items-center"] do
      div_ [class_ "h-4 w-20 skeleton-shimmer rounded"] ""
      div_ [class_ "h-4 w-4 skeleton-shimmer rounded-full"] ""


data FieldSize = FieldSm | FieldMd


data FieldCfg = FieldCfg
  { icon :: Maybe Text
  , inputType :: Text
  , placeholder :: Text
  , value :: Text
  , extraAttrs :: [Attribute]
  , dot :: Maybe Text -- colored circle class before label, e.g. "bg-fillError-strong"
  , suffix :: Maybe Text -- text at right edge of input, e.g. "events"
  }


instance Default FieldCfg where def = FieldCfg Nothing "text" "" "" [] Nothing Nothing


-- | Unified form field: auto-generates input from cfg, or uses custom content when provided
formField_ :: Monad m => FieldSize -> FieldCfg -> Text -> Text -> Bool -> Maybe (HtmlT m ()) -> HtmlT m ()
formField_ size cfg lbl name required customM =
  fieldset_ [class_ wrapperCls] do
    label_ [class_ labelCls, Lucid.for_ name] do
      whenJust cfg.dot \color -> div_ [class_ $ "w-1.5 h-1.5 rounded-full shrink-0 " <> color] ""
      whenJust cfg.icon \ic -> faSprite_ ic "solid" "w-4 h-4 text-iconNeutral shrink-0"
      toHtml lbl
      when required $ span_ [class_ reqCls] "*"
    case customM of
      Just content -> content
      Nothing -> case (cfg.inputType, cfg.suffix) of
        ("textarea", _) -> textarea_ ([class_ textareaCls, name_ name, id_ name, placeholder_ cfg.placeholder] <> [required_ "true" | required] <> cfg.extraAttrs) $ toHtml cfg.value
        (_, Just sfx) -> div_ [class_ "relative"] do
          input_ $ [class_ $ inputCls <> " pr-14", value_ cfg.value, name_ name, id_ name, type_ cfg.inputType, placeholder_ cfg.placeholder] <> [required_ "true" | required] <> cfg.extraAttrs
          span_ [class_ "absolute right-2 top-1/2 -translate-y-1/2 text-xs text-textWeak"] $ toHtml sfx
        _ -> input_ $ [class_ inputCls, value_ cfg.value, name_ name, id_ name, type_ cfg.inputType, placeholder_ cfg.placeholder] <> [required_ "true" | required] <> cfg.extraAttrs
  where
    (wrapperCls, labelCls, inputCls, textareaCls, reqCls) = case size of
      FieldSm -> ("fieldset flex-1 min-w-0", "label text-xs text-textStrong", "input input-sm w-full", "textarea textarea-sm w-full", "text-textError")
      FieldMd -> ("fieldset", "label flex w-full items-center gap-1 text-textStrong", "input w-full h-12", "textarea w-full", "text-textWeak")


formSelectField_ :: Monad m => FieldSize -> Text -> Text -> Bool -> HtmlT m () -> HtmlT m ()
formSelectField_ size lbl name required options =
  fieldset_ [class_ wrapperCls] do
    label_ [class_ labelCls, Lucid.for_ name] do
      toHtml lbl
      when required $ span_ [class_ reqCls] "*"
    select_ ([class_ selectCls, name_ name, id_ name] <> [required_ "true" | required]) options
  where
    (wrapperCls, labelCls, selectCls, reqCls) = case size of
      FieldSm -> ("fieldset flex-1 min-w-0", "label text-xs text-textStrong", "select select-sm w-full", "text-textError")
      FieldMd -> ("fieldset", "label flex w-full items-center gap-1 text-textStrong", "select w-full h-12", "text-textWeak")


data PanelCfg = PanelCfg
  { icon :: Maybe Text
  , subtitle :: Maybe Text
  , collapsible :: Maybe Bool -- Nothing = static section, Just True = starts open, Just False = starts closed
  , sectionId :: Maybe Text -- id attribute on the details element
  , raised :: Bool -- surface-raised card wrapper
  }
  deriving stock (Generic)
  deriving anyclass (Default)


panel_ :: Monad m => PanelCfg -> Text -> HtmlT m () -> HtmlT m ()
panel_ cfg title content = case cfg.collapsible of
  Nothing -> wrapper do
    header do
      whenJust cfg.icon \ic -> faSprite_ ic "regular" "w-4 h-4 text-iconNeutral"
      toHtml title
      whenJust cfg.subtitle $ span_ [class_ "normal-case tracking-normal font-normal"] . toHtml
    content
    where
      wrapper = if cfg.raised then div_ [class_ "surface-raised rounded-2xl p-4 relative"] else div_ [class_ "space-y-4"]
      header = if cfg.raised then span_ [class_ "flex items-center gap-2 text-sm font-semibold text-textStrong mb-3"] else h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wider flex items-center gap-2"]
  Just startOpen ->
    details_ ([class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] <> [open_ "" | startOpen] <> maybe [] (pure . id_) cfg.sectionId) do
      summary_ [class_ "p-3 cursor-pointer list-none flex items-center justify-between gap-2 hover:bg-fillWeak transition-colors"] do
        div_ [class_ "flex items-center gap-2"] do
          whenJust cfg.icon \ic -> faSprite_ ic "regular" "w-3.5 h-3.5 text-iconNeutral"
          span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml title
          whenJust cfg.subtitle $ span_ [class_ "text-xs text-textWeak"] . toHtml
        faSprite_ "chevron-down" "regular" "w-3.5 h-3.5 text-iconNeutral"
      div_ [class_ "px-3 pb-3"] content


formCheckbox_ :: Monad m => FieldSize -> Text -> Text -> [Attribute] -> HtmlT m ()
formCheckbox_ FieldSm lbl name extraAttrs =
  label_ [class_ "flex items-center gap-2 text-xs cursor-pointer"] do
    input_ $ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ name] <> extraAttrs
    span_ [] $ toHtml lbl
formCheckbox_ FieldMd lbl name extraAttrs =
  label_ [class_ "label cursor-pointer flex items-center gap-2"] do
    input_ $ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ name] <> extraAttrs
    span_ [class_ "text-sm"] $ toHtml lbl


tagInput_ :: Monad m => Text -> Text -> [Attribute] -> HtmlT m ()
tagInput_ inputId ph attrs = textarea_ ([class_ "textarea w-full min-h-12 resize-none", id_ inputId, placeholder_ ph, data_ "tagify" ""] <> attrs) ""


formActionsModal_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
formActionsModal_ modalId submitBtn = div_ [class_ "mt-3 flex justify-end gap-2"] do
  label_ [Lucid.for_ modalId, class_ "btn btn-outline cursor-pointer"] "Cancel"
  submitBtn


connectionBadge_ :: Monad m => Text -> HtmlT m ()
connectionBadge_ status = span_ [class_ $ "badge badge-sm gap-1 " <> badgeCls] do
  whenJust iconM \icon -> faSprite_ icon "regular" "h-3 w-3"
  toHtml status
  where
    (badgeCls, iconM) = case status of
      "Active" -> ("badge-soft badge-success", Just "circle-check")
      "Connected" -> ("badge-soft badge-success", Just "circle-check")
      "Not connected" -> ("badge-soft badge-secondary", Just "circle-info")
      "Configure" -> ("badge-soft badge-secondary", Nothing)
      _ -> ("badge-soft badge-secondary", Nothing)


data BadgeColor = BrandBadge | SuccessBadge | ErrorBadge | NeutralBadge


iconBadge_ :: Monad m => BadgeColor -> Text -> HtmlT m ()
iconBadge_ = iconBadgeWith_ "p-2" "h-4 w-4" "rounded-full"


iconBadgeSq_ :: Monad m => BadgeColor -> Text -> HtmlT m ()
iconBadgeSq_ = iconBadgeWith_ "p-2" "h-4 w-4" "rounded-lg"


iconBadgeLg_ :: Monad m => BadgeColor -> Text -> HtmlT m ()
iconBadgeLg_ = iconBadgeWith_ "p-3" "h-6 w-6" "rounded-full"


iconBadgeXs_ :: Monad m => BadgeColor -> Text -> HtmlT m ()
iconBadgeXs_ = iconBadgeWith_ "p-1.5" "h-3.5 w-3.5" "rounded-md"


iconBadgeWith_ :: Monad m => Text -> Text -> Text -> BadgeColor -> Text -> HtmlT m ()
iconBadgeWith_ pad iconSize shape color icon =
  div_ [class_ $ pad <> " shrink-0 " <> shape <> " " <> bg color] $ faSprite_ icon "regular" (iconSize <> " " <> fg color)
  where
    bg = \case BrandBadge -> "bg-fillBrand-weak"; SuccessBadge -> "bg-fillSuccess-weak"; ErrorBadge -> "bg-fillError-weak"; NeutralBadge -> "bg-fillWeak"
    fg = \case BrandBadge -> "text-iconBrand"; SuccessBadge -> "text-iconSuccess"; ErrorBadge -> "text-iconError"; NeutralBadge -> "text-iconNeutral"


data ModalCfg = ModalCfg
  { autoOpen :: Bool
  , boxClass :: Text
  , boxStyle :: Text
  , wrapperClass :: Text
  , hideClose :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (Default)


modalWith_ :: T.Text -> ModalCfg -> Maybe (Html ()) -> Html () -> Html ()
modalWith_ modalId cfg triggerM contentHtml = do
  whenJust triggerM $ label_ [Lucid.for_ modalId]
  input_
    $ [ class_ "modal-toggle"
      , Lucid.id_ modalId
      , Lucid.type_ "checkbox"
      , Aria.label_ "Toggle modal"
      , [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup end
          on closeModal from body set my.checked to false end
          on change if my.checked then
            add .overflow-hidden to <body/>
            wait 50ms then
            set :modal to the next <.modal/> then
            set :focusable to :modal.querySelector('input:not([type=hidden]):not([type=checkbox]), textarea, select, [tabindex]:not([tabindex="-1"])') then
            if :focusable then call :focusable.focus() end
          else
            remove .overflow-hidden from <body/>
          end
      |]
      ]
    <> [checked_ | cfg.autoOpen]
  div_
    [ class_ $ "modal w-screen " <> cfg.wrapperClass
    , role_ "dialog"
    , Aria.label_ "Modal dialog"
    , style_ "--color-base-100: var(--color-fillWeaker)"
    , [__|on keydown[key=='Enter' and target.tagName=='INPUT' and target.type!='textarea'] from .modal-box
        set :form to target.closest('form') then
        if :form then call :form.requestSubmit() then halt end
      |]
    ]
    do
      label_ [class_ "modal-backdrop", Lucid.for_ modalId, Aria.label_ "Close modal"] ""
      div_
        ( [class_ $ "modal-box relative w-auto flex flex-col gap-5 " <> bool "max-w-5xl" cfg.boxClass (not $ T.null cfg.boxClass)]
            <> [style_ cfg.boxStyle | not $ T.null cfg.boxStyle]
        )
        do
          unless cfg.hideClose $ modalCloseButton_ modalId
          div_ [class_ "space-y-2"] contentHtml


confirmModal_ :: Text -> Text -> Text -> [Attribute] -> Text -> Html ()
confirmModal_ modalId title description confirmAttrs confirmText =
  modalWith_ modalId def{boxClass = "p-6", hideClose = True} Nothing do
    div_ [class_ "flex items-start gap-3 mb-4"] do
      iconBadgeWith_ "p-2" "h-5 w-5" "rounded-full" ErrorBadge "triangle-alert"
      div_ do
        h3_ [class_ "text-lg font-semibold text-textStrong"] $ toHtml title
        p_ [class_ "text-sm text-textWeak mt-1"] $ toHtml description
    div_ [class_ "flex justify-end gap-2 mt-6"] do
      label_ [class_ "btn btn-sm btn-ghost", Lucid.for_ modalId] "Cancel"
      button_ ([class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90"] <> confirmAttrs) $ toHtml confirmText
