module Pages.Components (statBox, drawer_, statBox_, emptyState_, dateTime, paymentPlanPicker, navBar, modal_) where

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
import Utils (faSprite_, onpointerdown_)


statBox :: Maybe ProjectId -> Text -> Text -> Int -> Maybe Int -> Html ()
statBox pid title helpInfo val bckupValM = wrapper do
  div_ do
    div_ [class_ "inline-block flex flex-row content-between"] do
      span_ [class_ "font-bold text-textStrong text-2xl"] $ toHtml @Text $ fmt (commaizeF val)
      maybe "" (\bVal -> small_ $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
    span_ [class_ "text-textWeak"] $ toHtml title
  span_ [class_ "inline-block tooltip", term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 h-4"
  where
    tl = getTargetPage title
    pidT = maybe "" (.toText) pid
    wrapper =
      if T.null tl
        then div_ [class_ "col-span-1 p-5 card-round col-span-1 flex flex-row content-between justify-between"]
        else a_ [href_ $ "/p/" <> pidT <> tl, class_ "col-span-1 p-5 card-round flex flex-row content-between justify-between"]


statBox_ :: Maybe ProjectId -> Maybe (Text, Text, Text) -> Text -> Text -> Text -> Maybe Int -> Maybe Text -> Html ()
statBox_ pid iconM title helpInfo val bckupValM valClsM = do
  -- let tl = getTargetPage title
  -- let pidT = case pid of
  --       Just p -> p.toText
  --       Nothing -> ""
  div_ [class_ "bg-fillWeaker rounded-3xl flex flex-col gap-3 p-5 border border-strokeWeak"] do
    whenJust iconM $ \(icon, kind, color) -> do
      div_ [class_ "flex items-center justify-center h-10 w-10 bg-fillWeaker rounded-xl"] do
        faSprite_ icon kind $ "w-4 h-4 " <> color
    div_ [class_ "flex flex-col gap-1"] do
      let fsiz = if isJust iconM then "text-2xl " else "text-4xl "
      span_ [class_ $ "font-bold  " <> fsiz <> fromMaybe "text-textStrong" valClsM] $ toHtml val
      div_ [class_ "flex gap-2 items-center text-sm text-textWeak"] do
        p_ [] $ toHtml title
        span_ [term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 mt-[-2px]"


emptyState_ :: Text -> Text -> Maybe Text -> Text -> Html ()
emptyState_ title subTxt url btnText =
  let (processedUrl, targetAttr) = maybe ("", []) (\u -> (u, [target_ "_blank" | "https://" `T.isPrefixOf` u])) url
   in section_ [class_ "w-max mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4"] do
        div_ [] $ faSprite_ "empty" "regular" "h-24 w-24 stroke-strokeBrand-strong fill-fillBrand-strong"
        div_ [class_ "flex flex-col gap-2"] do
          h2_ [class_ "text-xl text-textStrong font-bold"] $ toHtml title
          p_ [class_ "text-sm font-medium text-textWeak"] $ toHtml subTxt
          a_ ([href_ processedUrl, class_ "btn text-sm w-max mx-auto btn-primary"] ++ targetAttr) $ toHtml btnText


getTargetPage :: Text -> Text
getTargetPage "Requests" = "/log_explorer"
getTargetPage "Anomalies" = "/anomalies"
getTargetPage "Endpoints" = "/endpoints"
getTargetPage _ = ""


drawer_ :: Text -> Maybe Text -> Maybe (Html ()) -> Html () -> Html ()
drawer_ drawerId urlM content trigger = div_ [class_ "drawer drawer-end inline-block w-auto"] do
  input_ [id_ drawerId, type_ "checkbox", class_ "drawer-toggle", [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup |]]
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block"] trigger
  div_ [class_ "drawer-side top-0 left-0 w-full h-full flex z-10000 overflow-y-scroll "] do
    label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "w-full drawer-overlay grow flex-1"] ""
    div_ [style_ "width: min(90vw, 1200px)", class_ "bg-bgRaised h-full overflow-y-scroll overflow-x-hidden w-full"] do
      div_
        [id_ $ drawerId <> "-content", class_ "py-4 px-8 h-full flex flex-col gap-8", hxSwap_ "innerHTML"]
        $ div_ (maybe [] (\url -> [hxGet_ url, hxTrigger_ "intersect once"]) urlM)
        $ fromMaybe (span_ [class_ "loading loading-dots loading-md"] "") content


dateTime :: UTCTime -> Maybe UTCTime -> Html ()
dateTime t endTM = do
  span_ [class_ "flex items-center rounded-lg px-2 py-1.5 text-xs gap-2 border border-strokeWeak bg-fillWeaker text-textStrong"] do
    faSprite_ "calendar" "regular" "w-4 h-4 fill-none"
    toHtml $ formatTime defaultTimeLocale "%b. %d, %I:%M:%S %p" t
    whenJust endTM $ \endT -> do
      toHtml $ " - " <> formatTime defaultTimeLocale "%b. %d, %I:%M:%S %p" endT


paymentPlanPicker :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Html ()
paymentPlanPicker pid lemonUrl criticalUrl currentPlan freePricingEnabled basicAuthEnabled = do
  let gridCols =
        if basicAuthEnabled
          then "grid-cols-1"
          else
            if freePricingEnabled
              then "grid-cols-3"
              else "grid-cols-2"
  div_ [class_ "flex flex-col gap-8 w-full"] do
    div_ [class_ "flex flex-col gap-2 w-full"] do
      div_ [class_ "flex items-center justify-between w-full gap-4"] do
        p_ [class_ " text-textStrong"] "Total events"
        p_ [class_ " text-textWeak", id_ "num_requests"] "25 Million"
      input_ [type_ "range", min_ "20000000", max_ "500000000", step_ "10000000", value_ "20000000", class_ "range range-primary range-sm w-full", id_ "price_range"]
    div_ [class_ "flex flex-col gap-8 mt-6 w-full"] do
      div_ [class_ $ "grid gap-8 w-full " <> gridCols] do
        when basicAuthEnabled $ openSourcePricing pid (currentPlan == "Open Source")
        when (freePricingEnabled && not basicAuthEnabled) $ freePricing pid (currentPlan == "Free")
        when (not basicAuthEnabled) $ popularPricing pid lemonUrl (currentPlan == "Bring nothing") freePricingEnabled
        when (not basicAuthEnabled) $ systemsPricing pid criticalUrl (currentPlan == "Bring your own storage")
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
                 let calculatedPrice = value <= 20_000_000 ? 34 : 34 + ((value- 20_000_000)/500_000)
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
                  <> if isCurrent then [disabled_ "disabled"] else []
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
  nav_ [id_ "main-navbar", class_ "fixed z-20 top-0 w-full w-full px-6 py-4 bg-base-100 flex flex-row justify-between"] do
    div_ [class_ "flex justify-between items-center gap-4 w-[1000px] mx-auto"] do
      a_ [href_ "https://apitoolkit.io", class_ "flex items-center text-textWeak hover:text-textStrong"] do
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
modal_ modalId btnTrigger contentHtml = do
  label_ [Lucid.for_ modalId] btnTrigger
  input_
    [ class_ "modal-toggle"
    , Lucid.id_ modalId
    , Lucid.type_ "checkbox"
    , [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup end
          on closeModal from body set my.checked to false end
      |]
    ]
  div_ [class_ "modal w-screen", role_ "dialog", style_ "--color-base-100: var(--color-fillWeaker)"] do
    label_ [class_ "modal-backdrop", Lucid.for_ modalId] ""
    div_ [class_ "modal-box w-auto flex flex-col gap-5 max-w-5xl"] do
      label_ [Lucid.for_ modalId, class_ "btn btn-sm btn-circle btn-ghost absolute right-2 top-2"] "✕"
      div_ contentHtml
