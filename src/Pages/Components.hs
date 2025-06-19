module Pages.Components (statBox, drawer_, statBox_, emptyState_, dateTime, paymentPlanPicker, navBar) where

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
statBox pid title helpInfo val bckupValM = do
  let tl = getTargetPage title
  let pidT = case pid of
        Just p -> p.toText
        Nothing -> ""
  if not (T.null tl)
    then do
      a_ [href_ $ "/p/" <> pidT <> tl, class_ "col-span-1 p-5 card-round flex flex-row content-between justify-between"] do
        div_ do
          div_ [class_ "inline-block flex flex-row content-between"] do
            span_ [class_ "font-bold text-textStrong text-2xl"] $ toHtml @Text $ fmt (commaizeF val)
            maybe "" (\bVal -> small_ $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
          span_ [class_ "text-textWeak"] $ toHtml title
        span_ [class_ "inline-block tooltip", term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 h-4"
    else do
      div_ [class_ "col-span-1 p-5 card-round col-span-1 flex flex-row content-between justify-between"] do
        div_ do
          div_ [class_ "inline-block flex flex-row content-between"] do
            span_ [class_ "font-bold text-textStrong text-2xl"] $ toHtml @Text $ fmt (commaizeF val)
            maybe "" (\bVal -> small_ $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
          span_ [class_ "text-textWeak"] $ toHtml title
        span_ [class_ "inline-block tooltip", term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 h-4"


statBox_ :: Maybe ProjectId -> Maybe (Text, Text, Text) -> Text -> Text -> Text -> Maybe Int -> Maybe Text -> Html ()
statBox_ pid iconM title helpInfo val bckupValM valClsM = do
  -- let tl = getTargetPage title
  -- let pidT = case pid of
  --       Just p -> p.toText
  --       Nothing -> ""
  div_ [class_ "bg-fillWeaker rounded-3xl flex flex-col gap-3 p-5 border border-strokeWeak"] do
    whenJust iconM $ \(icon, kind, color) -> do
      div_ [class_ "flex items-center justify-center h-10 w-10 bg-slate-50 rounded-xl"] do
        faSprite_ icon kind $ "w-4 h-4 " <> color
    div_ [class_ "flex flex-col gap-1"] do
      let fsiz = if isJust iconM then "text-2xl " else "text-4xl "
      span_ [class_ $ "font-bold  " <> fsiz <> fromMaybe "text-gray-800" valClsM] $ toHtml val
      div_ [class_ "flex gap-2 items-center text-sm text-gray-500"] do
        p_ [] $ toHtml title
        span_ [term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 mt-[-2px]"


emptyState_ :: Text -> Text -> Maybe Text -> Text -> Html ()
emptyState_ title subTxt url btnText =
  let (processedUrl, targetAttr) = maybe ("", []) (\u -> (u, [target_ "_blank" | "https://" `T.isPrefixOf` u])) url
   in section_ [class_ "w-max mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4"] do
        div_ [] $ faSprite_ "empty" "regular" "h-24 w-24 stroke-blue-500 fill-blue-500"
        div_ [class_ "flex flex-col gap-2"] do
          h2_ [class_ "text-xl text-slate-800 font-bold"] $ toHtml title
          p_ [class_ "text-sm font-medium text-gray-500"] $ toHtml subTxt
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


paymentPlanPicker :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
paymentPlanPicker pid lemonUrl criticalUrl currentPlan = do
  div_ [class_ "flex flex-col gap-8 w-full"] do
    div_ [class_ "flex flex-col gap-2 w-full"] do
      div_ [class_ "flex items-center justify-between w-full gap-4"] do
        p_ [class_ " text-textStrong"] "Total events"
        p_ [class_ " text-textWeak", id_ "num_requests"] "25 Million"
      input_ [type_ "range", min_ "20000000", max_ "500000000", step_ "10000000", value_ "20000000", class_ "range range-primary range-sm w-full", id_ "price_range"]
    div_ [class_ "flex flex-col gap-8 mt-6 w-full"] do
      div_ [class_ "grid grid-cols-3 gap-8 w-full"] do
        freePricing pid (currentPlan == "Free")
        popularPricing pid lemonUrl (currentPlan == "Pay as you use")
        systemsPricing pid criticalUrl (currentPlan == "Critical Systems Plan")
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
               const reqsContainer = document.querySelector("#num_requests")

               function priceChange() {
                 const value = price_indicator.value
                 let num_reqs = Math.floor(value/1000000)
                 let calculatedPrice = value <= 20_000_000 ? 34 : 34 + ((value- 20_000_000)/500_000)
                 priceContainer.innerText = calculatedPrice
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
    [ class_ "relative bg-white rounded-2xl py-11 px-4 outline outline-strokeWeak overflow-hidden"
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
              span_ [class_ ""] "Starts at"
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


popularPricing :: Projects.ProjectId -> Text -> Bool -> Html ()
popularPricing pid lemonUrl isCurrent = do
  div_ [class_ "relative"] do
    div_
      [ class_ "relative bg-white rounded-2xl py-11 px-4 outline overflow-hidden outline-strokeBrand-strong shadow-[0px_3px_3px_-1.5px_rgba(10,13,18,0.04)] shadow-[0px_8px_8px_-4px_rgba(10,13,18,0.03)] shadow-[0px_20px_24px_-4px_rgba(10,13,18,0.08)]"
      , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
      , id_ "GraduatedPricing"
      , hxIndicator_ "#loadingIndicator"
      , hxSwap_ "none"
      , hxVals_ "js:{orderIdM: document.querySelector('#popularPricing').value}"
      ]
      $ do
        div_ [class_ "w-[500px] h-36 right-0 top-0 rotate-y-15 rotate-z-15 top-[-55px] right-[-40px] rounded-t-2xl absolute bg-gradient-to-b from-blue-100 to-white"] pass
        div_
          [ class_ "relative flex flex-col gap-2 overflow-hidden"
          , onpointerdown_ "handlePaymentPlanSelect(event, 'popularPlan')"
          , id_ "popularPlan"
          ]
          do
            input_ [type_ "hidden", class_ "orderId", id_ "popularPricing", name_ "ord", value_ ""]
            div_ [class_ "flex-col justify-start items-start gap-1 flex"] $ do
              div_ [class_ "text-xl font-semibold text-textStrong"] "Pay as you use"
              div_ [class_ "text-textStrong text-sm"] "This plan can be adjusted"
            div_ [class_ "flex items-center gap-1 mt-4"] do
              div_ [class_ "flex items-end"] do
                span_ [class_ "text-textStrong text-xl"] "$"
                span_ [class_ "text-4xl text-textStrong", id_ "price"] "34"
              div_ [class_ "flex flex-col text-textWeak text-sm"] do
                span_ [class_ ""] "Starts at"
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
              "Everything in "
              span_ [class_ "text-textBrand"] "free"
              " plus..."
    div_ [class_ "px-3 py-1.5 bg-fillBrand-strong absolute top-0 left-1/2 -translate-x-1/2 -translate-y-1/2 rounded-lg inline-flex justify-center items-center gap-2"] do
      div_ [class_ "justify-start text-white"] do
        span_ [class_ "text-sm font-medium leading-tight"] "🌟"
        span_ [class_ "leading-tight text-sm"] "MOST POPULAR"
  where
    features =
      [ "Unlimited events per day"
      , "Unlimited team members"
      , "Opentelemetry Logs, Traces and Metrics"
      , "Last 14 days data retention"
      ]


systemsPricing :: Projects.ProjectId -> Text -> Bool -> Html ()
systemsPricing pid critical isCurrent = do
  div_
    [ class_ "relative bg-white rounded-2xl py-11 px-4 outline outline-strokeWeak overflow-hidden"
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
          div_ [class_ "w-full h-36  right-[-10px]  top-[-45px] absolute bg-gradient-to-bl from-slate-500/10 to-slate-500/0"] pass
          div_ [class_ "flex-col justify-start items-start gap-1 flex"] $ do
            div_ [class_ "text-xl font-semibold text-textStrong"] "Critical Systems"
            div_ [class_ "text-textStrong text-sm"] "Business plan"
          div_ [class_ "flex items-center gap-1 mt-4"] do
            div_ [class_ "flex items-end"] do
              span_ [class_ "text-textStrong text-xl"] "$"
              span_ [class_ "text-4xl text-textStrong"] "199"
            div_ [class_ "flex flex-col text-textWeak text-sm"] do
              span_ [class_ ""] "Starts at"
              span_ [class_ ""] "/per month"
          div_ [[__|on click halt|]] do
            button_
              [ class_ $ "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg " <> if isCurrent then "bg-fillDisabled cursor-not-allowed border-0 text-textInverse-strong" else "bg-fillStrong text-white"
              , term "_" [text|on click call window.payLemon("SystemsPricing", "$critical") |]
              , type_ "button"
              ]
              do
                if isCurrent then "Current plan" else "Start 30 day free trial"
          included features $ span_ [] do
            "Everything in "
            span_ [class_ "text-textBrand"] "pay as you use"
            " plus..."
  where
    features =
      [ "24/7 support from our team of industry experts"
      , "Last 30 days data retention"
      ]


included :: [Text] -> Html () -> Html ()
included features title =
  div_ [class_ "flex-col justify-start items-start gap-3 flex"] $ do
    span_ [class_ "text-textStrong text-sm font-medium italic"] $ toHtml title
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
      a_ [href_ "https://apitoolkit.io", class_ "flex items-center text-gray-500 hover:text-gray-700"] do
        img_
          [ class_ "h-12 sd-hidden"
          , src_ "/public/assets/svgs/logo.svg"
          ]
        img_
          [ class_ "h-12 w-10 hidden sd-show"
          , src_ "/public/assets/svgs/logo_mini.svg"
          ]
