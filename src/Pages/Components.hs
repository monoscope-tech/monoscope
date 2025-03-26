module Pages.Components (statBox, drawer_, statBox_, emptyState_, dateTime, paymentPlanPicker) where

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
import Utils (faSprite_)


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
    div_ [style_ "width: min(90vw, 1200px)", class_ "bg-bgRaised h-full overflow-y-scroll"] do
      div_
        ([id_ $ drawerId <> "-content", class_ "py-4 px-8 h-full flex flex-col gap-8", hxSwap_ "innerHTML"])
        $ div_ (maybe [] (\url -> [hxGet_ url, hxTrigger_ "intersect once"]) urlM)
        $ fromMaybe (span_ [class_ "loading loading-dots loading-md"] "") content


dateTime :: UTCTime -> Maybe UTCTime -> Html ()
dateTime t endTM = do
  span_ [class_ "flex items-center rounded-lg px-2 py-1.5 text-xs gap-2 border border-strokeWeak bg-fillWeaker text-textStrong"] do
    faSprite_ "calendar" "regular" "w-4 h-4 fill-none"
    toHtml $ formatTime defaultTimeLocale "%b. %d, %I:%M:%S %p" t
    whenJust endTM $ \endT -> do
      toHtml $ " - " <> formatTime defaultTimeLocale "%b. %d, %I:%M:%S %p" endT


paymentPlanPicker :: Projects.ProjectId -> Text -> Text -> Bool -> Html ()
paymentPlanPicker pid lemonUrl criticalUrl isSettings = do
  div_ [class_ "flex flex-col gap-8 w-full"] do
    div_ [class_ "flex flex-col gap-2 w-full"] do
      div_ [class_ "flex items-center justify-between w-full gap-4"] do
        p_ [class_ " text-textStrong"] "Total events"
        p_ [class_ " text-textWeak", id_ "num_requests"] "25 Million"
      input_ [type_ "range", min_ "25000000", max_ "500000000", step_ "10000000", value_ "25000000", class_ "range range-primary range-sm w-full", id_ "price_range"]
    div_ [class_ "grid grid-cols-2 gap-8 mt-6 w-full"] do
      popularPricing pid lemonUrl isSettings
      systemsPricing pid criticalUrl isSettings
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
                 let calculatedPrice = value <= 20_000_000 ? 32 : 32 + ((value- 20_000_000)/500_000)
                 priceContainer.innerText = "$" + calculatedPrice
                 reqsContainer.innerText = num_reqs + " Million"
               }

               price_indicator.addEventListener('input', priceChange)

               function handlePaymentPlanSelect(event, id) {
                event.stopPropagation()
                const target = event.currentTarget
                if(id === 'popularPlan') {
                  window.paymentPlanUrl = "$lemonUrl"
                  target.classList.add('border-[var(--brand-color)]')
                  document.querySelector('#systemsPlan').classList.remove('border-[var(--brand-color)]')
                } else {
                  window.paymentPlanUrl = "$criticalUrl"
                  target.classList.add('border-[var(--brand-color)]')
                  document.querySelector('#popularPlan').classList.remove('border-[var(--brand-color)]')
                }
               }
            |]


popularPricing :: Projects.ProjectId -> Text -> Bool -> Html ()
popularPricing pid lemonUrl isSettings = do
  div_
    [ class_ "flex flex-col gap-2 h-full w-full"
    , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
    , id_ "GraduatedPricing"
    , hxIndicator_ "#loadingIndicator"
    , hxVals_ "js:{orderId: document.querySelector('#popularPricing').value}"
    ]
    $ do
      div_
        [ class_ "flex flex-col gap-2 h-full rounded-2xl p-8 border border-[var(--brand-color)] flex-col flex gap-8 relative shadow-[0px_4px_8px_-2px_rgba(0,0,0,0.04)] shadow-[0px_2px_4px_-2px_rgba(0,0,0,0.08)]"
        , onclick_ "handlePaymentPlanSelect(event, 'popularPlan')"
        , id_ "popularPlan"
        ]
        do
          div_ [class_ "absolute -right-8 -top-8"] do
            div_ [class_ "relative"] do
              p_ [class_ "font-semibold text-brand"] "Most popular!"
              img_ [class_ "absolute top-1 -left-1/2 h-14 w-14", src_ "/public/assets/svgs/drawn-arrow.svg"]
          div_ [class_ "flex-col justify-start items-start gap-2 flex"] $ do
            input_ [type_ "hidden", class_ "orderId", id_ "popularPricing", name_ "ord", value_ ""]
            div_ [class_ "text-center  text-textStrong text-4xl font-bold"] "Pay as you use"
            div_ [class_ "text-brand text-base font-semibold"] "Start your FREE 30-day trial"
            div_ [class_ " text-textWeak text-sm font-medium"] do
              "Starts at "
              span_ [class_ "", id_ "price"] "$49"

          div_ [class_ "flex-col justify-start items-start gap-6 flex"] $ do
            span_ [class_ " text-textWeak text-base font-semibold"] "What’s Included:"
            mapM_ featureRow features
          unless isSettings do
            div_ [class_ "flex-col justify-start items-start gap-6 mt-auto flex", [__|on click halt|]] $ do
              button_
                [ class_ "btn-primary h-12 rounded-sm w-full font-semibold rounded-lg shadow-[0px_1px_2px_0px_rgba(10,13,18,0.05)] shadow-[inset_0px_-2px_0px_0px_rgba(10,13,18,0.05)] shadow-[inset_0px_0px_0px_1px_rgba(10,13,18,0.18)]"
                , term "_" [text|on click call window.payLemon("GraduatedPricing","$lemonUrl") |]
                , type_ "button"
                ]
                do
                  "Start free 30 day trial"
  where
    features =
      [ "Unlimited team members"
      , "Opentelemetry Logs, Traces and Metrics"
      , "Last 14 days data retention"
      ]


systemsPricing :: Projects.ProjectId -> Text -> Bool -> Html ()
systemsPricing pid critical isSettings = do
  div_
    [ class_ "flex flex-col gap-2 w-full"
    , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
    , id_ "SystemsPricing"
    , hxIndicator_ "#loadingIndicator"
    , hxVals_ "js:{orderId: document.querySelector('#systemsPricing').value}"
    ]
    $ do
      div_
        [ class_ "flex flex-col gap-2 h-full rounded-2xl p-8 border flex-col flex gap-8 relative shadow-[0px_4px_8px_-2px_rgba(0,0,0,0.04)] shadow-[0px_2px_4px_-2px_rgba(0,0,0,0.08)]"
        , onclick_ "handlePaymentPlanSelect(event, 'systemsPlan')"
        , id_ "systemsPlan"
        ]
        do
          div_ [class_ "flex-col justify-start items-start gap-2 flex"] $ do
            -- div_ [class_ "relative"] $ do
            --   span_ [class_ "text-brand font-semibold"] "Most popular!"
            input_ [type_ "hidden", class_ "orderId", id_ "systemsPricing", name_ "order", value_ ""]
            div_ [class_ "text-center  text-textStrong text-4xl font-bold"] "Critical Systems"
            div_ [class_ "text-base font-semibold"] "Business plan"
            div_ [class_ " text-textWeak text-sm font-medium"] "Starts at $500/monthly"

          div_ [class_ "flex-col justify-start items-start gap-6 flex"] $ do
            span_ [class_ " text-textWeak text-base font-semibold"] "Everything in plus and..."
            mapM_ featureRow features
          unless isSettings do
            div_ [class_ "flex-col justify-start items-start gap-6 mt-auto  flex", [__|on click halt|]] $ do
              button_
                [ class_ "btn-primary h-12 rounded-sm w-full font-semibold rounded-lg shadow-[0px_1px_2px_0px_rgba(10,13,18,0.05)] shadow-[inset_0px_-2px_0px_0px_rgba(10,13,18,0.05)] shadow-[inset_0px_0px_0px_1px_rgba(10,13,18,0.18)]"
                , term "_" [text|on click call window.payLemon("SystemsPricing", "$critical") |]
                , type_ "button"
                ]
                "Start free 30 day trial"
  where
    features =
      [ "24/7 support from our team of industry experts"
      , "Last 30 days data retention"
      ]


featureRow :: Text -> Html ()
featureRow feature =
  div_ [class_ "flex items-center gap-2"] $ do
    div_ [class_ "rounded-full bg-green-100 h-5 w-5 flex items-center justify-center"] do
      faSprite_ "check" "regular" "h-3 w-3 text-green-500"
    p_ [class_ " text-textWeak"] (toHtml feature)
