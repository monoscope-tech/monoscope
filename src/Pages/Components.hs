module Pages.Components (drawer_, drawerLoadingSkeleton_, tableSkeleton_, deferredShell_, Deferred (..), withDeferredBody, emptyState_, EmptyStateCfg (..), EmptyStateSize (..), EmptyStateAction (..), facetRail_, facetSection_, facetOption_, factGrid_, metaChip_, resizer_, detailTab_, httpTab_, tabPanel_, jsonTab_, dateTime, localTime_, localTimeFmt_, paymentPlanPicker, navBar, modal_, modalCloseButton_, primaryButton_, headerRow_, chartSkeleton_, FieldSize (..), FieldCfg (..), formField_, formSelectField_, formCheckbox_, options_, PanelCfg (..), panel_, tagInput_, formActionsModal_, connectionBadge_, confirmModal_, BadgeColor (..), iconBadge_, iconBadgeLg_, iconBadgeXs_, iconBadgeWith_, ModalCfg (..), modalWith_, colorChip_, metadataChip_, getTargetPage, settingsSection_, settingsH2_, sectionLabel_, infoBanner_, settingsNavLink_, dirtyFormSaveAttr_, sparkline_, periodToggle_, abbreviateUnit, compactTimeAgo, stackTrace_, durationMenu_, durationQuery, untilLabel) where

import Data.Aeson qualified as AE
import Data.Default (Default (..))
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (makeAttribute, makeElement)
import Lucid.Htmx (hxGet_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.StackTrace qualified as StackTrace
import PyF qualified
import Relude
import Utils (LoadingSize (..), LoadingType (..), deleteParam, faSprite_, jsonValueToHtmlTree, loadingIndicator_, toUriStr)


data EmptyStateSize = ESFull | ESCompact
  deriving stock (Generic)
  deriving anyclass (Default)


-- | Empty-state action slot: a primary CTA link (url, label), arbitrary custom Html
-- (e.g. a close-panel button), or nothing. Generic 'Default' picks the first constructor
-- (ESNone), so the fields of ESLink/ESCustom need no Default instances of their own.
data EmptyStateAction = ESNone | ESLink Text Text | ESCustom (Html ())
  deriving stock (Generic)
  deriving anyclass (Default)


data EmptyStateCfg = EmptyStateCfg
  { icon :: Maybe Text -- Nothing → default "empty" glyph
  , size :: EmptyStateSize
  , action :: EmptyStateAction
  }
  deriving stock (Generic)
  deriving anyclass (Default)


-- | Unified empty / absent-data state. @ESFull@ is the page-level state (large brand icon,
-- heading, optional CTA); @ESCompact@ is the muted inline "nothing here" hint for small
-- cards/panels. Empty @subTxt@ renders no paragraph.
-- Usage: @emptyState_ def{icon = Just "chart-line", action = ESLink "/setup" "Get started"} "No data" "…"@
emptyState_ :: EmptyStateCfg -> Text -> Text -> Html ()
emptyState_ cfg title subTxt =
  section_ [class_ $ sectionCls <> " mx-auto text-center flex flex-col gap-3 empty-state"] do
    div_ [] $ faSprite_ (fromMaybe "empty" cfg.icon) "regular" iconCls
    div_ [class_ "flex flex-col gap-1.5"] do
      h2_ [class_ titleCls] $ toHtml title
      unless (T.null subTxt) $ p_ [class_ "text-sm text-textWeak max-w-md mx-auto"] $ toHtml subTxt
      case cfg.action of
        ESNone -> pass
        ESLink u txt ->
          unless (T.null txt)
            $ a_ ([href_ u, class_ "btn text-sm w-max mx-auto btn-primary"] <> bool [] [target_ "_blank", rel_ "noopener noreferrer"] ("https://" `T.isPrefixOf` u))
            $ toHtml txt
        ESCustom h -> h
  where
    (sectionCls, iconCls, titleCls) = case cfg.size of
      ESFull -> ("max-w-max my-8 p-5 sm:py-8 sm:px-12", "h-12 w-12 stroke-strokeBrand-strong fill-fillBrand-strong", "text-base text-textStrong font-semibold")
      ESCompact -> ("max-w-sm my-2 p-4", "h-6 w-6 text-iconNeutral", "text-sm text-textWeak")


-- | Shared shell for searchable facet trees. The content decides how filters change
-- (query-editor operations in Explorer, URL parameters in inventories); search,
-- accessibility, and disclosure markers stay identical.
facetRail_ :: Maybe Text -> Text -> Text -> Maybe (Html ()) -> Html () -> Html ()
facetRail_ elemId extraClass searchLabel actions content =
  div_ ([class_ $ "facet-rail flex flex-col gap-2 " <> extraClass, data_ "component" "facet-rail"] <> [id_ x | x <- maybeToList elemId]) do
    label_ [class_ "input input-sm sticky top-0 z-10 flex h-9 w-[calc(100%-1rem)] mx-2 items-center gap-2 border-strokeWeak bg-bgBase"] do
      faSprite_ "magnifying-glass" "regular" "h-3.5 w-3.5 text-iconNeutral"
      input_
        [ type_ "search"
        , name_ "facet-search"
        , class_ "grow py-1"
        , placeholder_ searchLabel
        , Aria.label_ searchLabel
        , oninput_ "const root=this.closest('[data-component=\"facet-rail\"]'),q=this.value.toLowerCase();root.querySelectorAll('[data-component=\"facet-option\"]').forEach(el=>el.classList.toggle('hidden',!el.textContent.toLowerCase().includes(q)));root.querySelectorAll('[data-component=\"facet-section\"]').forEach(el=>el.classList.toggle('hidden',!el.textContent.toLowerCase().includes(q)))"
        , onkeydown_ "if(event.key==='Escape'){this.value='';this.dispatchEvent(new Event('input',{bubbles:true}))}"
        ]
    whenJust actions id
    content


-- | Native details/summary facet disclosure shared by Explorer's lazy hierarchy
-- and inventory filter menus. Extra attributes must not include @class@.
facetSection_ :: Bool -> Text -> [Attribute] -> Html () -> Html () -> Html ()
facetSection_ startOpen extraClass attrs title content =
  details_
    ( [ class_ $ "facet-section block border-t border-strokeWeak [&[open]>summary_.facet-chevron]:rotate-0 " <> extraClass
      , data_ "component" "facet-section"
      ]
        <> attrs
        <> [open_ "" | startOpen]
    )
    do
      summary_ [class_ "flex cursor-pointer list-none items-center gap-2 rounded px-2 py-2 text-xs font-semibold text-textStrong hover:bg-fillWeak [&::-webkit-details-marker]:hidden"] do
        faSprite_ "chevron-down" "regular" "facet-chevron h-2.5 w-2.5 shrink-0 -rotate-90 transition-transform"
        div_ [class_ "min-w-0 flex-1"] title
      content


-- | Shared option row. Callers provide the control/label body and optional count.
facetOption_ :: Text -> [Attribute] -> Html () -> Html () -> Html ()
facetOption_ extraClass attrs body trailing =
  label_
    ( [ class_ $ "flex min-h-7 cursor-pointer items-center justify-between gap-2 rounded px-2 py-1 hover:bg-fillWeak " <> extraClass
      , data_ "component" "facet-option"
      ]
        <> attrs
    )
    do
      div_ [class_ "flex min-w-0 flex-1 items-center gap-2"] body
      trailing


-- | A row of labelled figures in a detail panel. @cols@ carries the responsive grid
-- utilities, since how many facts fit is the caller's decision, not the component's.
factGrid_ :: Text -> [(Text, Text)] -> Html ()
factGrid_ cols facts =
  dl_ [class_ $ "grid divide-x divide-strokeWeak rounded-lg border border-strokeWeak " <> cols] $ forM_ facts \(label, value) ->
    div_ [class_ "min-w-0 px-3 py-3"] do
      dt_ [class_ "text-xs text-textWeak"] $ toHtml label
      dd_ [class_ "mt-1 truncate font-semibold tabular-nums text-textStrong", term "data-tippy-content" value] $ toHtml value


-- | @label: value@ chip for the identity strip at the top of a detail panel.
metaChip_ :: Text -> Text -> Html ()
metaChip_ label value = span_ [class_ "inline-flex items-center gap-1 rounded-md border border-strokeWeak bg-fillWeak px-2 py-1 text-xs text-textWeak", data_ "tippy-content" $ label <> ": " <> value] do
  strong_ [class_ "font-medium text-textStrong"] $ toHtml $ label <> ":"
  toHtml value


-- | The section path a breadcrumb page title links back to, when it has one.
getTargetPage :: Text -> Maybe Text
getTargetPage = (`lookup` [("Requests", "/log_explorer"), ("Issues", "/issues"), ("Endpoints", "/endpoints")])


-- | Element with a non-standard tag name (custom elements, SVG), which Lucid has no builder for.
el_ :: Text -> [Attribute] -> Html () -> Html ()
el_ n = with (makeElement n)


-- | @startOpen@ controls whether the drawer renders expanded on load. Keep this
-- distinct from @content@ presence: URL-driven drawers (e.g. @?expand=@) pass
-- @startOpen = isJust content@ to reopen from a shared link, while drawers with
-- eager inline content that should stay collapsed pass @False@.
--
-- >>> let checks b = T.count "checked" $ toStrict $ renderText $ drawer_ "d" b Nothing (Just "x") ""
-- >>> checks True == checks False + 1
-- True
drawer_ :: Text -> Bool -> Maybe Text -> Maybe (Html ()) -> Html () -> Html ()
drawer_ drawerId startOpen urlM content trigger = div_ [class_ "drawer drawer-end inline-block w-auto"] do
  input_
    ( [ id_ drawerId
      , type_ "checkbox"
      , class_ "drawer-toggle"
      , Aria.label_ "Toggle drawer"
      ]
        <> [checked_ | startOpen]
        <> [ [__|on change
            if my.checked then
              add .overflow-hidden to <body/>
              set my._focusTrapCleanup to window.createFocusTrap(my.closest('.drawer').querySelector('.drawer-side > [role=dialog]'))
              set :closeBtn to my.closest('.drawer').querySelector('button[aria-label="Close drawer"]')
              if :closeBtn then call :closeBtn.focus() end
            else
              remove .overflow-hidden from <body/>
              if my._focusTrapCleanup then call my._focusTrapCleanup() end
            end
      |]
           ]
    )
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block", Aria.label_ "Open drawer"] trigger
  div_ [class_ "drawer-side top-0 left-0 w-full h-full flex z-10000 overflow-y-scroll "] do
    label_ [Lucid.for_ drawerId, Aria.label_ "Close drawer", class_ "w-full drawer-overlay grow flex-1"] ""
    div_
      [ id_ $ drawerId <> "-panel"
      , style_ "width: min(90vw, 1200px)"
      , class_ "bg-bgRaised fixed right-0 top-0 h-full overflow-y-auto overscroll-contain overflow-x-hidden w-full max-sm:w-screen! relative"
      , role_ "dialog"
      , term "aria-modal" "true"
      , Aria.label_ "Details"
      ]
      do
        div_ [class_ "sticky top-3 z-30 h-0 flex justify-end"]
          $ button_
            [ type_ "button"
            , Aria.label_ "Close drawer"
            , class_ "btn btn-sm btn-circle btn-ghost mr-3 text-iconNeutral hover:bg-fillWeak hover:text-iconBrand max-sm:size-11"
            , term "_" $ "on click set #" <> drawerId <> ".checked to false then trigger change on #" <> drawerId
            ]
          $ faSprite_ "xmark" "regular" "h-3.5 w-3.5"
        div_
          [ id_ $ drawerId <> "-content"
          , class_ "pb-4 px-8 h-full flex flex-col gap-8"
          , term "hx-on::after:swap" "window.evalScriptsFromContent(this); window.labelDrawer(this.closest('[role=dialog]'))"
          ]
          -- hx-swap sits on the requester rather than being inherited from the wrapper:
          -- htmx 4 resolves inheritance explicitly, so a parent's value no longer reaches here.
          $ div_ (maybe [] (\url -> [hxGet_ url, hxTrigger_ "intersect once", hxSwap_ "innerHTML"]) urlM)
          $ fromMaybe (loadingIndicator_ LdMD LdDots) content
        div_ [id_ $ drawerId <> "-indicator", class_ "htmx-indicator absolute inset-0 z-10 w-full box-border bg-bgRaised px-8"] drawerLoadingSkeleton_


dateTime :: UTCTime -> Maybe UTCTime -> Html ()
dateTime t endTM = do
  let utcOf = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"
      title = utcOf t <> foldMap ((" — " <>) . utcOf) endTM
  span_
    [ class_ "flex items-center rounded-lg px-2 py-1.5 text-xs gap-2 border border-strokeWeak bg-fillWeaker text-textStrong whitespace-nowrap shrink-0"
    , Lucid.title_ (toText title)
    ]
    do
      faSprite_ "calendar" "regular" "w-4 h-4 fill-none"
      localTime_ t
      whenJust endTM \endT -> " - " >> localTime_ endT


-- | A UTC instant rendered as a <local-time> custom element, which reformats it
-- into the viewer's timezone client-side (matching the virtual log list). The
-- server-rendered UTC text is the no-JS fallback; the ISO @datetime@ is the
-- machine value the element reads.
localTime_ :: UTCTime -> Html ()
localTime_ = localTimeFmt_ "MMM dd, HH:mm:ss"


-- | 'localTime_' with an explicit date-fns @dfFmt@ pattern the <local-time> element
-- applies client-side (in the viewer's zone). The no-JS fallback is the instant in
-- UTC — a fixed readable form, so callers never hand-maintain a second pattern.
localTimeFmt_ :: Text -> UTCTime -> Html ()
localTimeFmt_ dfFmt ts =
  el_
    "local-time"
    [makeAttribute "datetime" (toText $ iso8601Show ts), makeAttribute "format" dfFmt]
    (toHtml $ formatTime defaultTimeLocale "%b %d, %H:%M:%S UTC" ts)


paymentPlanPicker :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Bool -> Projects.BillingProvider -> Html ()
paymentPlanPicker pid lemonUrl criticalUrl currentPlan freePricingEnabled basicAuthEnabled isOnboarding provider = do
  let gridCols = bool "grid-cols-1 md:grid-cols-2" "grid-cols-1 md:grid-cols-3" (freePricingEnabled && not basicAuthEnabled)
      useStripe = provider /= Projects.LemonSqueezyProvider
  div_ ([class_ "flex flex-col gap-8 w-full"] <> [hxVals_ "{\"isOnboarding\": true}" | isOnboarding]) do
    unless basicAuthEnabled $ div_ [class_ "flex flex-col gap-2 w-full"] do
      div_ [class_ "flex items-center justify-between w-full gap-4"] do
        p_ [class_ " text-textStrong"] "Total events"
        p_ [class_ " text-textWeak", id_ "num_requests"] "25 Million"
      input_ [type_ "range", min_ "20000000", max_ "500000000", step_ "10000000", value_ "20000000", class_ "range range-primary range-sm w-full", id_ "price_range"]
    div_ [class_ "flex flex-col gap-8 mt-6 w-full"] do
      div_ [class_ $ "grid gap-8 w-full " <> gridCols] do
        let isCurrent p = not isOnboarding && currentPlan == p
        if basicAuthEnabled
          then openSourcePricing pid (isCurrent "Open Source") >> enterprisePricing
          else do
            when freePricingEnabled $ freePricing pid (isCurrent "Free")
            popularPricing pid lemonUrl (isCurrent "Bring nothing") freePricingEnabled useStripe
            systemsPricing pid criticalUrl (isCurrent "Bring your own storage") useStripe
    unless useStripe
      $ script_ [src_ "https://assets.lemonsqueezy.com/lemon.js"] ("" :: Text)
    -- Guarded: #price_range/#price only exist when the usage slider is rendered.
    unless basicAuthEnabled
      $ script_
        [text|
               const price_indicator = document.querySelector("#price_range");
               const priceContainer = document.querySelector("#price")
               const criticalContainer = document.querySelector("#critical_price")
               const reqsContainer = document.querySelector("#num_requests")

               function priceChange() {
                 const value = price_indicator.value
                 let num_reqs = Math.floor(value/1000000)
                 let calculatedPrice = value <= 20_000_000 ? 29 : 29 + ((value- 20_000_000)/1_000_000)
                 let calculatedPriceCritical = value <= 100_000_000 ? 199 : 199 + ((value - 100_000_000)/1_000_000)
                 priceContainer.innerText = calculatedPrice
                 criticalContainer.innerText = calculatedPriceCritical
                 reqsContainer.innerText = num_reqs + " Million"
               }

               price_indicator.addEventListener('input', priceChange)
            |]
    unless useStripe
      $ script_
        [text|
             window.payLemon = function(plan, url) {
             if (typeof LemonSqueezy === 'undefined') { window.open(url, '_blank'); return; }
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
            |]


pricingContent_ :: Text -> Text -> Html () -> Html () -> [Text] -> Html () -> Html ()
pricingContent_ title subtitle priceEl ctaEl features featuresTitle = do
  div_ [class_ "flex-col justify-start items-start gap-1 flex"] do
    div_ [class_ "text-xl font-semibold text-textStrong"] $ toHtml title
    div_ [class_ "text-textStrong text-sm"] $ toHtml subtitle
  div_ [class_ "flex items-center gap-1 mt-4"] priceEl
  ctaEl
  included features featuresTitle


priceDisplay_ :: [Attribute] -> Text -> Text -> Html ()
priceDisplay_ extraAttrs amount subtext = do
  div_ [class_ "flex items-end"] do
    span_ [class_ "text-textStrong text-xl"] "$"
    span_ ([class_ "text-4xl text-textStrong"] <> extraAttrs) $ toHtml amount
  div_ [class_ "flex flex-col text-textWeak text-sm"] $ span_ [] $ toHtml subtext


pricingPostAttrs :: Projects.ProjectId -> Text -> Text -> [Attribute] -> [Attribute]
pricingPostAttrs pid cardId outlineCls extras =
  [ class_ $ "relative bg-bgRaised rounded-2xl py-11 px-4 outline overflow-hidden " <> outlineCls
  , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/pricing"
  , id_ cardId
  , hxSwap_ "none"
  ]
    <> extras


pricingBtnCls :: Bool -> Text -> Text
pricingBtnCls isCurrent normalCls = "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg " <> if isCurrent then "bg-fillDisabled cursor-not-allowed border-0 text-textInverse-strong" else normalCls


-- | Plan CTA: inert when already the current plan, else a Stripe checkout POST or a LemonSqueezy popup.
pricingCta_ :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Html ()
pricingCta_ pid plan normalCls lemonUrl isCurrent useStripe =
  div_ [[__|on click halt|]]
    $ button_ ([class_ $ pricingBtnCls isCurrent normalCls, type_ "button"] <> attrs)
    $ if isCurrent then "Current plan" else "Start 30 day free trial"
  where
    attrs :: [Attribute]
    attrs
      | isCurrent = []
      | useStripe = [hxPost_ $ "/p/" <> pid.toText <> "/stripe_checkout", hxVals_ $ "{\"plan\": \"" <> plan <> "\"}", hxSwap_ "none"]
      | otherwise = [term "_" $ "on click call window.payLemon(\"" <> plan <> "\", \"" <> lemonUrl <> "\")"]


pricingBadge_ :: Html () -> Html () -> Html ()
pricingBadge_ badge content = div_ [class_ "relative"] do
  content
  div_ [class_ "px-3 py-1.5 bg-fillBrand-strong absolute top-0 left-1/2 -translate-x-1/2 -translate-y-1/2 rounded-lg inline-flex justify-center items-center gap-2"]
    $ div_ [class_ "justify-start text-white"] badge


freePricing :: Projects.ProjectId -> Bool -> Html ()
freePricing pid isCurrent =
  div_ (pricingPostAttrs pid "freePricing" "outline-strokeWeak" [])
    $ div_ [class_ "flex flex-col gap-2 h-full relative"] do
      div_ [class_ "pricing-gradient-slate"] pass
      pricingContent_
        "Free tier"
        "Free forever"
        (priceDisplay_ [] "0" "/per month")
        (div_ [[__|on click halt|]] $ button_ [class_ $ pricingBtnCls isCurrent "bg-fillStrong text-textInverse-strong", [__| on click htmx.trigger("#freePricing", "click")|], type_ "button"] $ if isCurrent then "Current plan" else "Start free")
        ["10K events per day", "1 team member", "Opentelemetry Logs, Traces and Metrics", "Last 30 days data retention"]
        "What's included:"


popularPricing :: Projects.ProjectId -> Text -> Bool -> Bool -> Bool -> Html ()
popularPricing pid lemonUrl isCurrent freeTierEnabled useStripe =
  div_ [class_ "relative"]
    $ div_ (pricingPostAttrs pid "GraduatedPricing" "outline-strokeWeak shadow-[0px_3px_3px_-1.5px_rgba(10,13,18,0.04)] shadow-[0px_8px_8px_-4px_rgba(10,13,18,0.03)] shadow-[0px_20px_24px_-4px_rgba(10,13,18,0.08)]" [hxVals_ "js:{orderIdM: document.querySelector('#popularPricing').value}"]) do
      div_ [class_ "pricing-gradient-slate-wide"] pass
      div_ [class_ "relative flex flex-col gap-2 overflow-hidden"] do
        input_ [type_ "hidden", class_ "orderId", id_ "popularPricing", name_ "ord", value_ ""]
        pricingContent_
          "Bring nothing"
          "This plan can be adjusted"
          (priceDisplay_ [id_ "price"] "29" "/per month")
          (pricingCta_ pid "GraduatedPricing" "btn-primary" lemonUrl isCurrent useStripe)
          ["Fully managed cloud service", "Predictable usage-based pricing", "Intelligent incident alerts", "Query your data in english", "30 days data retention included"]
          (span_ [] $ when freeTierEnabled $ "Everything in " >> span_ [class_ "text-textBrand"] "free" >> " plus...")


systemsPricing :: Projects.ProjectId -> Text -> Bool -> Bool -> Html ()
systemsPricing pid critical isCurrent useStripe =
  pricingBadge_ (span_ [class_ "text-sm font-medium leading-tight"] "🌟" >> span_ [class_ "leading-tight text-sm"] "MOST POPULAR")
    $ div_ (pricingPostAttrs pid "SystemsPricing" "outline-strokeBrand-strong" [hxVals_ "js:{orderIdM: document.querySelector('#systemsPricing').value}"])
    $ div_ [class_ "flex flex-col gap-2"] do
      input_ [type_ "hidden", class_ "orderId", id_ "systemsPricing", name_ "ord", value_ ""]
      div_ [class_ "pricing-gradient-brand-wide"] pass
      pricingContent_
        "Bring your own storage"
        "Business plan"
        (priceDisplay_ [id_ "critical_price"] "199" "/per month")
        (pricingCta_ pid "SystemsPricing" "bg-fillStrong text-textInverse-strong" critical isCurrent useStripe)
        ["Own and control all your data", "Save all your data to any S3-compatible bucket", "Unlimited data retention period", "Query years of data via monoscope", "No extra cost for data retention"]
        (span_ [] $ "Everything in " >> span_ [class_ "text-textBrand"] "bring nothing" >> " plus...")


openSourcePricing :: Projects.ProjectId -> Bool -> Html ()
openSourcePricing pid isCurrent =
  div_ (pricingPostAttrs pid "openSourcePricing" "outline-strokeWeak" [hxVals_ "{\"plan\": \"Open Source\"}"])
    $ div_ [class_ "flex flex-col gap-2 h-full relative"] do
      div_ [class_ "w-full h-36 right-[-20px] top-[-45px] absolute bg-gradient-to-bl from-green-500/10 to-green-500/0"] pass
      pricingContent_
        "Open Source Community Edition"
        "Self-hosted deployment"
        (priceDisplay_ [] "0" "Free forever")
        (div_ $ button_ ([class_ $ pricingBtnCls isCurrent "bg-green-700 hover:bg-green-600 text-white", type_ "submit"] <> [disabled_ "disabled" | isCurrent]) $ if isCurrent then "Current plan" else "Continue with Open Source")
        ["Unlimited events", "Unlimited team members", "Self-hosted deployment", "Full control over your data", "Community support", "All APItoolkit features"]
        "What's included:"


enterprisePricing :: Html ()
enterprisePricing =
  pricingBadge_ (span_ [class_ "leading-tight text-sm"] "RECOMMENDED FOR TEAMS")
    $ div_ [class_ "relative bg-bgRaised rounded-2xl py-11 px-4 outline outline-strokeBrand-strong overflow-hidden"]
    $ div_ [class_ "flex flex-col gap-2"] do
      div_ [class_ "pricing-gradient-brand-wide"] pass
      pricingContent_
        "Enterprise"
        "Custom plan for your team"
        (div_ [class_ "flex items-end"] (span_ [class_ "text-4xl text-textStrong"] "Custom") >> div_ [class_ "flex flex-col text-textWeak text-sm"] (span_ [] "pricing"))
        (div_ $ a_ [class_ "btn btn-primary mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg", href_ "https://monoscope.tech/pricing", target_ "_blank"] "Contact us")
        ["Premium features & integrations", "SSO & advanced auth", "Priority support & SLA", "Advanced security & compliance"]
        (span_ [] $ "Everything in " >> span_ [class_ "text-textBrand"] "open source" >> " plus...")


included :: [Text] -> Html () -> Html ()
included features title =
  div_ [class_ "flex-col justify-start items-start gap-3 flex"] do
    div_ [class_ "text-textStrong text-sm h-6 font-medium italic"] title
    forM_ features \feature -> div_ [class_ "flex items-center gap-3"] do
      faSprite_ "feature-check" "regular" "h-4 text-iconBrand shrink-0"
      p_ [class_ "text-sm text-textStrong leading-tight"] $ toHtml feature


navBar :: Html ()
navBar =
  nav_ [id_ "main-navbar", class_ "fixed z-20 top-0 w-full px-4 py-4 bg-bgOverlay flex flex-row justify-between"]
    $ div_ [class_ "flex justify-between items-center gap-4 w-[1000px] mx-auto"]
    $ a_ [href_ "https://monoscope.tech", class_ "flex items-center text-textWeak hover:text-textStrong"] do
      img_ [class_ "h-12 dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
      img_ [class_ "h-12 hidden dark:block", src_ "/public/assets/svgs/logo_white.svg"]


modal_ :: Text -> Html () -> Html () -> Html ()
modal_ modalId btnTrigger = modalWith_ modalId def (Just btnTrigger)


modalCloseButton_ :: Monad m => Text -> HtmlT m ()
modalCloseButton_ modalId = label_ [Lucid.for_ modalId, Aria.label_ "Close modal", class_ "btn btn-sm btn-circle btn-ghost !absolute right-2 top-2 tap-target"] "✕"


primaryButton_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
primaryButton_ attrs = button_ (class_ " btn btn-primary btn-sm " : attrs)


headerRow_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
headerRow_ attrs = div_ (class_ " flex items-center justify-between " : attrs)


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


-- CSS-only tabs (no JS): a hidden radio per tab styles its own label active via
-- has-[:checked], and its marker class drives the matching panel's visibility via
-- group-has-[.marker:checked] on the scoped container. Survives HTMX morphs; wrap the
-- nav in @[__|on click halt the event's bubbling|]@ to keep the native label→radio toggle
-- while stopping the click from reaching ancestor handlers. @grp@ (the radio name) must be
-- per-instance so two tab bars on one page can't cross-uncheck each other.
detailTab_ :: Text -> Text -> Text -> Bool -> Html () -> Html ()
detailTab_ grp marker extra active inner =
  label_ [class_ $ "cursor-pointer border-b-2 border-b-strokeWeak px-4 py-1.5 text-sm has-[:focus-visible]:ring-2 has-[:focus-visible]:ring-strokeBrand-strong has-[:focus-visible]:rounded-sm has-[:checked]:font-bold has-[:checked]:border-strokeBrand-strong has-[:checked]:text-textBrand " <> extra] do
    input_ $ [type_ "radio", name_ grp, class_ $ "sr-only " <> marker] <> [checked_ | active]
    inner


-- Nested "box" tabs (e.g. inside an HTTP request panel); active styling mirrors @t-tab-box-active@.
httpTab_ :: Text -> Text -> Bool -> Html () -> Html ()
httpTab_ grp marker active inner =
  label_ [class_ "cursor-pointer px-3 py-1 rounded-lg text-textWeak has-[:focus-visible]:ring-2 has-[:focus-visible]:ring-strokeBrand-strong has-[:checked]:shadow-[0px_4px_8px_-2px_rgba(0,0,0,0.04)] has-[:checked]:text-textStrong has-[:checked]:border has-[:checked]:border-strokeStrong has-[:checked]:bg-bgOverlay"] do
    input_ $ [type_ "radio", name_ grp, class_ $ "sr-only " <> marker] <> [checked_ | active]
    inner


-- Content panel revealed when its tab's radio is checked. @visCls@ MUST be the full,
-- literal @group-has-[.MARKER:checked]/GRP:block@ class (plus any extra panel classes,
-- space-separated) written out at the call site — Tailwind's scanner only compiles complete
-- literal class strings, so building this by runtime concatenation silently produces CSS that
-- never exists. MARKER must match the tab's radio class and GRP the @group/GRP@ container.
tabPanel_ :: Text -> Text -> Html () -> Html ()
tabPanel_ visCls elemId = div_ [class_ $ "hidden " <> visCls, id_ elemId]


-- A tab content panel holding a JSON tree; the common shape behind most tab bodies. @visCls@
-- is the literal group-has class — see 'tabPanel_'.
jsonTab_ :: Text -> Text -> AE.Value -> Maybe Text -> Html ()
jsonTab_ visCls elemId val filt = tabPanel_ visCls elemId $ jsonValueToHtmlTree val filt


-- >>> T.isInfixOf "pt-12" (toStrict $ renderText drawerLoadingSkeleton_)
-- True
drawerLoadingSkeleton_ :: Html ()
drawerLoadingSkeleton_ = div_ [class_ "flex w-full flex-col gap-5 pt-12", role_ "status", Aria.label_ "Loading metric detail"] do
  div_ [class_ "flex items-center justify-between gap-4 border-b border-strokeWeak pb-3"] do
    div_ [class_ "flex flex-col gap-2"] do
      div_ [class_ "h-4 w-52 rounded skeleton-shimmer"] ""
      div_ [class_ "h-3 w-20 rounded skeleton-shimmer"] ""
    div_ [class_ "flex gap-2"] do
      div_ [class_ "h-8 w-32 rounded-lg skeleton-shimmer"] ""
      div_ [class_ "h-8 w-24 rounded-lg skeleton-shimmer"] ""
      div_ [class_ "h-8 w-8 rounded-lg skeleton-shimmer"] ""
  chartSkeleton_
  div_ [class_ "flex flex-col gap-3 rounded-2xl border border-strokeWeak p-4"] do
    div_ [class_ "h-3 w-24 rounded skeleton-shimmer"] ""
    div_ [class_ "h-3 w-full rounded skeleton-shimmer"] ""
    div_ [class_ "h-3 w-3/5 rounded skeleton-shimmer"] ""


chartSkeleton_ :: Html ()
chartSkeleton_ = div_ [class_ "h-64 w-full rounded-lg relative overflow-hidden bg-fillWeaker"] do
  div_ [class_ "absolute left-0 top-4 bottom-8 w-px bg-strokeWeak"] ""
  div_ [class_ "absolute left-4 right-4 bottom-8 h-px bg-strokeWeak"] ""
  div_ [class_ "absolute left-8 right-4 top-8 bottom-12 flex items-end gap-2"]
    $ forM_ (["h-3/5", "h-2/5", "h-4/5", "h-1/2", "h-3/4", "h-2/5"] `zip` ["0s", "0.1s", "0.2s", "0.3s", "0.4s", "0.5s"])
    $ \(hCls, delay) -> div_ [class_ $ "flex-1 skeleton-shimmer rounded-t " <> hCls, style_ $ "animation-delay: " <> delay] ""
  div_ [class_ "absolute left-1 top-6 w-3 h-2 skeleton-shimmer rounded"] ""
  div_ [class_ "absolute left-1 top-1/2 w-4 h-2 skeleton-shimmer rounded"] ""
  div_ [class_ "absolute left-8 bottom-3 w-6 h-2 skeleton-shimmer rounded"] ""
  div_ [class_ "absolute left-1/2 bottom-3 w-6 h-2 skeleton-shimmer rounded"] ""
  div_ [class_ "absolute right-4 bottom-3 w-6 h-2 skeleton-shimmer rounded"] ""


-- | A page body whose query is too slow to hold up first paint. The first request answers
-- with 'DeferredShell' — a skeleton that immediately re-fetches the same URL with the
-- handler's defer parameter set — and that second request answers with 'DeferredBody'.
--
-- Both branches live under the same container id, so the response swaps itself in place and
-- the surrounding chrome (nav, tabs, time picker) is never re-rendered. That chrome is what
-- the user navigates by, so it must arrive in milliseconds regardless of query cost.
data Deferred a = DeferredShell Text Text (Html ()) | DeferredBody a


-- | Run @load@ only on the request that carries the defer parameter. The first request pays
-- for nothing but chrome.
withDeferredBody :: Monad m => Maybe Text -> Text -> Text -> Html () -> m a -> m (Deferred a)
withDeferredBody deferredM containerId url skeleton load
  | isNothing deferredM = pure $ DeferredShell containerId url skeleton
  | otherwise = DeferredBody <$> load


instance ToHtml a => ToHtml (Deferred a) where
  toHtml (DeferredBody body) = toHtml body
  toHtml (DeferredShell containerId url skeleton) = toHtmlRaw $ deferredShell_ containerId url [] skeleton
  toHtmlRaw = toHtml


-- | The skeleton half of 'Deferred', also usable on its own for a panel whose body is one
-- fragment of a larger page.
deferredShell_ :: Text -> Text -> [Attribute] -> Html () -> Html ()
deferredShell_ containerId url extra =
  div_
    ( [ id_ containerId
      , class_ "w-full"
      , -- Marks the page as still waiting on its body. Tests wait for this to disappear
        -- instead of a fixed timeout, which is what stops a deferred page from quietly
        -- passing an assertion it never actually ran against real content.
        data_ "deferred-shell" ""
      , hxGet_ url
      , hxTrigger_ "load"
      , hxTarget_ "this"
      , hxSwap_ "outerHTML"
      , hxSelect_ $ "#" <> containerId
      , -- The body inherits @hx-preload: mouseover@; without this the shell would also fetch
        -- itself on hover, doubling the very request it exists to make once.
        makeAttribute "hx-preload" "false"
      ]
        -- Must not carry `class`: Lucid concatenates duplicate class attributes with no
        -- separator, which would corrupt both this element's classes and the caller's.
        <> extra
    )


-- | Placeholder for a table that has not loaded yet: a header strip and @rows@ row bars.
-- Deliberately not an empty table — a zero state that turns into rows a second later reads
-- as "nothing here" during the moment a user is deciding whether the page is broken.
tableSkeleton_ :: Int -> Html ()
tableSkeleton_ rows = div_ [class_ "flex w-full flex-col gap-3 px-4 pt-4", role_ "status", Aria.label_ "Loading"] do
  div_ [class_ "flex items-center gap-3"] do
    div_ [class_ "h-8 w-64 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "ml-auto h-8 w-28 rounded-lg skeleton-shimmer"] ""
  div_ [class_ "flex flex-col gap-2 rounded-lg border border-strokeWeak p-3"]
    $ replicateM_ rows
    $ div_ [class_ "flex items-center gap-4"] do
      div_ [class_ "h-3 grow rounded skeleton-shimmer"] ""
      div_ [class_ "h-3 w-24 shrink-0 rounded skeleton-shimmer max-md:hidden"] ""
      div_ [class_ "h-3 w-16 shrink-0 rounded skeleton-shimmer"] ""


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
    fromMaybe
      ( case (cfg.inputType, cfg.suffix) of
          ("textarea", _) -> textarea_ ([class_ textareaCls, name_ name, id_ name, placeholder_ cfg.placeholder] <> commonAttrs) $ toHtml cfg.value
          (_, Just sfx) -> div_ [class_ "relative"] do
            input_ $ [class_ $ inputCls <> " pr-14"] <> inputAttrs
            span_ [class_ "absolute right-2 top-1/2 -translate-y-1/2 text-xs text-textWeak"] $ toHtml sfx
          _ -> input_ $ [class_ inputCls] <> inputAttrs
      )
      customM
  where
    commonAttrs = [required_ "true" | required] <> cfg.extraAttrs
    inputAttrs = [value_ cfg.value, name_ name, id_ name, type_ cfg.inputType, placeholder_ cfg.placeholder] <> commonAttrs
    (wrapperCls, labelCls, inputCls, textareaCls, reqCls) = case size of
      FieldSm -> ("fieldset flex-1 min-w-0", "label text-xs text-textStrong", "input input-sm w-full", "textarea textarea-sm w-full leading-relaxed", "text-textError")
      FieldMd -> ("fieldset", "label flex w-full items-center gap-1 text-textStrong", "input w-full h-12", "textarea w-full", "text-textWeak")


-- | @\<option\>@ list from (value, label) pairs, marking the given value selected.
options_ :: Monad m => Maybe Text -> [(Text, Text)] -> HtmlT m ()
options_ sel = mapM_ \(v, label) -> option_ (value_ v : [selected_ "" | sel == Just v]) $ toHtml label


formSelectField_ :: Monad m => FieldSize -> Text -> Text -> Bool -> HtmlT m () -> HtmlT m ()
formSelectField_ size lbl name required options =
  formField_ size def lbl name required
    $ Just
    $ select_ ([class_ selectCls, name_ name, id_ name] <> [required_ "true" | required]) options
  where
    selectCls = case size of
      FieldSm -> "select select-sm w-full cursor-pointer"
      FieldMd -> "select w-full h-12 cursor-pointer"


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
formCheckbox_ size lbl name extraAttrs =
  label_ [class_ lblCls] do
    input_ $ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ name] <> extraAttrs
    span_ [class_ spanCls] $ toHtml lbl
  where
    (lblCls, spanCls) = case size of
      FieldSm -> ("flex items-center gap-2 text-xs cursor-pointer", "")
      FieldMd -> ("label cursor-pointer flex items-center gap-2", "text-sm")


tagInput_ :: Monad m => Text -> Text -> [Attribute] -> HtmlT m ()
tagInput_ inputId ph attrs = textarea_ ([class_ "textarea w-full min-h-12 resize-none", id_ inputId, placeholder_ ph, data_ "tagify" ""] <> attrs) ""


formActionsModal_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
formActionsModal_ modalId submitBtn = div_ [class_ "mt-3 flex justify-end gap-2"] do
  label_ [Lucid.for_ modalId, class_ "btn cursor-pointer"] "Cancel"
  submitBtn


connectionBadge_ :: Monad m => Text -> HtmlT m ()
connectionBadge_ status = span_ [class_ $ "badge badge-sm gap-1 " <> badgeCls] do
  whenJust iconM \icon -> faSprite_ icon "regular" "h-3 w-3"
  toHtml status
  where
    (badgeCls, iconM)
      | status `elem` ["Active", "Connected"] = ("badge-soft badge-success", Just "circle-check")
      | status == "Not connected" = ("badge-soft badge-secondary", Just "circle-info")
      | otherwise = ("badge-soft badge-secondary", Nothing)


data BadgeColor = BrandBadge | SuccessBadge | ErrorBadge | NeutralBadge


iconBadge_ :: Monad m => BadgeColor -> Text -> HtmlT m ()
iconBadge_ = iconBadgeWith_ "p-2" "h-4 w-4" "rounded-full"


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


modalWith_ :: Text -> ModalCfg -> Maybe (Html ()) -> Html () -> Html ()
modalWith_ modalId cfg triggerM contentHtml = do
  whenJust triggerM $ label_ [Lucid.for_ modalId]
  input_
    $ [ class_ "modal-toggle"
      , id_ modalId
      , type_ "checkbox"
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


colorChip_ :: Monad m => Text -> Text -> Text -> HtmlT m ()
colorChip_ color icon label = span_ [class_ $ "inline-flex items-center gap-1.5 text-xs rounded-full px-2.5 py-1 " <> bool "text-textWeak bg-fillWeaker" color (color /= "")] do
  faSprite_ icon "regular" "h-3 w-3"
  toHtml label


metadataChip_ :: Monad m => Text -> Text -> HtmlT m ()
metadataChip_ = colorChip_ ""


-- | Standard settings page layout: scrollable container with padded, max-width section
settingsSection_ :: Monad m => HtmlT m () -> HtmlT m ()
settingsSection_ body = div_ [class_ "w-full h-full overflow-y-auto"] do
  section_ [class_ "py-6 px-4 sm:py-8 sm:px-8 lg:px-12 max-w-4xl space-y-6"] body


settingsH2_ :: Monad m => Text -> HtmlT m ()
settingsH2_ = h2_ [class_ "text-textStrong text-xl sm:text-2xl font-bold tracking-tight"] . toHtml


-- | Uppercase section label used in settings subsections
sectionLabel_ :: Monad m => Text -> HtmlT m ()
sectionLabel_ = h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wider"] . toHtml


-- | Info banner with brand background and info icon
infoBanner_ :: Monad m => HtmlT m () -> HtmlT m ()
infoBanner_ content = div_ [class_ "rounded-lg bg-fillBrand-weak p-3 text-xs text-textStrong"] do
  faSprite_ "circle-info" "regular" "h-3.5 w-3.5 inline mr-1.5"
  content


-- | Navigation link row for settings pages (icon + title + description + chevron)
settingsNavLink_ :: Monad m => Text -> Text -> Text -> Text -> HtmlT m ()
settingsNavLink_ href icon title desc =
  a_ [href_ href, class_ "flex items-center justify-between gap-3 p-3 group hover:bg-fillWeaker transition-colors first:rounded-t-xl last:rounded-b-xl"] do
    div_ [class_ "flex items-center gap-3 min-w-0"] do
      faSprite_ icon "solid" "h-4 w-4 text-iconNeutral shrink-0"
      div_ [class_ "min-w-0"] do
        span_ [class_ "text-sm font-medium text-textStrong block"] $ toHtml title
        p_ [class_ "text-xs text-textWeak"] $ toHtml desc
    faSprite_ "chevron-right" "regular" "w-3 h-3 text-iconNeutral shrink-0"


-- | Hyperscript attribute for a save button that activates when its parent form changes
dirtyFormSaveAttr_ :: Attribute
dirtyFormSaveAttr_ = [__| on change from closest <form/> remove @disabled from me then remove .btn-ghost from me then remove .text-textWeak from me then add .btn-primary to me |]


-- | Abbreviate time unit (e.g., "hours" → "hrs")
--
-- >>> abbreviateUnit "hours"
-- "hrs"
-- >>> abbreviateUnit "minute"
-- "min"
-- >>> abbreviateUnit "days"
-- "days"
abbreviateUnit :: Text -> Text
abbreviateUnit w = fromMaybe w $ lookup w [("hours", "hrs"), ("hour", "hr"), ("minutes", "mins"), ("minute", "min"), ("seconds", "secs"), ("second", "sec")]


-- | Compact time ago display (e.g., "23 hrs ago" instead of "23 hours ago")
compactTimeAgo :: Text -> Text
compactTimeAgo = unwords . map abbreviateUnit . words


-- | Popover offering silence durations plus an indefinite option — the shared
-- vocabulary behind monitor mute and issue acknowledge, so "for how long?" is
-- answered the same way everywhere. @req@ turns the chosen duration in minutes
-- (empty for indefinite) into the htmx attributes that submit it, leaving each
-- caller its own verb and swap semantics.
durationMenu_ :: Text -> Text -> (Text -> [Attribute]) -> (Text -> Html ()) -> Html ()
durationMenu_ popId heading req trigger = div_ [class_ "inline-block"] do
  trigger popId
  -- The options read as bare durations ("4 hours") once focus lands inside, so
  -- the heading has to be the group's accessible name, not just visible text.
  div_ [id_ popId, term "popover" "auto", role_ "group", Aria.label_ heading, class_ "dropdown dropdown-start menu bg-bgRaised p-1 text-sm border border-strokeWeak z-50 min-w-36 rounded-md shadow-lg mt-1", style_ $ "position-try: flip-block; position-anchor: --anchor-" <> popId] do
    span_ [class_ "px-3 py-1 text-xs font-medium text-textWeak", Aria.hidden_ "true"] $ toHtml heading
    forM_ @[] @_ @(Int, Text) [(60, "1 hour"), (240, "4 hours"), (480, "8 hours"), (1440, "1 day"), (4320, "3 days"), (10080, "1 week")] \(mins, label) ->
      button_ ([type_ "button", class_ itemCls] <> req (show mins)) $ toHtml label
    button_ ([type_ "button", class_ $ itemCls <> " border-t border-strokeWeak"] <> req "") "Indefinitely"
  where
    itemCls = "px-3 py-1.5 text-sm text-left hover:bg-fillWeaker rounded cursor-pointer w-full"


-- | Query suffix for a 'durationMenu_' choice: @?param=minutes@, or nothing at
-- all for the indefinite option (absent duration means "no end").
--
-- >>> durationQuery "duration" "240"
-- "?duration=240"
-- >>> durationQuery "duration" ""
-- ""
durationQuery :: Text -> Text -> Text
durationQuery param q = if T.null q then "" else "?" <> param <> "=" <> q


-- | Human label for a silence window. Anything more than a year out is the
-- indefinite sentinel; anything already past reads as expired rather than
-- rounding up to a misleading "1m left".
--
-- >>> import Data.Time (UTCTime (..), fromGregorian)
-- >>> let t0 = UTCTime (fromGregorian 2026 1 1) 0
-- >>> untilLabel "Muted" t0 (UTCTime (fromGregorian 2026 1 1) (4 * 3600))
-- "Muted \183 4h left"
-- >>> untilLabel "Ack'd" t0 (UTCTime (fromGregorian 2126 1 1) 0)
-- "Ack'd indefinitely"
-- >>> untilLabel "Ack'd" t0 (UTCTime (fromGregorian 2025 12 31) (23 * 3600))
-- "Ack'd \183 expired"
untilLabel :: Text -> UTCTime -> UTCTime -> Text
untilLabel what now until'
  | diffMins <= 0 = what <> " \xb7 expired"
  | diffMins > 525600 = what <> " indefinitely"
  | diffMins >= 1440 = what <> " \xb7 " <> show (diffMins `div` 1440) <> "d left"
  | diffMins >= 60 = what <> " \xb7 " <> show (diffMins `div` 60) <> "h left"
  | otherwise = what <> " \xb7 " <> show (max 1 diffMins) <> "m left"
  where
    diffMins = round (diffUTCTime until' now / 60) :: Int


periodToggle_ :: Text -> Text -> Text -> Html ()
periodToggle_ baseUrl targetId currentPeriod =
  span_ [class_ "inline-flex rounded-md border border-strokeWeak overflow-hidden", role_ "group", Aria.label_ "Activity window"] do
    forM_ ["24h" :: Text, "7d" :: Text] \val ->
      let url = deleteParam "period" baseUrl <> "&period=" <> val
       in button_
            [ class_ $ "cursor-pointer px-2 py-0.5 text-xs font-medium transition-colors " <> bool "text-textWeak hover:bg-fillWeaker hover:text-textStrong" "bg-fillWeak text-textStrong" (val == currentPeriod)
            , type_ "button"
            , Aria.label_ $ "Show " <> val <> " activity"
            , term "aria-pressed" $ bool "false" "true" (val == currentPeriod)
            , hxGet_ url
            , hxTarget_ $ "#" <> targetId
            , hxSelect_ $ "#" <> targetId
            , hxSwap_ "outerHTML"
            , hxPushUrl_ "true"
            ]
            $ toHtml val


sparkline_ :: [Int] -> Html ()
sparkline_ buckets
  | null buckets || all (== 0) buckets = span_ [class_ "text-textWeak text-xs"] "-"
  | otherwise = do
      let peakVal = foldr max 1 buckets
          peak = fromIntegral @Int @Double peakVal
          n = length buckets
          h = 40 :: Int
          barZone = 32 :: Int
          peakLabel = formatCompact peakVal
          labelW = T.length peakLabel * 9 + 4
          gap = 2 :: Int
          barW = max 2 (120 `div` n - gap)
          barsEnd = n * (barW + gap)
          topPad = h - barZone
          -- fall back to 0 when nothing matches peakVal (all-negative buckets,
          -- where the seed 1 wins) so the marker stays on-chart
          peakIdx = maybe 0 fst $ find ((== peakVal) . snd) $ zip [0 ..] buckets
          lineX1 = peakIdx * (barW + gap) + barW
          lineX2 = barsEnd + 2
          w = lineX2 + labelW
      el_
        "svg"
        [ makeAttribute "viewBox" $ toText [PyF.fmt|0 0 {w} {h}|]
        , style_ [PyF.fmt|width:100%;height:{h}px|]
        , makeAttribute "preserveAspectRatio" "xMinYMid meet"
        ]
        $ do
          forM_ (zip [0 ..] buckets) \(i, v) -> do
            let barH = max 2 (round @Double @Int $ (fromIntegral v / peak) * fromIntegral barZone)
            el_
              "rect"
              [ makeAttribute "x" $ show $ i * (barW + gap)
              , makeAttribute "y" $ show $ h - barH
              , makeAttribute "width" $ show barW
              , makeAttribute "height" $ show barH
              , makeAttribute "rx" "1.5"
              , makeAttribute "fill" "var(--color-chart-default)"
              , makeAttribute "opacity" "0.7"
              ]
              ""
          el_
            "line"
            [ makeAttribute "x1" $ show lineX1
            , makeAttribute "y1" $ show topPad
            , makeAttribute "x2" $ show lineX2
            , makeAttribute "y2" $ show topPad
            , makeAttribute "stroke" "var(--color-textWeak)"
            , makeAttribute "stroke-width" "0.7"
            , makeAttribute "stroke-dasharray" "3,2"
            ]
            ""
          el_
            "text"
            [ makeAttribute "x" $ show (lineX2 + 1)
            , makeAttribute "y" $ show (topPad + 4)
            , makeAttribute "text-anchor" "start"
            , makeAttribute "fill" "var(--color-textWeak)"
            , makeAttribute "font-size" "11"
            , makeAttribute "font-family" "system-ui"
            ]
            $ toHtml peakLabel
  where
    formatCompact n
      | n >= 1000000 = toText [PyF.fmt|{fromIntegral @Int @Double n / 1000000:.1f}M|]
      | n >= 1000 = toText [PyF.fmt|{fromIntegral @Int @Double n / 1000:.1f}K|]
      | otherwise = show n


-- | A stack trace as frames rather than a blob.
--
-- Rendering the raw string in a @\<pre\>@ is what this replaces: it told a reader nothing the
-- string didn't already say, and it left nothing for the panel to hang a source snippet on.
-- Each frame is a native @\<details\>@ — the disclosure, its open state, and the screen-reader
-- announcement all come for free, and it survives an HTMX morph because there is no JS state
-- to lose.
--
-- The first in-app frame is open by default, which is Sentry's default and the right one
-- during an incident: the top of a stack is usually five frames of framework before anything
-- you wrote. @snippetUrl@ is asked for a frame's surrounding source only when that frame is
-- actually opened, because resolving it is a network call the error panel must not block on.
stackTrace_ :: Projects.ProjectId -> Maybe Text -> (Text -> Maybe Text) -> (Text -> Maybe Text) -> Text -> Html ()
stackTrace_ pid svcM attr resAttr stack = case StackTrace.framesFor attr stack of
  [] -> pass
  frames ->
    let firstInApp = fst <$> find (StackTrace.isInApp . snd) (zip [0 :: Int ..] frames)
     in div_ [class_ "flex flex-col rounded-md border border-strokeWeak bg-bgBase overflow-hidden"]
          $ forM_ (zip [0 :: Int ..] frames) \(i, f) -> frame_ (Just i == firstInApp) (snippetUrl f) f
  where
    -- 'Nothing' for a frame we could not place — the panel then says "no source linked"
    -- rather than firing a request that can only fail.
    snippetUrl f = do
      path <- f.file
      n <- f.line
      pure
        $ "/p/"
        <> pid.toText
        <> "/code_context?file="
        <> toUriStr path
        <> "&line="
        <> show n
        <> foldMap (("&service=" <>) . toUriStr) svcM
        -- The revision the span reported, so the snippet is the source that threw rather
        -- than the source as it is now.
        <> foldMap (("&revision=" <>) . toUriStr) (StackTrace.revisionFor resAttr)
    -- Signature pins the block to `Html ()`. Without it Lucid's `Term` leaves each element
    -- polymorphic in its result, and the summary inside the details reads as a discarded
    -- value under -Wunused-do-bind.
    frame_ :: Bool -> Maybe Text -> StackTrace.Frame -> Html ()
    frame_ open urlM f = case f.file of
      -- A line we could not resolve to a file keeps its text verbatim and gets no
      -- disclosure: there is nothing behind it to disclose, and a details with an empty
      -- panel is a control that lies about having more to show.
      Nothing -> div_ [class_ "px-3 py-1 font-mono text-2xs text-textWeak whitespace-pre-wrap break-words border-b border-strokeWeak last:border-0"] $ toHtml f.raw
      Just path ->
        details_ ([class_ "group/fr border-b border-strokeWeak last:border-0"] <> [open_ "" | open]) do
          summary_ [class_ "flex items-center gap-2 px-3 py-1.5 cursor-pointer select-none hover:bg-fillWeaker text-xs"] do
            faSprite_ "chevron-right" "regular" "w-3 h-3 shrink-0 text-iconNeutral transition-transform group-open/fr:rotate-90"
            span_ [class_ $ "font-mono truncate " <> bool "text-textWeak" "text-textStrong font-medium" (StackTrace.isInApp f)] $ toHtml path
            whenJust f.line \n -> span_ [class_ "font-mono tabular-nums text-textWeak shrink-0"] $ toHtml @Text (":" <> show n)
            whenJust f.function \fn -> span_ [class_ "text-textWeak truncate max-md:hidden"] $ toHtml ("in " <> fn)
            -- In-app is a heuristic over the path, so it is labelled rather than encoded
            -- only in weight: a reader has to be able to tell it apart from emphasis.
            when (StackTrace.isInApp f) $ span_ [class_ "ml-auto shrink-0 rounded-sm border border-strokeWeak px-1 text-2xs text-textWeak"] $ toHtml @Text "in app"
          case urlM of
            Nothing -> div_ [class_ "px-3 pb-2 pl-8 text-2xs text-textWeak italic"] "No source linked for this file."
            Just url ->
              div_
                [ class_ "px-3 pb-2"
                , hxGet_ url
                , -- `intersect once`, not `load`: the panel holds every frame, and fetching
                  -- thirty blobs from the SCM API to show one is how a rate limit is spent.
                  hxTrigger_ "intersect once"
                , hxTarget_ "this"
                , hxSwap_ "innerHTML"
                ]
                $ div_ [class_ "pl-5 py-1 text-2xs text-textWeak"] "Loading source…"
