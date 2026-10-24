module Pages.Components (drawer_, PanelStyle (..), PanelCfg (..), panel_, entityHeader_, detailSection_, pivotLinks_, disclosure_, settingsSection_, settingsH2_, sectionLabel_, fieldLabel_, headerRow_, navBar, brandLogo_, resizer_, resizerWith_, detailTab_, httpTab_, tabPanel_, segmented_, periodToggle_, Deferred (..), RefreshingDeferred (..), withDeferredBody, deferredShell_, lazyPanel_, timeRefreshListener_, drawerLoadingSkeleton_, tableSkeleton_, chartSkeleton_, EmptyStateCfg (..), EmptyStateSize (..), EmptyStateAction (..), emptyState_, callout_, BadgeColor (..), IconBadgeSize (..), iconBadge_, tonePill_, colorChip_, metadataChip_, metaChip_, connectionBadge_, silenceBadge_, statFigure_, factGrid_, kvList_, sparkline_, eventsCountCell_, lastSeenCell_, dateTime, localTimeFmt_, FieldSize (..), FieldCfg (..), formField_, formSelectField_, formCheckbox_, options_, roleOptions_, tagInput_, toggleRow_, searchInput_, copyField_, copyButton_, facetRail_, facetSection_, facetOption_, ModalCfg (..), modalWith_, formActionsModal_, dangerAction_, floatingDialog_, dialogField_, kbdHints_, durationMenu_, primaryButton_, inlineIconBtn_, RowAction (..), rowActions_, externalLink_, installationSettingsLink_, dirtyFormSaveAttr_, resetFormOnSuccessAttr_, detailsClosedBelowAttr_, keyboardActivateAttr_, listboxNavAttr_, copySourceAttr_, filterInputAttr_, TraceFrame (..), stackTrace_, stackFrames_, codeContextUrl, curlCommand, getTargetPage, abbreviateUnit, agoText, durationQuery, untilLabel, paymentPlanPicker) where

import Data.Default (Default (..))
import Data.List (elemIndex, findIndex, lookup)
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
import Text.Time.Pretty (prettyTimeAuto)
import Utils (LoadingSize (..), LoadingType (..), copyToClipboardAttr_, countNoun, deleteParam, faSprite_, formatUTC, formatWithCommas, loadingIndicator_, navTabAttrs, popoverPanel_, toUriStr)


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
      -- p, not h2: an empty state announces a condition, it does not open a document
      -- section. As an h2 it outranked the page's own subject heading in the outline
      -- (a screen reader heard "No stack trace in this event" before the issue title).
      -- titleCls is unchanged, so nothing moves visually.
      p_ [class_ titleCls] $ toHtml title
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


-- | The search box: magnifying glass + @type=search@ input labelled @ph@. @extra@ sizes
-- the wrapper; @attrs@ carry the behavior (a 'filterInputAttr_', a form name, htmx).
searchInput_ :: Text -> Text -> [Attribute] -> Html ()
searchInput_ extra ph attrs = label_ [class_ $ "input input-sm flex items-center gap-2 " <> extra] do
  faSprite_ "magnifying-glass" "regular" "h-3.5 w-3.5 shrink-0 text-iconNeutral"
  input_ $ [type_ "search", class_ "min-w-0 grow bg-transparent max-md:text-base", placeholder_ ph, Aria.label_ (T.dropWhileEnd (== '…') ph)] <> attrs


-- | Shared shell for searchable facet trees. The content decides how filters change
-- (query-editor operations in Explorer, URL parameters in inventories); search,
-- accessibility, and disclosure markers stay identical.
facetRail_ :: Maybe Text -> Text -> Text -> Maybe (Html ()) -> Html () -> Html ()
facetRail_ elemId extraClass searchLabel actions content =
  div_ ([class_ $ "facet-rail flex flex-col gap-2 " <> extraClass, data_ "component" "facet-rail"] <> [id_ x | x <- maybeToList elemId]) do
    searchInput_
      "sticky top-0 z-10 h-9 w-[calc(100%-1rem)] mx-2 border-strokeStrong bg-bgBase"
      searchLabel
      [ name_ "facet-search"
      , oninput_ "const q=this.value.toLowerCase();this.closest('[data-component=\"facet-rail\"]').querySelectorAll('[data-component=\"facet-option\"],[data-component=\"facet-section\"]').forEach(el=>el.classList.toggle('hidden',!el.textContent.toLowerCase().includes(q)))"
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


-- | Header of an entity detail drawer: icon and title with inline @badges@, then the
-- identity @chips@ row. Its sections go in a @main_@ that owns their rules
-- (@[&>*+*]:border-t [&>*+*]:pt-4@), so 'detailSection_' and 'pivotLinks_' carry none.
entityHeader_ :: Html () -> Text -> Html () -> Html () -> Html ()
entityHeader_ icon title badges chips = header_ [class_ "border-b border-strokeWeak px-5 py-4 pr-14"] do
  div_ [class_ "flex flex-wrap items-center gap-2"] do
    icon
    h2_ [data_ "drawer-title" "true", class_ "min-w-0 break-words text-lg font-semibold text-textStrong"] $ toHtml title
    badges
  div_ [class_ "mt-2 flex flex-wrap gap-1.5 empty:hidden"] chips


-- | A titled section of an entity detail drawer; @aside@ sits opposite the title.
detailSection_ :: [Attribute] -> Text -> Html () -> Html () -> Html ()
detailSection_ attrs title aside body = section_ ([class_ "space-y-3"] <> attrs) do
  div_ [class_ "flex flex-wrap items-center justify-between gap-2"] $ h3_ [class_ "font-semibold text-textStrong"] (toHtml title) >> aside
  body


-- | The (label, url) links that close an entity detail drawer, pivoting to related pages.
pivotLinks_ :: [(Text, Text)] -> Html ()
pivotLinks_ links = div_ [class_ "flex flex-wrap gap-2"] $ forM_ links \(label, url) -> a_ ([href_ url, class_ "btn btn-sm"] <> navTabAttrs) $ toHtml label


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
-- >>> let h = toStrict $ renderText $ drawer_ "d" False Nothing Nothing ""
-- >>> "drawer-side top-0 left-0 w-full h-full flex justify-end z-10000 overflow-y-scroll overflow-x-hidden" `T.isInfixOf` h
-- True
-- >>> "pointer-events-none max-md:pointer-events-auto" `T.isInfixOf` h
-- True
-- >>> "aria-modal" `T.isInfixOf` h
-- False
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
              if window.matchMedia('(max-width: 767px)').matches then
                add .overflow-hidden to <body/>
                set my._focusTrapCleanup to window.createFocusTrap(my.closest('.drawer').querySelector('.drawer-side > [role=dialog]'))
                set :closeBtn to my.closest('.drawer').querySelector('[data-drawer-close-primary]') or my.closest('.drawer').querySelector('button[aria-label="Close drawer"]')
                if :closeBtn then call :closeBtn.focus() end
              end
            else
              remove .overflow-hidden from <body/>
              if my._focusTrapCleanup then
                call my._focusTrapCleanup()
                set my._focusTrapCleanup to null
              end
            end
          on keydown[key == 'Escape' and my.checked and not (the event's target matches <input, textarea, select, [contenteditable]/>) and no <[popover]:popover-open/> and no <dialog[open]/>] from window
            set my.checked to false
            trigger change on me
          end
      |]
           ]
    )
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block", Aria.label_ "Open drawer"] trigger
  -- The closed panel is translated beyond the viewport. Clip it here rather than letting
  -- its transformed width create a page-level horizontal scroll range.
  -- Desktop drawers are non-modal workbench panels: the page remains interactive so
  -- list rows and charts can load their next selection straight into the open drawer.
  -- Mobile keeps the ordinary modal overlay because the panel occupies the viewport.
  div_ [class_ "drawer-side top-0 left-0 w-full h-full flex justify-end z-10000 overflow-y-scroll overflow-x-hidden pointer-events-none max-md:pointer-events-auto"] do
    label_ [Lucid.for_ drawerId, Aria.label_ "Close drawer", class_ "hidden max-md:flex w-full drawer-overlay grow flex-1"] ""
    div_
      [ id_ $ drawerId <> "-panel"
      , class_ "pointer-events-auto fixed right-0 top-0 h-dvh w-[min(90vw,75rem)] max-sm:w-screen! overflow-y-auto overscroll-contain overflow-x-hidden bg-bgRaised border-s border-strokeWeak shadow-2xl relative [&:has(.widget-editor)>.drawer-floating-close]:hidden"
      , role_ "dialog"
      , Aria.label_ "Details"
      ]
      do
        div_ [class_ "drawer-floating-close sticky top-3 z-30 h-0 flex justify-end"]
          $ button_
            [ type_ "button"
            , Aria.label_ "Close drawer"
            , class_ "btn btn-sm btn-circle btn-ghost mr-3 text-iconNeutral hover:bg-fillWeak hover:text-iconBrand max-sm:size-11"
            , term "_" [text|on click set #${drawerId}.checked to false then trigger change on #${drawerId}|]
            ]
          $ faSprite_ "xmark" "regular" "h-3.5 w-3.5"
        div_
          [ id_ $ drawerId <> "-content"
          , class_ "h-full flex flex-col gap-8 px-8 pb-4 max-lg:px-6 max-md:px-4"
          , term "hx-on::after:swap" "window.labelDrawer(this.closest('[role=dialog]'))"
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
  let useStripe = provider /= Projects.LemonSqueezyProvider
  div_ ([class_ "flex flex-col gap-8 w-full"] <> [hxVals_ "{\"isOnboarding\": true}" | isOnboarding]) do
    unless basicAuthEnabled $ div_ [class_ "flex flex-col gap-2 w-full"] do
      div_ [class_ "flex items-center justify-between w-full gap-4"] do
        label_ [class_ "text-textStrong", Lucid.for_ "price_range"] "Total events"
        p_ [class_ " text-textWeak", term "hx-live:text" "Math.floor(q('#price_range').value / 1e6) + ' Million'"] "20 Million"
      input_ [type_ "range", min_ "20000000", max_ "500000000", step_ "10000000", value_ "20000000", class_ "range range-primary range-sm w-full", id_ "price_range"]
    div_ [class_ "flex flex-col gap-8 mt-6 w-full"] do
      div_ [class_ $ "grid gap-8 w-full " <> bool "grid-cols-1 md:grid-cols-2" "grid-cols-1 md:grid-cols-3" (freePricingEnabled && not basicAuthEnabled)] do
        let isCurrent p = not isOnboarding && currentPlan == p
        if basicAuthEnabled
          then openSourcePricing pid (isCurrent "Open Source") >> enterprisePricing
          else do
            when freePricingEnabled $ freePricing pid (isCurrent "Free")
            popularPricing pid lemonUrl (isCurrent "Bring nothing") freePricingEnabled useStripe
            systemsPricing pid criticalUrl (isCurrent "Bring your own storage") useStripe
    unless useStripe do
      script_ [src_ "https://assets.lemonsqueezy.com/lemon.js"] ("" :: Text)
      script_
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
  div_ [class_ "flex-col justify-start items-start gap-3 flex"] do
    div_ [class_ "text-textStrong text-sm h-6 font-medium italic"] featuresTitle
    forM_ features \feature -> div_ [class_ "flex items-center gap-3"] do
      faSprite_ "feature-check" "regular" "h-4 text-iconBrand shrink-0"
      p_ [class_ "text-sm text-textStrong leading-tight"] $ toHtml feature


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


-- | Plan CTA button. The base classes are written inline here rather than returned by
-- a class-computing helper, so a reader sees the styling on the element and there is one
-- place holding it. @normalCls@ is the per-plan accent, applied only when this is not
-- already the current plan — which is also the only case where the label is the caller's.
pricingButton_ :: Bool -> Text -> [Attribute] -> Text -> Html ()
pricingButton_ isCurrent normalCls attrs label =
  button_ ([class_ $ "btn mb-6 mt-4 h-8 px-3 py-1 w-full text-sm font-semibold rounded-lg " <> bool normalCls "bg-fillDisabled cursor-not-allowed border-0 text-textInverse-strong" isCurrent] <> attrs)
    $ toHtml
    $ bool label "Current plan" isCurrent


-- | Plan CTA: inert when already the current plan, else a Stripe checkout POST or a LemonSqueezy popup.
pricingCta_ :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Html ()
pricingCta_ pid plan normalCls lemonUrl isCurrent useStripe =
  div_ [[__|on click halt|]]
    $ pricingButton_ isCurrent normalCls (type_ "button" : attrs) "Start 30 day free trial"
  where
    attrs :: [Attribute]
    attrs
      | isCurrent = []
      | useStripe = [hxPost_ $ "/p/" <> pid.toText <> "/stripe_checkout", hxVals_ $ "{\"plan\": \"" <> plan <> "\"}", hxSwap_ "none"]
      | otherwise = [term "hx-on:click" $ "window.payLemon(\"" <> plan <> "\", \"" <> lemonUrl <> "\")"]


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
        (div_ [[__|on click halt|]] $ pricingButton_ isCurrent "bg-fillStrong text-textInverse-strong" [term "hx-on:click" "htmx.trigger('#freePricing', 'click')", type_ "button"] "Start free")
        ["10K events per day", "1 user: project owner only", "Opentelemetry Logs, Traces and Metrics", "Last 30 days data retention"]
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
          (priceDisplay_ [term "hx-live:text" "(v => v <= 2e7 ? 29 : 29 + (v - 2e7) / 1e6)(+q('#price_range').value)"] "29" "/per month")
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
        (priceDisplay_ [term "hx-live:text" "(v => v <= 1e8 ? 199 : 199 + (v - 1e8) / 1e6)(+q('#price_range').value)"] "199" "/per month")
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
        (div_ $ pricingButton_ isCurrent "bg-green-700 hover:bg-green-600 text-white" (type_ "submit" : [disabled_ "disabled" | isCurrent]) "Continue with Open Source")
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


navBar :: Html ()
navBar =
  nav_ [id_ "main-navbar", class_ "fixed z-20 top-0 w-full px-4 py-4 bg-bgOverlay flex flex-row justify-between"]
    $ div_ [class_ "flex justify-between items-center gap-4 w-[1000px] mx-auto"]
    $ a_ [href_ "https://monoscope.tech", class_ "flex items-center text-textWeak hover:text-textStrong"]
    $ brandLogo_ "h-12"


-- | Theme-aware wordmark; @h@ is the height utility, e.g. @"h-6"@.
brandLogo_ :: Text -> Html ()
brandLogo_ h = do
  img_ [class_ $ h <> " dark:hidden", src_ "/public/assets/svgs/logo_black.svg", alt_ "Monoscope"]
  img_ [class_ $ h <> " hidden dark:block", src_ "/public/assets/svgs/logo_white.svg", alt_ "Monoscope"]


modalCloseButton_ :: Monad m => Text -> HtmlT m ()
modalCloseButton_ modalId =
  label_
    [ Lucid.for_ modalId
    , Aria.label_ "Close modal"
    , class_ "tap-target !absolute right-3 top-3 z-10 inline-flex h-8 w-8 cursor-pointer items-center justify-center rounded-lg text-iconNeutral hover:bg-fillWeak hover:text-textStrong focus-visible:outline-2 focus-visible:outline-offset-2 active:scale-[0.96] transition-[color,background-color,scale]"
    ]
    $ faSprite_ "xmark" "regular" "h-3.5 w-3.5"


primaryButton_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
primaryButton_ attrs = button_ (class_ " btn btn-primary btn-sm " : attrs)


headerRow_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
headerRow_ attrs = div_ (class_ " flex items-center justify-between " : attrs)


-- | Pointer and keyboard-operable panel separator; behaviour lives in web-components/src/resizer.ts.
-- Persists the width to localStorage and the URL param @key@.
--
-- >>> let html = toStrict $ renderText $ resizer_ "facets-container" "facets_width" True
-- >>> all (`T.isInfixOf` html) ["role=\"separator\"", "tabindex=\"0\"", "aria-orientation=\"vertical\"", "data-resize-target=\"facets-container\"", "data-url-param=\"facets_width\""]
-- True
resizer_ :: Text -> Text -> Bool -> Html ()
resizer_ targetId key = resizerWith_ [data_ "url-param" key] targetId key


-- | 'resizer_' without URL persistence; @attrs@ may add @data-min@/@data-max@ clamps (px or vw).
resizerWith_ :: [Attribute] -> Text -> Text -> Bool -> Html ()
resizerWith_ attrs targetId key increasingDirection =
  div_
    ( [ class_ "group px-r relative shrink-0 h-full flex items-center justify-center cursor-ew-resize overflow-visible select-none touch-none"
      , role_ "separator"
      , Aria.label_ "Resize panel"
      , tabindex_ "0"
      , term "aria-orientation" "vertical"
      , term "aria-valuemin" "0"
      , term "aria-controls" targetId
      , data_ "resize-target" targetId
      , data_ "resize-direction" (if increasingDirection then "increase" else "decrease")
      , data_ "resize-key" key
      ]
        <> attrs
    )
    $ div_ [class_ "h-full border-l hover:border-strokeBrand-strong"]
    $ div_
      [ id_ $ "resizer-" <> key
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
    $ forM_ @[] @_ @(Text, Text) [("h-3/5", "0s"), ("h-2/5", "0.1s"), ("h-4/5", "0.2s"), ("h-1/2", "0.3s"), ("h-3/4", "0.4s"), ("h-2/5", "0.5s")]
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


newtype RefreshingDeferred a = RefreshingDeferred (Deferred a)


instance ToHtml a => ToHtml (RefreshingDeferred a) where
  toHtml (RefreshingDeferred (DeferredBody body)) = toHtml body
  toHtml (RefreshingDeferred (DeferredShell cid url skeleton)) = toHtmlRaw (div_ ([id_ cid, class_ "w-full", data_ "deferred-shell" ""] <> timeRefreshAttrs cid url "load, update-query from:window") skeleton :: Html ())
  toHtmlRaw = toHtml


timeRefreshListener_ :: Text -> Text -> Html ()
timeRefreshListener_ cid url = div_ ([class_ "hidden", term "_" [text|on htmx:after:settle from #${cid} send input to <input[type=text]/> in #${cid}|]] <> timeRefreshAttrs cid url "update-query from:window") mempty


timeRefreshAttrs :: Text -> Text -> Text -> [Attribute]
timeRefreshAttrs cid url trigger = [hxGet_ url, hxTrigger_ trigger, hxTarget_ $ "#" <> cid, hxSelect_ $ "#" <> cid, hxSwap_ "outerMorph", term "hx-sync" "this:replace", term "hx-preload" "false", timeWindowVals_ ""]


-- | Sends the address bar's current time window, so a live refresh follows the picker. With
-- no window in the URL, @since@ falls back to the page's default range.
timeWindowVals_ :: Text -> Attribute
-- Inline rather than window.params(): the shell must resolve before the application script arrives.
timeWindowVals_ defaultSince = term "hx-vals" $ "js:{...(p=>({since:p.since ?? (p.from || p.to ? '' : '" <> defaultSince <> "'),from:p.from || '',to:p.to || ''}))(Object.fromEntries(new URLSearchParams(location.search)))}"


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


-- | A panel that fetches @url@ into itself on @trigger@ and shows a loader (announced as
-- @ariaLabel@) until then. @attrs@ carry the id, classes and htmx extensions.
lazyPanel_ :: Text -> Text -> Text -> [Attribute] -> Html ()
lazyPanel_ url trigger ariaLabel attrs =
  div_ ([hxGet_ url, hxTrigger_ trigger, hxTarget_ "this", hxSwap_ "innerHTML", Aria.busy_ "true"] <> attrs)
    $ div_ [class_ "flex justify-center py-8", role_ "status", Aria.label_ ariaLabel]
    $ loadingIndicator_ LdSM LdDots


-- | Placeholder for a table that has not loaded yet: a header strip and @rows@ row bars.
-- Deliberately not an empty table — a zero state that turns into rows a second later reads
-- as "nothing here" during the moment a user is deciding whether the page is broken.
tableSkeleton_ :: Int -> Html ()
tableSkeleton_ rows = div_ [class_ "flex w-full flex-col gap-3 px-4 pt-2", role_ "status", Aria.label_ "Loading"] do
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
  , hint :: Maybe Text -- help text under the field, linked via aria-describedby
  , inputId :: Maybe Text -- element id when the form renders more than once per page; defaults to the name
  }


instance Default FieldCfg where def = FieldCfg Nothing "text" "" "" [] Nothing Nothing Nothing Nothing


-- | Unified form field: auto-generates input from cfg, or uses custom content when provided
formField_ :: Monad m => FieldSize -> FieldCfg -> Text -> Text -> Bool -> Maybe (HtmlT m ()) -> HtmlT m ()
formField_ size cfg lbl name required customM =
  fieldset_ [class_ wrapperCls] do
    label_ [class_ labelCls, Lucid.for_ fid] do
      whenJust cfg.dot \color -> div_ [class_ $ "w-1.5 h-1.5 rounded-full shrink-0 " <> color] ""
      whenJust cfg.icon \ic -> faSprite_ ic "solid" "w-4 h-4 text-iconNeutral shrink-0"
      toHtml lbl
      when required $ span_ [class_ reqCls] "*"
    fromMaybe
      ( case (cfg.inputType, cfg.suffix) of
          ("textarea", _) -> textarea_ ([class_ textareaCls, name_ name, id_ fid, placeholder_ cfg.placeholder] <> commonAttrs) $ toHtml cfg.value
          (_, Just sfx) -> div_ [class_ "relative"] do
            input_ $ [class_ $ inputCls <> " pr-14"] <> inputAttrs
            span_ [class_ "absolute right-2 top-1/2 -translate-y-1/2 text-xs text-textWeak"] $ toHtml sfx
          _ -> input_ $ [class_ inputCls] <> inputAttrs
      )
      customM
    whenJust cfg.hint $ p_ [id_ $ fid <> "-hint", class_ "text-xs text-textWeak"] . toHtml
  where
    fid = fromMaybe name cfg.inputId
    commonAttrs = [required_ "true" | required] <> [Aria.describedby_ $ fid <> "-hint" | isJust cfg.hint] <> cfg.extraAttrs
    inputAttrs = [value_ cfg.value, name_ name, id_ fid, type_ cfg.inputType, placeholder_ cfg.placeholder] <> commonAttrs
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


-- | @Collapsible open@ is a details disclosure.
data PanelStyle = Raised | Collapsible Bool


data PanelCfg = PanelCfg
  { icon :: Maybe Text
  , subtitle :: Maybe Text
  , sectionId :: Maybe Text -- id of a collapsible panel's details element
  }
  deriving stock (Generic)
  deriving anyclass (Default)


panel_ :: Monad m => PanelStyle -> PanelCfg -> Text -> HtmlT m () -> HtmlT m ()
panel_ style cfg title content = case style of
  Raised -> div_ [class_ "surface-raised rounded-2xl p-4 relative"] do
    span_ [class_ "flex items-center gap-2 text-sm font-semibold text-textStrong mb-3"] do
      whenJust cfg.icon \ic -> faSprite_ ic "regular" "w-4 h-4 text-iconNeutral"
      toHtml title
      whenJust cfg.subtitle $ span_ [class_ "normal-case tracking-normal font-normal"] . toHtml
    content
  Collapsible startOpen ->
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


connectionBadge_ :: Monad m => Bool -> HtmlT m ()
connectionBadge_ connected = span_ [class_ $ "badge badge-sm badge-soft gap-1 " <> bool "badge-secondary" "badge-success" connected] do
  faSprite_ (bool "circle-info" "circle-check" connected) "regular" "h-3 w-3"
  bool "Not connected" "Connected" connected


data BadgeColor = BrandBadge | SuccessBadge | WarningBadge | ErrorBadge | NeutralBadge


-- | A tone's (fill, stroke, text, icon) tokens, shared by every toned surface.
toneCls :: BadgeColor -> (Text, Text, Text, Text)
toneCls = \case
  BrandBadge -> ("bg-fillBrand-weak", "border-strokeBrand-weak", "text-textBrand", "text-iconBrand")
  SuccessBadge -> ("bg-fillSuccess-weak", "border-strokeSuccess-weak", "text-textSuccess", "text-iconSuccess")
  WarningBadge -> ("bg-fillWarning-weak", "border-strokeWarning-weak", "text-textWarning", "text-iconWarning")
  ErrorBadge -> ("bg-fillError-weak", "border-strokeError-weak", "text-textError", "text-iconError")
  NeutralBadge -> ("bg-fillWeak", "border-strokeWeak", "text-textWeak", "text-iconNeutral")


data IconBadgeSize = IconXs | IconSm | IconLg


-- | Toned tile holding one icon, leading a card or dialog heading.
iconBadge_ :: Monad m => IconBadgeSize -> BadgeColor -> Text -> HtmlT m ()
iconBadge_ size tone icon = div_ [class_ $ unwords ["w-fit shrink-0", box, bg]] $ faSprite_ icon "regular" (glyph <> " " <> fg)
  where
    (bg, _, _, fg) = toneCls tone
    (box, glyph) = case size of
      IconXs -> ("p-1.5 rounded-md", "h-3.5 w-3.5")
      IconSm -> ("p-2 rounded-full", "h-4 w-4")
      IconLg -> ("p-3 rounded-full", "h-6 w-6")


-- | A status pill: tone carries the meaning, the label repeats it so colour is never the only signal.
tonePill_ :: BadgeColor -> Text -> Html ()
tonePill_ tone = span_ [class_ $ unwords ["inline-flex w-fit items-center whitespace-nowrap rounded-full border px-2 py-0.5 text-2xs font-medium", bg, txt, border]] . toHtml
  where
    (bg, border, txt, _) = toneCls tone


-- | A label over a big number. @value@ is the number's slot (text, or a widget's live value
-- slot); @cls@ and @attrs@ extend the wrapper (alignment, tooltip).
statFigure_ :: Text -> [Attribute] -> Text -> Html () -> Html ()
statFigure_ cls attrs label value = div_ ([class_ $ "flex flex-col gap-1 leading-none " <> cls] <> attrs) do
  fieldLabel_ "" label
  span_ [class_ "text-2xl font-semibold text-textStrong tabular-nums leading-none"] value


-- | Small uppercase muted label for a field, figure or sub-block. Not a heading: card and
-- section titles use 'sectionLabel_'.
fieldLabel_ :: Text -> Text -> Html ()
fieldLabel_ cls = span_ [class_ $ "block text-2xs font-semibold text-textWeak uppercase tracking-wide " <> cls] . toHtml


-- | A label/value list: muted terms in an auto-width column beside their values.
kvList_ :: Text -> [(Text, Html ())] -> Html ()
kvList_ cls rows = dl_ [class_ $ "grid grid-cols-[auto_minmax(0,1fr)] gap-x-4 gap-y-1.5 " <> cls] $ forM_ rows \(k, v) -> do
  dt_ [class_ "text-textWeak"] $ toHtml k
  dd_ [class_ "min-w-0 truncate text-textStrong"] v


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
      , -- Escape is heard on window: focus sits inside the modal, which is not this input's subtree.
        [__|on keyup[key is 'Escape'] from window or closeModal from body if my.checked set my.checked to false then send change to me end
          on change if my.checked then
            add .overflow-hidden to <body/>
            wait 50ms then
            set :modal to the next <.modal/> then
            set :focusable to :modal.querySelector('[autofocus]') or :modal.querySelector('input:not([type=hidden]):not([type=checkbox]), textarea, select, [tabindex]:not([tabindex="-1"])') then
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
    , [__|on keydown[key=='Enter' and target.tagName=='INPUT' and target.type!='textarea']
        set :form to target.closest('form') then
        if :form then halt the event then call :form.requestSubmit() end
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


-- | Red ghost trigger plus the confirmation modal it opens, e.g. a Remove/Disconnect action.
dangerAction_ :: Text -> Text -> Text -> Text -> Text -> [Attribute] -> Text -> Html ()
dangerAction_ modalId icon label title description confirmAttrs confirmText = do
  label_ [class_ "btn btn-sm btn-ghost gap-1 text-textError hover:bg-fillError-weak", Lucid.for_ modalId] do
    faSprite_ icon "regular" "w-3 h-3"
    span_ $ toHtml label
  modalWith_ modalId def{boxClass = "p-6", hideClose = True} Nothing do
    div_ [class_ "flex items-start gap-3 mb-4"] do
      iconBadge_ IconSm ErrorBadge "triangle-alert"
      div_ do
        h3_ [class_ "text-lg font-semibold text-textStrong"] $ toHtml title
        p_ [class_ "text-sm text-textWeak mt-1"] $ toHtml description
    div_ [class_ "flex justify-end gap-2 mt-6"] do
      label_ [class_ "btn btn-sm btn-ghost", Lucid.for_ modalId] "Cancel"
      button_ ([type_ "button", class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90"] <> confirmAttrs) $ toHtml confirmText


-- | Native @<dialog>@ prompt floating over a blurred backdrop: white title strip, a card holding
-- @body@ above Cancel/@submit@, and a keyboard-hint row. Open with @showModal()@; the title id is
-- @dialogId-title@. Children may style on @group-has-[…]/dialog@.
floatingDialog_ :: Text -> Html () -> (Text, [Text]) -> [Attribute] -> Html () -> Html () -> Html ()
floatingDialog_ dialogId title submitHint formAttrs body submit =
  with (makeElement "dialog") [id_ dialogId, class_ "modal !items-start px-4 pt-[15vh] backdrop:backdrop-blur-sm", term "aria-labelledby" (dialogId <> "-title")] do
    form_ [method_ "dialog", class_ "modal-backdrop"] $ button_ [Aria.label_ "Close"] "close"
    form_ (formAttrs <> [class_ "modal-box group/dialog !w-full !max-w-lg !overflow-visible !rounded-none !border-0 !bg-transparent !p-0 !shadow-none flex flex-col"]) do
      div_ [class_ "mb-2 flex items-center justify-between px-3"] do
        h2_ [id_ $ dialogId <> "-title", class_ "text-xs font-medium uppercase tracking-wider text-white"] title
        button_ [type_ "button", Aria.label_ "Close", class_ "inline-flex h-8 w-8 cursor-pointer items-center justify-center rounded-lg text-white/70 hover:bg-white/10 hover:text-white focus-visible:outline-2 focus-visible:outline-white active:scale-[0.96] transition-[color,background-color,scale]", onclick_ "this.closest('dialog').close()"]
          $ faSprite_ "xmark" "regular" "h-3.5 w-3.5"
      div_ [class_ "w-full overflow-hidden rounded-lg border border-base-300 bg-bgBase shadow-2xl"] do
        body
        div_ [class_ "flex justify-end gap-1.5 p-2"] do
          button_ [type_ "button", class_ "btn btn-sm btn-ghost cursor-pointer shadow-none active:scale-[0.96] transition-transform", onclick_ "this.closest('dialog').close()"] "Cancel"
          button_ [type_ "submit", class_ "btn btn-sm btn-primary cursor-pointer gap-1.5 shadow-none active:scale-[0.96] transition-transform"] submit
      kbdHints_ [submitHint, ("Close", ["esc"])]


-- | Single labelled text input inside a 'floatingDialog_' card.
dialogField_ :: Text -> Text -> Text -> Text -> [Attribute] -> Html ()
dialogField_ fieldId name label placeholder extra =
  div_ [class_ "border-b border-base-300 px-4 py-3"] $ label_ [Lucid.for_ fieldId, class_ "flex flex-col gap-1.5 text-xs font-medium text-textStrong"] do
    span_ $ toHtml label
    input_ $ [id_ fieldId, name_ name, autofocus_, autocomplete_ "off", placeholder_ placeholder, class_ "no-focus-ring w-full bg-transparent py-2 text-sm font-normal outline-none placeholder:text-textDisabled"] <> extra


-- | White hint row under dark-backdrop prompts: @(label, keys)@ pairs.
kbdHints_ :: [(Text, [Text])] -> Html ()
kbdHints_ hints = div_ [class_ "cmd-palette-hints mt-3 flex items-center justify-center gap-6 text-xs text-white drop-shadow dark:text-white/80"]
  $ forM_ hints \(label, keys) -> div_ [class_ "flex items-center gap-1.5"] $ toHtml label >> traverse_ (kbd_ [class_ "kbd kbd-xs"] . toHtml) keys


-- | Segmented two-or-more-way radio switch; the first option starts checked. Input ids are
-- @idPrefix <> value@ so callers can style siblings on @:checked@.
segmented_ :: Text -> Text -> Text -> Text -> [(Text, Text)] -> Html ()
segmented_ cls legend grp idPrefix opts = fieldset_ [class_ cls] do
  legend_ [class_ "sr-only"] $ toHtml legend
  forM_ (zip (True : repeat False) opts) \(active, (val, lbl)) ->
    label_ [class_ "cursor-pointer rounded-md px-3 py-1 text-center font-medium text-textWeak has-[:checked]:bg-bgRaised has-[:checked]:text-textStrong has-[:checked]:shadow-xs has-[:focus-visible]:outline-2 has-[:focus-visible]:outline-offset-2 transition-[color,background-color,box-shadow] duration-100"] do
      input_ $ [type_ "radio", name_ grp, id_ (idPrefix <> val), class_ "sr-only"] <> [checked_ | active] <> [value_ val]
      toHtml lbl


-- | One quiet row action. @attrs@ carries whatever the destination needs — the
-- HTMX nav triple for an in-app tab swap, nothing for a link that must do a
-- full page load.
data RowAction = RowAction
  { icon :: Text
  , label :: Text
  , href :: Text
  , attrs :: [Attribute]
  }


-- | The trailing action cluster on a table row.
--
-- Two rules make this readable in a long table. It is neutral at rest, because
-- brand blue belongs to the row's primary link — twenty rows of blue secondary
-- actions out-shout the thing they sit next to. And the caller right-aligns it
-- (@justify-between@ against the row's name), so the actions land in one fixed
-- vertical lane instead of ragging off the end of names of different lengths.
--
-- Labels drop below @lg@ where the row gets tight; the accessible name is on
-- the anchor, so the icon-only state is still announced and still tooltipped.
rowActions_ :: [RowAction] -> Html ()
rowActions_ acts = div_ [class_ "flex items-center gap-0.5 shrink-0 max-md:gap-1"] $ forM_ acts \a ->
  a_
    ( [ href_ a.href
      , -- Below md the label drops and the control is icon-only, so it grows a
        -- real tap target instead of shrinking to the glyph.
        class_ "inline-flex items-center justify-center gap-1.5 rounded-sm px-1.5 py-1 text-xs text-textWeak transition-colors max-md:min-h-10 max-md:min-w-10 hover:bg-fillWeak hover:text-textBrand focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-strokeBrand-strong"
      , Aria.label_ a.label
      , term "data-tippy-content" a.label
      ]
        <> a.attrs
    )
    do
      faSprite_ a.icon "regular" "h-3.5 w-3.5 shrink-0"
      span_ [class_ "max-md:hidden"] $ toHtml a.label


-- | Quiet icon-only row action; @attrs@ carries the request (or a 'popoverTrigger_'), the response swaps nothing.
inlineIconBtn_ :: Text -> Text -> [Attribute] -> Html ()
inlineIconBtn_ tip icon attrs =
  button_ ([type_ "button", term "data-tippy-content" tip, Aria.label_ tip, class_ "cursor-pointer hover:text-textBrand transition-colors tap-target", hxSwap_ "none"] <> attrs)
    $ faSprite_ icon "regular" "h-3.5 w-3.5"


-- | Bell-slash chip for a silenced monitor or issue: @short@ labels the chip, @long@ the tooltip.
silenceBadge_ :: Text -> Text -> UTCTime -> UTCTime -> Html ()
silenceBadge_ short long now until' = span_ [class_ "badge badge-sm badge-ghost gap-1 shrink-0", term "data-tippy-content" $ untilLabel long now until' <> " \x2014 notifications are paused"] do
  faSprite_ "bell-slash" "regular" "h-3 w-3"
  toHtml $ untilLabel short now until'


-- | Copy-to-clipboard button. @src@ is the hyperscript expression naming what to copy — an
-- element's text (@#api-key's innerText@) or one of the button's own attributes
-- (@my \@data-url@, paired with that attribute in @attrs@). The label reverts after two
-- seconds so the control stops claiming a copy that happened minutes ago.
copyButton_ :: Text -> Text -> Text -> [Attribute] -> Html ()
copyButton_ cls iconCls src attrs =
  button_
    ( [ type_ "button"
      , class_ cls
      , term "_" [text|on click call navigator.clipboard.writeText($src) then put 'Copied!' into the first <span/> in me then wait 2s then put 'Copy' into the first <span/> in me|]
      ]
        <> attrs
    )
    do
      faSprite_ "copy" "regular" iconCls
      span_ "Copy"


-- | Table cell for an event count, coloured by volume; zero reads as a dash.
eventsCountCell_ :: Int -> Html ()
eventsCountCell_ n = span_ [class_ $ "tabular-nums font-medium text-sm " <> color] $ toHtml $ bool (formatWithCommas (fromIntegral n)) "\x2014" (n == 0)
  where
    color
      | n >= 100 = "text-fillError-strong"
      | n >= 10 = "text-fillWarning-strong"
      | n > 0 = "text-textStrong"
      | otherwise = "text-textWeak font-normal"


-- | Table cell for a relative "last seen", exact time on hover. @nowrap@: the column is
-- narrow enough that "50 secs ago" would wrap and rag the row heights.
lastSeenCell_ :: UTCTime -> Maybe UTCTime -> Html ()
lastSeenCell_ now = \case
  Just t -> span_ [class_ "text-xs text-textWeak whitespace-nowrap", term "data-tippy-content" $ formatUTC t] $ toHtml $ agoText now t
  Nothing -> span_ [class_ "text-textWeak text-xs"] "-"


-- | A copy-pasteable curl for a request: one flag per line, every value shell-quoted.
--
-- >>> putTextLn $ curlCommand "POST" "https://api.x/a?b=1" [("Accept", "*/*")] (Just "{\"n\":\"it's\"}")
-- curl -X POST 'https://api.x/a?b=1' \
--   -H 'Accept: */*' \
--   -d '{"n":"it'\''s"}'
curlCommand :: Text -> Text -> [(Text, Text)] -> Maybe Text -> Text
curlCommand method url headers body = T.intercalate " \\\n  " $ ("curl -X " <> method <> " " <> q url) : ["-H " <> q (k <> ": " <> v) | (k, v) <- headers] <> ["-d " <> q b | b <- maybeToList body]
  where
    q t = "'" <> T.replace "'" "'\\''" t <> "'"


-- | Type-to-filter attribute for a text input: hides each element matched by @sel@
-- (a CSS selector, e.g. @"#list .row"@) whose @data-filter@ — or, absent that,
-- textContent — lacks the input's value. It reruns on input and after every swap, so
-- rows swapped in later (time presets, pagination) stay filtered; an hx-live binding
-- would instead re-scan every row on any DOM mutation, which stalls large span trees.
-- @sel@ is spliced into hyperscript source, so it must be a constant or built from
-- server-generated ids, never from user input. Pair with @type_ "search"@ so Escape
-- clears the box and refilters natively.
filterInputAttr_ :: Text -> Attribute
filterInputAttr_ sel = term "_" [text|on input or htmx:after:swap from document set v to my value.toLowerCase() then repeat for el in <${sel}/> if (el.dataset.filter or el.textContent).toLowerCase().includes(v) remove .hidden from el else add .hidden to el end end|]


-- | Toned icon + label chip.
colorChip_ :: Monad m => BadgeColor -> Text -> Text -> HtmlT m ()
colorChip_ tone icon label = span_ [class_ $ unwords ["inline-flex items-center gap-1.5 text-xs rounded-full px-2.5 py-1", bg, txt]] do
  faSprite_ icon "regular" "h-3 w-3"
  toHtml label
  where
    (bg, _, txt, _) = toneCls tone


metadataChip_ :: Monad m => Text -> Text -> HtmlT m ()
metadataChip_ = colorChip_ NeutralBadge


-- | Standard settings page layout: scrollable container with padded, max-width section
settingsSection_ :: Monad m => HtmlT m () -> HtmlT m ()
settingsSection_ body = div_ [class_ "w-full h-full overflow-y-auto"] do
  section_ [class_ "py-6 px-4 sm:py-8 sm:px-8 lg:px-12 max-w-4xl space-y-6"] body


settingsH2_ :: Monad m => Text -> HtmlT m ()
settingsH2_ = h2_ [class_ "text-textStrong text-xl sm:text-2xl font-bold tracking-tight"] . toHtml


-- | Uppercase section label used in settings subsections
sectionLabel_ :: Monad m => Text -> HtmlT m ()
sectionLabel_ = h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wider"] . toHtml


-- | Toned notice box: tone icon, optional title, then the body (muted under a title).
callout_ :: Monad m => BadgeColor -> Maybe Text -> HtmlT m () -> HtmlT m ()
callout_ tone titleM body = div_ [class_ $ unwords ["flex items-start gap-2.5 rounded-lg border p-3 text-sm", bg, border]] do
  faSprite_ icon "regular" $ "h-4 w-4 shrink-0 mt-0.5 " <> fg
  div_ [class_ "min-w-0 flex-1"] $ case titleM of
    Nothing -> div_ [class_ "text-textStrong"] body
    Just t -> p_ [class_ "font-medium text-textStrong"] (toHtml t) >> div_ [class_ "mt-0.5 text-textWeak"] body
  where
    (bg, border, _, fg) = toneCls tone
    icon = case tone of
      SuccessBadge -> "circle-check"
      WarningBadge -> "triangle-exclamation"
      ErrorBadge -> "circle-exclamation"
      BrandBadge -> "circle-info"
      NeutralBadge -> "circle-info"


-- | Read-only monospace value with a copy button; @elId@ names the value element.
copyField_ :: Text -> Text -> Html ()
copyField_ elId val = div_ [class_ "flex items-center gap-2"] do
  code_ [id_ elId, class_ "min-w-0 flex-1 overflow-x-auto whitespace-nowrap rounded-lg border border-strokeWeak bg-bgBase px-3 py-2 font-mono text-xs text-textStrong"] $ toHtml val
  copyButton_ "btn btn-sm gap-1 shrink-0" "h-3.5 w-3.5" ("#" <> elId <> "'s innerText") []


-- | Title and description beside a toggle switch; @attrs@ carry the input's name, checked state.
toggleRow_ :: Text -> Text -> Text -> [Attribute] -> Html ()
toggleRow_ inputId lbl desc attrs = div_ [class_ "flex items-center justify-between gap-3"] do
  label_ [Lucid.for_ inputId, class_ "min-w-0 cursor-pointer"] do
    span_ [class_ "block text-sm font-medium text-textStrong"] $ toHtml lbl
    span_ [class_ "block mt-0.5 text-xs text-textWeak"] $ toHtml desc
  input_ $ [type_ "checkbox", id_ inputId, class_ "toggle toggle-sm toggle-primary shrink-0"] <> attrs


-- | Project role @\<option\>@s; values are the 'Display' of @ProjectMembers.Permissions@.
roleOptions_ :: Monad m => Maybe Text -> HtmlT m ()
roleOptions_ sel = options_ sel [("admin", "Admin"), ("edit", "Editor"), ("view", "Viewer")]


-- | \"Repository missing?\" escape hatch under GitHub-App repo pickers: the list is
-- exactly what the installation was granted, so widening it happens on GitHub.
installationSettingsLink_ :: Monad m => Text -> HtmlT m ()
installationSettingsLink_ url = externalLink_ [href_ url, target_ "_blank", rel_ "noopener"] "Repository missing? Add it to the installation on GitHub"


-- | Brand link with a trailing ↗, for a jump to another view (Explorer, a monitor) or site.
-- @attrs@ must not carry @class_@: Lucid would fuse it into this one.
externalLink_ :: Monad m => [Attribute] -> HtmlT m () -> HtmlT m ()
externalLink_ attrs label = a_ ([class_ "text-xs text-textBrand hover:underline inline-flex items-center gap-1 w-fit"] <> attrs) do
  label
  faSprite_ "arrow-up-right-from-square" "regular" "w-3 h-3 shrink-0"


-- | A server-open @\<details\>@ that starts collapsed below @px@ viewport width.
-- Seeds once; after that the reader's own toggling owns the state.
detailsClosedBelowAttr_ :: Int -> Attribute
detailsClosedBelowAttr_ px = term "hx-live" $ "if (!this.dataset.seeded) { this.dataset.seeded = '1'; if (window.innerWidth < " <> show px <> ") this.open = false }"


-- | Enter/Space activates the element's click behavior, for label/row controls that
-- cannot be a native @\<button\>@.
keyboardActivateAttr_ :: Attribute
keyboardActivateAttr_ = [__|on keydown[key=='Enter' or key==' '] halt the event then call me.click() end|]


-- | Combobox keyboard nav for a wrapper holding the search input and its @[role=option]@ items
-- (hidden ones skipped): typing highlights the first visible option, arrows move @.active@,
-- Enter clicks it. Filtering itself belongs on the options, e.g. @hx-live:hidden@.
listboxNavAttr_ :: Attribute
listboxNavAttr_ =
  [__|on input call htmx.live.refresh() then wait 0ms
        remove .active from <[role=option]/> in me
        set f to the first <[role=option]:not([hidden])/> in me
        if f add .active to f end
      end
      on keydown[key=='ArrowDown' or key=='ArrowUp'] halt the event
        set opts to <[role=option]:not([hidden])/> in me
        set d to -1
        if event.key is 'ArrowDown' set d to 1 end
        set n to opts[opts.indexOf(the first <[role=option].active:not([hidden])/> in me) + d]
        if n remove .active from <[role=option]/> in me then add .active to n then call n.scrollIntoView({block:'nearest'}) end
      end
      on keydown[key=='Enter'] set a to the first <[role=option].active:not([hidden])/> in me
        if a halt the event then call a.click() end
      end|]


-- | Click-to-copy for the element named by the JS expression @el@: copies its innerText,
-- flashes @.copy-success@ on the trigger and raises the success toast.
copySourceAttr_ :: Text -> Attribute
copySourceAttr_ el = copyToClipboardAttr_ ("(" <> el <> ").innerText") "Value copied to the Clipboard"


-- | Clear a form once its request succeeds.
resetFormOnSuccessAttr_ :: Attribute
resetFormOnSuccessAttr_ = term "hx-on:htmx:after:request" "if (event.detail.ctx.response.status < 400) this.reset()"


-- | Save button that activates when its parent form changes.
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


-- | The compact form of "how long ago was this" ("23 hrs ago"), relative to the render clock.
agoText :: UTCTime -> UTCTime -> Text
agoText now = unwords . map abbreviateUnit . words . toText . prettyTimeAuto now


-- | Popover offering silence durations plus an indefinite option — the shared
-- vocabulary behind monitor mute and issue acknowledge, so "for how long?" is
-- answered the same way everywhere. @req@ turns the chosen duration in minutes
-- (empty for indefinite) into the htmx attributes that submit it, leaving each
-- caller its own verb and swap semantics.
durationMenu_ :: Text -> Text -> [(Text, Text)] -> (Text -> [Attribute]) -> (Text -> Html ()) -> Html ()
durationMenu_ popId heading extras req trigger = div_ [class_ "inline-block"] do
  trigger popId
  -- The options read as bare durations ("4 hours") once focus lands inside, so
  -- the heading has to be the group's accessible name, not just visible text.
  div_ (popoverPanel_ popId <> [role_ "group", Aria.label_ heading, class_ "dropdown dropdown-start menu bg-bgRaised p-1 text-sm border border-strokeWeak z-50 min-w-36 rounded-md shadow-lg mt-1"]) do
    span_ [class_ "px-3 py-1 text-xs font-medium text-textWeak", Aria.hidden_ "true"] $ toHtml heading
    forM_ @[] @_ @(Int, Text) [(60, "1 hour"), (240, "4 hours"), (480, "8 hours"), (1440, "1 day"), (4320, "3 days"), (10080, "1 week")] \(mins, label) ->
      button_ ([type_ "button", class_ "px-3 py-1.5 text-sm text-left hover:bg-fillWeaker rounded cursor-pointer w-full"] <> req (show mins)) $ toHtml label
    forM_ (extras <> [("Indefinitely", "")]) \(label, q) -> button_ ([type_ "button", class_ "px-3 py-1.5 text-sm text-left hover:bg-fillWeaker rounded cursor-pointer w-full border-t border-strokeWeak"] <> req q) $ toHtml label


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
  | null buckets || all (== 0) buckets = span_ [class_ "text-textWeak text-xs"] "\x2014"
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
          peakIdx = fromMaybe 0 $ elemIndex peakVal buckets
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


-- | A frame normalised across our two parsers: 'StackTrace.Frame' (Explorer) and
-- @EF.StackFrame@ (Issues, whose issue hash derives from that parse).
data TraceFrame = TraceFrame {raw :: Text, function :: Maybe Text, file :: Maybe Text, line :: Maybe Int, context :: Maybe Text, inApp :: Bool}


-- | Explorer's stack trace: frames from the printed stack, else the span's @code.*@ attributes.
-- @resAttr@ reads the resource, where the deploy's revision lives, so a snippet shows the
-- source that threw rather than the source as it is now.
stackTrace_ :: Projects.ProjectId -> Maybe Text -> (Text -> Maybe Text) -> (Text -> Maybe Text) -> Text -> Html ()
stackTrace_ pid svcM attr resAttr stack =
  stackFrames_ (codeContextUrl pid svcM (StackTrace.revisionFor resAttr)) stack [TraceFrame f.raw f.function f.file f.line Nothing (StackTrace.isInApp f) | f <- StackTrace.framesFor attr stack]


codeContextUrl :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Text -> Int -> Text
codeContextUrl pid svcM revM path n = "/p/" <> pid.toText <> "/code_context?file=" <> toUriStr path <> "&line=" <> show n <> foldMap (("&service=" <>) . toUriStr) svcM <> foldMap (("&revision=" <>) . toUriStr) revM


-- | A stack trace as frames rather than a blob, the way Sentry renders one. Each frame is a
-- native @\<details\>@; its source snippet loads on @intersect once@ inside it, so only when
-- opened — resolving it is an SCM call the panel must not block on. The first in-app frame
-- starts open; runtime frames keep their call-order position but fold behind a count; the
-- raw text stays one disclosure away, and is all we show when nothing parsed.
stackFrames_ :: (Text -> Int -> Text) -> Text -> [TraceFrame] -> Html ()
stackFrames_ snippetUrl raw frames
  | null frames = rawBlock
  | otherwise = div_ [class_ "group/frames"] do
      div_ [class_ "max-h-96 overflow-y-auto"] $ forM_ (zip [0 :: Int ..] frames) \(i, f) -> frame_ (Just i == firstInApp) f
      when (folded > 0)
        $ label_ [class_ "flex items-center gap-1.5 px-3 py-2 text-xs text-textBrand cursor-pointer border-t border-strokeWeak hover:bg-fillWeaker transition-colors"] do
          input_ [type_ "checkbox", class_ "sys-frames sr-only"]
          faSprite_ "chevron-down" "regular" "w-3 h-3 shrink-0 group-has-[.sys-frames:checked]/frames:rotate-180 transition-transform"
          span_ [class_ "group-has-[.sys-frames:checked]/frames:hidden"] $ toHtml $ "Show " <> countNoun folded "runtime frame"
          span_ [class_ "hidden group-has-[.sys-frames:checked]/frames:inline"] "Hide runtime frames"
      unless (T.null $ T.strip raw) $ div_ [class_ "border-t border-strokeWeak"] $ disclosure_ [] "px-3 py-2 text-xs text-textWeak hover:text-textStrong" "Raw" rawBlock
  where
    firstInApp = findIndex (.inApp) frames
    -- Only once something is in-app: folding every frame would leave an empty list.
    folds f = isJust firstInApp && not f.inApp && isJust f.file
    folded = length $ filter folds frames
    rawBlock = pre_ [class_ "max-h-80 overflow-auto text-xs font-mono leading-relaxed whitespace-pre-wrap break-words px-3 py-2"] $ toHtml raw
    frame_ :: Bool -> TraceFrame -> Html ()
    frame_ open f = div_ [class_ $ "border-b border-strokeWeak last:border-0 " <> bool "" "hidden group-has-[.sys-frames:checked]/frames:block bg-fillWeaker" (folds f)] do
      case f.file of
        -- An unplaced line keeps its text and gets no disclosure: there is nothing behind it.
        Nothing -> div_ [class_ "px-3 py-1 font-mono text-2xs text-textWeak whitespace-pre-wrap break-words"] $ toHtml f.raw
        Just path -> disclosure_ [open_ "" | open] "px-3 py-1.5 hover:bg-fillWeaker text-xs" (summary path) case f.line of
          Nothing -> div_ [class_ "px-3 pb-2 pl-8 text-2xs text-textWeak italic"] "No source linked for this file."
          Just n -> div_ [class_ "px-3 pb-2", hxGet_ (snippetUrl path n), hxTrigger_ "intersect once", hxTarget_ "this", hxSwap_ "innerHTML"] $ div_ [class_ "pl-5 py-1 text-2xs text-textWeak"] "Loading source…"
      whenJust f.context $ pre_ [class_ "mx-3 mb-1.5 text-xs font-mono text-textStrong bg-fillWeaker rounded px-2 py-1 overflow-x-auto"] . toHtml
      where
        summary path = do
          span_ [class_ $ "font-mono truncate " <> bool "text-textWeak" "text-textStrong font-medium" f.inApp] $ toHtml $ path <> foldMap ((":" <>) . show) f.line
          whenJust f.function \fn -> span_ [class_ "text-textWeak truncate max-md:hidden"] $ toHtml ("in " <> fn)
          -- In-app is a path heuristic, so it is labelled rather than encoded only in weight.
          when f.inApp $ span_ [class_ "ml-auto shrink-0 rounded-sm border border-strokeWeak px-1 text-2xs text-textWeak"] "in app"


-- | Native disclosure with a chevron that turns when open. @attrs@ go on the @\<details\>@
-- (e.g. @open_@); @summaryCls@ adds to the summary row's flex layout. The rotate keys off the
-- chevron's own summary, so disclosures nest without the outer one turning inner chevrons.
disclosure_ :: [Attribute] -> Text -> Html () -> Html () -> Html ()
disclosure_ attrs summaryCls title body = details_ attrs do
  summary_ [class_ $ "flex items-center gap-1.5 cursor-pointer select-none list-none [&::-webkit-details-marker]:hidden " <> summaryCls] do
    faSprite_ "chevron-right" "regular" "w-3 h-3 shrink-0 text-iconNeutral transition-transform [details[open]>summary>&]:rotate-90"
    title
  body
