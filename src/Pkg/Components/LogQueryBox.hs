module Pkg.Components.LogQueryBox (logQueryBox_, visTypes, queryLibraryContent_, queryEditorInitializationCode, enrichSchemaWithFacets, LogQueryBoxConfig (..), visualizationTabs_) where

import Data.Aeson qualified as AE
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.LogPatterns (knownPatternFields)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import NeatInterpolation (text)
import Pages.Components (modal_)
import Pkg.SchemaLearning.Catalog (FacetData (..), FacetValue (..))
import Relude
import Utils (displayTimestamp, faSprite_, formatUTC, onpointerdown_, popoverPanel_, popoverTrigger_)


-- | Configuration record for the log query box component
data LogQueryBoxConfig = LogQueryBoxConfig
  { pid :: Projects.ProjectId
  , currentRange :: Maybe (Text, Text)
  , source :: Maybe Text
  , targetSpan :: Maybe Text
  , query :: Maybe Text
  , vizType :: Maybe Text
  , updateUrl :: Bool
  -- ^ Whether to update the URL when the query changes
  , alert :: Bool
  , patternSelected :: Maybe Text
  , targetWidgetPreview :: Maybe Text
  -- ^ ID of the widget preview element to update when the query changes
  , mobileExtra :: Maybe (Html ())
  -- ^ Extra content rendered mobile-only in the viz tabs row
  , parseError :: Maybe Text
  -- ^ Server-side parse error to display inline on page load
  }
  deriving (Generic, Show)
  deriving anyclass (Default)


-- | Reusable log query box component that can be used in both Logs and Dashboards pages
-- This component provides a unified interface for querying logs and visualizing data
logQueryBox_ :: LogQueryBoxConfig -> Html ()
logQueryBox_ config = do
  let noActiveQuery = all T.null config.query
  modal_ "saveQueryMdl" "" $ form_
    [ class_ "flex flex-col p-3 gap-3"
    , id_ "saveQueryForm"
    , hxPost_ $ "/p/" <> config.pid.toText <> "/log_explorer/queries"
    , hxVals_ "js:{query: document.getElementById('saveQueryMdl').dataset.pendingQuery || window.getQueryFromEditor()}"
    , hxTarget_ "#queryLibraryPopover"
    , hxSwap_ "innerHTML"
    , hxPushUrl_ "false"
    , [__|on htmx:after:request set #saveQueryMdl.dataset.pendingQuery to null|]
    ]
    do
      strong_ "Name your query"
      input_ [type_ "hidden", value_ "", name_ "queryLibId", id_ "queryLibId"]
      input_ [class_ "input input-md", placeholder_ "query title", name_ "queryTitle"]
      button_ [type_ "submit", class_ "btn btn-primary cursor-pointer"] "Save"
  form_
    [ id_ "log_explorer_form"
    , class_ "flex flex-col gap-1 w-full max-w-full"
    , [__| on keydown if event.key is 'Enter' halt |]
    ]
    do
      div_ [class_ "flex flex-col gap-2 items-stretch justify-center group/fltr"] do
        -- `data-query-state` is the single source of the error look: the border
        -- and the message row below both derive from it in CSS, so JS sets one
        -- attribute instead of toggling classes on three elements across two
        -- files (which had drifted to two different border colours).
        div_ [class_ "group/qbox px-1 py-0.5 flex-1 flex flex-col gap-1 bg-fillWeaker rounded-lg border border-strokeWeak data-[query-state=error]:border-strokeError-strong group-has-[.ai-search:checked]/fltr:border-2 group-has-[.ai-search:checked]/fltr:border-iconBrand group-has-[.ai-search:checked]/fltr:shadow-xs shadow-strokeBrand-weak has-[.ai-search:focus-visible]:ring-2 has-[.ai-search:focus-visible]:ring-strokeBrand-strong", id_ "queryBox", data_ "query-state" (bool "ok" "error" (isJust config.parseError))] do
          input_
            $ [ class_ "sr-only ai-search"
              , type_ "checkbox"
              , id_ "ai-search-chkbox"
              , Aria.label_ "Ask in plain English with AI search"
              , -- Every path that opens or collapses AI search routes through `change`, so
                -- the preference below is written once here instead of at four call sites.
                [__|on change
                    call localStorage.setItem('aiSearchExpanded', my.checked)
                    if me.checked then call #ai-search-input.focus() end
                  on keydown[key=='Space' and shiftKey] from document set my.checked to true then send change to me
                  on keydown[key=='?' and not ctrlKey and not metaKey and not altKey] from document
                    if event.target.tagName is not 'INPUT' and event.target.tagName is not 'TEXTAREA' and event.target.contentEditable is not 'true'
                      set my.checked to true
                      set #ai-search-input.value to ''
                      send change to me
                      halt
                    end
                  |]
              ]
            <> [checked_ | isJust config.targetWidgetPreview || noActiveQuery]
          -- Whether AI search or the KQL bar leads is a per-user preference, so it lives in
          -- localStorage and overrides the `noActiveQuery` default above. Applied by a
          -- synchronous script rather than a hyperscript `init` so the AI panel never flashes
          -- open before _hyperscript loads. Widget preview always forces AI, so it opts out.
          whenNothing_ config.targetWidgetPreview
            $ script_ "{const p=localStorage.getItem('aiSearchExpanded'),c=document.getElementById('ai-search-chkbox');if(p!==null&&c)c.checked=p==='true';}"
          div_ [class_ "w-full gap-2 items-center px-2 hidden group-has-[.ai-search:checked]/fltr:flex"] do
            span_ [class_ "text-2xs font-semibold text-textBrand bg-fillBrand-weak px-1.5 py-0.5 rounded shrink-0"] "AI"
            input_
              [ class_ "border-0 w-full flex-1 p-1 no-focus-ring peer"
              , placeholder_ "Ask in plain English — e.g. \"errors in payment service last hour\""
              , id_ "ai-search-input"
              , required_ "required"
              , name_ "input"
              , hxPost_ $ "/p/" <> config.pid.toText <> "/log_explorer/ai_search"
              , -- `htmx:trigger` is not special-cased by htmx; it only fires a request because
                -- it is listed here. Enter and the Submit button both dispatch it to skip the debounce.
                hxTrigger_ "input[this.value.trim().length > 0] changed delay:1s, htmx:trigger"
              , hxSwap_ "none"
              , hxExt_ "json-enc"
              , hxVals_ "js:{timezone: Intl.DateTimeFormat().resolvedOptions().timeZone}"
              , term "hx-validate" "false"
              , hxIndicator_ "#ai-search-loader"
              , data_ "container-id" (fromMaybe "visualization-widget-container" config.targetWidgetPreview)
              , -- The response fans out to three JS subsystems (time picker, query editor,
                -- viz tabs), so the routing lives in one named function beside them rather
                -- than as a branch tree here — see window.applyAiSearchResult.
                [__|on keydown[key=='Escape'] set #ai-search-chkbox.checked to false then send change to #ai-search-chkbox
                   on keydown[key=='Enter']
                     if my.value.trim().length > 0
                       then halt then trigger htmx:trigger
                     end
                   on htmx:after:request call window.applyAiSearchResult(event, me)|]
              ]
            span_ [class_ "htmx-indicator", id_ "ai-search-loader"] $ faSprite_ "spinner" "regular" "w-4 h-4 animate-spin"
            a_
              [ class_ "px-3 py-0.5 inline-flex gap-2 items-center cursor-pointer border text-textDisabled shadow-strokeBrand-weak hover:border-strokeBrand-weak rounded-sm peer-valid:border-strokeBrand-strong peer-valid:text-textBrand peer-valid:shadow-md"
              , onpointerdown_ "htmx.trigger('#ai-search-input', 'htmx:trigger')"
              ]
              do
                faSprite_ "arrow-right" "regular" "h-4 w-4"
                "Submit"
            label_ [Lucid.for_ "ai-search-chkbox", class_ "cursor-pointer p-1", data_ "tippy-content" "Collapse AI search"] $ faSprite_ "arrows-minimize" "regular" "h-4 w-4 inline-block text-iconBrand"

          -- Above the editor, not below it: the suggestions dropdown opens downward
          -- over that space and hid the message while the user was typing the query
          -- that caused it. Takes no room at all when there is no error — a
          -- permanently reserved line is a worse trade than the occasional shift.
          div_
            [ class_ "text-xs text-textError px-2 py-0.5 rounded items-center gap-1 hidden bg-fillError-weak group-data-[query-state=error]/qbox:flex"
            , id_ "query-parse-error"
            , data_ "msg" (fromMaybe "" config.parseError)
            , Aria.live_ "polite"
            ]
            do
              faSprite_ "triangle-exclamation" "regular" "h-3 w-3 shrink-0"
              span_ [id_ "query-parse-error-msg"] $ toHtml $ fromMaybe "" config.parseError
          div_ [class_ "w-full flex flex-1 gap-2 justify-between items-stretch min-w-0 max-md:flex-wrap"] do
            div_ [id_ "queryBuilder", class_ "w-full flex-1 flex items-center min-w-0 min-h-[38px]"]
              $ term
                "query-editor"
                ( [id_ "filterElement", class_ "w-full flex items-center min-h-[38px]", term "default-value" (fromMaybe "" config.query), term "project-id" config.pid.toText]
                    -- The editor validates against the server, which needs the same source the
                    -- query will run under: metrics live in another table, so without this the
                    -- Metrics page squiggles `metric_name` on a query it then runs happily.
                    <> maybeToList (term "query-source" <$> config.source)
                    <> maybeToList (term "target-widget-preview" <$> config.targetWidgetPreview)
                    <> [term "widget-editor" "true" | isJust config.targetWidgetPreview]
                )
                (queryEditorSkeleton_ config.query)

            whenNothing_ config.targetWidgetPreview $ do
              div_ [class_ "gap-[2px] flex items-center max-md:hidden"] do
                span_ [class_ "text-textWeak"] "in"
                select_
                  [ class_ "ml-1 select select-sm w-full max-w-xs h-full bg-bgBase border-strokeStrong"
                  , name_ "target-spans"
                  , id_ "spans-toggle"
                  , Aria.label_ "Target span type"
                  , onchange_ "this.form.dispatchEvent(new Event('submit', {bubbles: true}))"
                  ]
                  $ forM_ ([("all-spans", "All spans"), ("root-spans", "Trace Root Spans"), ("service-entry-spans", "Service Entry Spans")] :: [(Text, Text)]) \(v, label) ->
                    option_ (value_ v : [selected_ "true" | fromMaybe "all-spans" config.targetSpan == v]) $ toHtml label

              div_ [class_ "inline-block max-md:hidden"] do
                button_ ([type_ "button", class_ "rounded-lg px-3 py-1 text-textStrong inline-flex items-center border border-strokeStrong h-full cursor-pointer", Aria.label_ "Save query"] <> popoverTrigger_ "save-query-pop") $ faSprite_ "floppy-disk" "regular" "h-5 w-5 text-iconNeutral"
                ul_ ([class_ "dropdown dropdown-end border border-strokeWeak menu bg-bgRaised rounded-box w-60 p-2 shadow-lg"] <> popoverPanel_ "save-query-pop") do
                  li_ $ label_ [Lucid.for_ "saveQueryMdl", onclick_ "document.getElementById('saveQueryMdl').dataset.pendingQuery = null;"] "Save query to Query Library"
            button_
              [ type_ "submit"
              , class_ "leading-none rounded-lg px-3 py-1 cursor-pointer !h-auto btn btn-primary"
              , Aria.label_ "Run query"
              , onpointerdown_ "this.form.dispatchEvent(new Event('submit', {bubbles: true}))"
              ]
              do
                faSprite_ "magnifying-glass" "regular" "h-4 w-4 inline-block"

      div_ [class_ "flex items-between justify-between max-md:flex-wrap max-md:gap-0.5"] do
        div_ [class_ "flex items-center gap-2 max-md:gap-1 max-md:w-full"] do
          visualizationTabs_ config.vizType config.updateUrl config.targetWidgetPreview config.alert
          div_ [class_ "hidden group-has-[#viz-sessions:checked]/pg:flex items-center gap-1"] do
            span_ [class_ "text-textWeak text-xs"] "Sort:"
            select_
              [ class_ "select select-sm max-w-[130px]"
              , id_ "session-sort-select"
              , Aria.label_ "Sort sessions by"
              , onchange_ "window.setQueryParamAndReload('sort_by', this.value)"
              ]
              $ forM_ ([("last_seen", "Last seen"), ("first_seen", "First seen"), ("duration", "Duration"), ("errors", "Errors"), ("events", "Events")] :: [(Text, Text)]) \(v, label) ->
                option_ [value_ v] $ toHtml label
            script_ "document.getElementById('session-sort-select').value = new URLSearchParams(window.location.search).get('sort_by') || 'last_seen';"
          div_ [class_ "hidden group-has-[#viz-patterns:checked]/pg:flex items-center gap-1"] do
            let isCustom = any (`notElem` map fst knownPatternFields) config.patternSelected
            select_
              [ class_ "select select-sm max-w-[140px]"
              , id_ "pattern-target-select"
              , [__|on change
                    if my value is '__custom__'
                      add .hidden to me
                      remove .hidden from #pattern-target-input
                      call #pattern-target-input.focus()
                    else
                      call window.setQueryParamAndReload('pattern_target', my value)
                    end|]
              ]
              do
                forM_ knownPatternFields \(v, label) ->
                  option_ (value_ v : [selected_ "" | config.patternSelected == Just v || (v == "summary" && isNothing config.patternSelected)]) $ toHtml label
                option_ (value_ "__custom__" : [selected_ "" | isCustom]) "Other field..."
            input_
              [ class_ $ "input input-sm max-w-[200px]" <> bool " hidden" "" isCustom
              , id_ "pattern-target-input"
              , list_ "pattern-field-list"
              , placeholder_ "e.g. attributes.url.path"
              , value_ $ bool "" (fromMaybe "" config.patternSelected) isCustom
              , [__|on keydown[key is 'Enter']
                    call window.setQueryParamAndReload('pattern_target', my value)
                  end
                  on blur
                    if my value is not ''
                      call window.setQueryParamAndReload('pattern_target', my value)
                    else
                      add .hidden to me
                      remove .hidden from #pattern-target-select
                      set #pattern-target-select.value to 'summary'
                    end|]
              ]
            datalist_ [id_ "pattern-field-list"]
              $ forM_ (Map.keys Schema.telemetrySchema.fields) \f ->
                option_ [value_ f] ""
          span_ [class_ "text-textDisabled mx-2 text-xs max-md:hidden"] "|"
          termRaw "query-builder" [term "query-editor-selector" "#filterElement"] ("" :: Text)
          whenNothing_ config.targetWidgetPreview
            $ popularSearchChips_ config.pid noActiveQuery
          -- Mobile-only hide timeline, inside the viz tabs row so it stays on the same line
          fieldset_ [class_ "fieldset md:hidden ml-auto"] $ label_ [class_ "label space-x-1 min-h-6 items-center group-has-[.default-chart:checked]/pg:flex"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-xs rounded-sm toggle-chart", [__|init if window.innerWidth < 768 set my.checked to true|]]
              >> span_ [class_ "text-xs"] "Hide timeline"

        whenJust config.mobileExtra
          $ div_ [class_ "md:hidden flex items-center gap-2 text-sm w-full"]

        div_ [class_ "flex justify-end gap-2 max-md:hidden"] do
          fieldset_ [class_ "fieldset"] $ label_ [class_ "label space-x-1 hidden group-has-[.default-chart:checked]/pg:block"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded-sm toggle-chart"] >> span_ "Hide timeline"
          fieldset_ [class_ "fieldset"] $ label_ [class_ "label space-x-1 group-has-[#viz-patterns:checked]/pg:hidden group-has-[#viz-sessions:checked]/pg:hidden"] do
            input_
              $ [ type_ "checkbox"
                , id_ "create-alert-toggle"
                , class_ "checkbox checkbox-sm rounded-sm"
                , -- The #create-alert-toggle hash deep-link replays this element's own change
                  -- handler rather than duplicating the force-switch body on an ancestor init.
                  [__|init if window.location.hash.includes('create-alert-toggle') then set my.checked to true then send change to me end
                   on change if me.checked
                     set #viz-timeseries.checked to true
                     call updateVizTypeInUrl('timeseries', true)
                     set widgetJSON.type to 'timeseries'
                     send 'update-widget' to #visualization-widget-container
                   end|]
                ]
              <> [checked_ | config.alert]
            span_ "Create monitor"

  queryEditorInitializationCode config.vizType config.pid


-- | Helper for visualizing the data with different chart types
visualizationTabs_ :: Maybe Text -> Bool -> Maybe Text -> Bool -> Html ()
visualizationTabs_ vizTypeM updateUrl widgetContainerId alert =
  div_ [class_ "tabs tabs-box tabs-outline tabs-xs bg-fillWeak p-1 rounded-lg", id_ "visualizationTabs", role_ "radiogroup", Aria.label_ "Visualization type"] do
    -- A widget container means we are in the dashboard widget editor rather than the
    -- log explorer.
    let inWidgetEditor = isJust widgetContainerId
        -- A dashboard widget starts as a chart. Logs is a full log table — the most
        -- expensive thing on a dashboard and the wrong thing to drop on one by default.
        defaultVizType = fromMaybe (bool "logs" "timeseries" (alert || inWidgetEditor)) vizTypeM
        containerSelector = fromMaybe "visualization-widget-container" widgetContainerId
        -- Sessions is not a valid alerting surface. Patterns and Sessions are log-explorer
        -- views with no corresponding WidgetType, so a widget set to either could not be
        -- decoded on save: the tab was offered and simply did not work.
        hidden = bool [] ["sessions"] alert <> bool [] ["patterns", "sessions"] inWidgetEditor
        visible = filter (\(_, _, t, _) -> t `notElem` hidden) visTypes
    forM_ visible \(_icon, label, vizType, emoji) ->
      label_ [data_ "value" vizType, class_ "tab !shadow-none !border-strokeWeak flex gap-1"] do
        input_
          $ [ type_ "radio"
            , name_ "visualization"
            , id_ $ "viz-" <> vizType
            , class_ $ bool "no-chart" "default-chart" (vizType `elem` ["logs", "patterns", "sessions"])
            , value_ vizType
            , data_ "update-url" (bool "false" "true" updateUrl)
            , data_ "container-id" containerSelector
            , -- swapSessionsRegionIfNeeded (defined in queryEditorInitializationCode) refetches
              -- #page-summary-region when a viz change crosses the sessions boundary, since
              -- sessions renders a different server region than other viz types.
              [__| on change if my.checked
                          set prevViz to window.currentVisualizationType
                          call updateVizTypeInUrl(my.value, @data-update-url === 'true')
                          if window.widgetJSON
                            set widgetJSON.type to my.value
                            send 'update-widget' to #{@data-container-id}
                          end
                          if #resultTable exists
                            set #resultTable's mode to my.value
                            set #resultTable's mode to 'logs' unless my.value is 'patterns' or my.value is 'sessions'
                            call #resultTable.refetchLogs()
                          end
                          if window.swapSessionsRegionIfNeeded then call window.swapSessionsRegionIfNeeded(my.value, prevViz)
                        end
                     |]
            ]
          <> [checked_ | vizType == defaultVizType]
        -- Emojis only in widget mode, not in the log explorer
        when (isJust widgetContainerId) $ span_ [class_ "text-iconNeutral leading-none"] $ toHtml emoji
        span_ $ toHtml label


-- | Static stand-in rendered as a child of @\<query-editor\>@. Monaco is loaded on idle
-- rather than during first paint (see @deferredComponents@ in web-components/src/index.ts),
-- and the element is empty until then — without this the search bar is a blank gap for the
-- first second of every page load. Mirrors the component's own render() shell so the upgrade
-- is not a visible jump; Lit clears these children on first render (the component renders
-- into its light DOM), so nothing here needs to be interactive.
queryEditorSkeleton_ :: Maybe Text -> Html ()
queryEditorSkeleton_ query =
  div_ [class_ "relative w-full h-full pl-2 flex border rounded-md border-strokeStrong"] do
    div_ [class_ "relative overflow-x-hidden w-full flex-1"]
      $ div_ [class_ "w-full text-sm leading-[18px] pt-2 truncate font-mono"]
      $ case query of
        Just q | not (T.null q) -> toHtml q
        -- No opacity dimming on top of the token: at 14px that lands under the contrast
        -- floor in dark mode, and this is a stand-in nobody should have to squint at.
        _ -> span_ [class_ "text-textWeak"] "level == \"ERROR\""
    div_ [class_ "p-1"]
      -- Same group-has variant as the real pill, or the skeleton shows an AI-search
      -- button for the pre-upgrade moment on a page loaded with AI search already on.
      $ span_ [class_ "px-3 py-0.5 h-full inline-flex gap-2 items-center border border-strokeBrand-strong text-textBrand rounded-sm group-has-[.ai-search:checked]/fltr:hidden"] do
        faSprite_ "sparkles" "regular" "inline-block icon h-4 w-4 text-iconBrand"
        "AI search"


-- | Shared dropdown content for the query library (Popular + Saved + Recent tabs)
queryLibraryContent_ :: V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
queryLibraryContent_ queryLibSaved queryLibRecent =
  div_ [id_ "queryLibraryContent"]
    $ div_ [class_ "tabs tabs-box tabs-sm tabs-outline items-center p-0 h-full", role_ "tablist", id_ "queryLibraryTabListEl"] do
      tabPanel_ "Popular" True popularQueriesContent_
      tabPanel_ "Saved" False (queryLibraryItems_ "Saved" queryLibSaved)
      tabPanel_ "Recent" False (queryLibraryItems_ "Recent" queryLibRecent)
  where
    tabPanel_ :: Text -> Bool -> Html () -> Html ()
    tabPanel_ label isDefault content = do
      input_ $ [type_ "radio", name_ "querylib", role_ "tab", class_ "tab", Aria.label_ label] <> [checked_ | isDefault]
      div_ [role_ "tabpanel", class_ "tab-content max-h-[60dvh] overflow-y-auto"] content

    queryLibraryItems_ :: Text -> V.Vector Projects.QueryLibItem -> Html ()
    queryLibraryItems_ label items = do
      searchBar_ label
      div_ [class_ $ "divide-y divide-strokeWeak dataLibContent" <> label] $ V.forM_ items (queryLibItem_ (label == "Recent"))

    popularQueriesContent_ :: Html ()
    popularQueriesContent_ =
      div_ [class_ "divide-y divide-strokeWeak"]
        $ forM_ popularQueries \(query, label, desc) ->
          div_
            [ class_ "query-item px-3 py-2 hover:bg-fillWeak cursor-pointer transition-colors"
            , onclick_ $ applyQueryJS query <> "; " <> hidePopoverJS
            ]
            do
              div_ [class_ "text-sm text-textStrong"] $ toHtml label
              code_ [class_ "text-xs text-textWeak line-clamp-2 break-all block mt-0.5"] $ toHtml query
              whenJust desc $ small_ [class_ "text-xs text-textDisabled mt-0.5 block"] . toHtml

    searchBar_ :: Text -> Html ()
    searchBar_ label = div_ [class_ "flex gap-2 sticky top-0 px-3 py-2 bg-bgBase border-b border-strokeWeak z-20"] do
      label_ [class_ "input input-sm flex items-center gap-2 flex-1"] do
        faSprite_ "magnifying-glass" "regular" "h-3.5 w-3.5 opacity-70"
        input_
          [ type_ "text"
          , class_ "grow"
          , placeholder_ "Search"
          , data_ "filterParent" $ "dataLibContent" <> label
          , [__|on keyup
                 if the event's key is 'Escape' set my value to '' then trigger keyup
                 else show <.query-item/> in .{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()|]
          ]
      when (label == "Saved")
        $ label_ [class_ "tabs tabs-sm tabs-box tabs-outline bg-fillWeak text-textInverse-weak shrink items-center h-8", role_ "tablist"] do
          input_ [class_ "hidden", type_ "checkbox", id_ "queryLibraryGroup"]
          div_ [role_ "tab", class_ "tab h-full bg-fillWeaker group-has-[#queryLibraryGroup:checked]/pg:bg-transparent px-2", data_ "tippy-content" "My queries"] $ faSprite_ "user" "regular" "w-3 h-3"
          div_ [role_ "tab", class_ "tab h-full group-has-[#queryLibraryGroup:checked]/pg:bg-fillWeaker px-2", data_ "tippy-content" "All team queries"] $ faSprite_ "users" "regular" "w-3 h-3"


hidePopoverJS :: Text
hidePopoverJS = "document.getElementById('queryLibraryPopover')?.hidePopover()"


applyQueryJS :: Text -> Text
applyQueryJS q = "window.applyQuery(" <> decodeUtf8 (AE.encode q) <> ")"


popularQueries :: [(Text, Text, Maybe Text)]
popularQueries =
  [ ("level == \"ERROR\"", "Show errors", Nothing)
  , ("attributes.http.response.status_code >= 500", "HTTP 5xx responses", Nothing)
  , ("duration > 1000000000", "Slow requests (>1s)", Nothing)
  , ("attributes.exception.type != null", "Exceptions", Nothing)
  , ("attributes.error.type != null", "Error types", Nothing)
  , ("kind == \"span\" and duration > 5000000000", "Slow spans (>5s)", Nothing)
  , ("status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), resource.service.name", "Errors by service", Just "Bar chart — error rate per service over time")
  , ("| summarize count(*) by bin_auto(timestamp), level", "Volume by level", Just "Bar chart — log volume breakdown")
  , ("| summarize percentiles(duration, 50, 90, 99) by bin_auto(timestamp)", "Latency percentiles", Just "Line chart — p50/p90/p99 over time")
  ]


-- | Visualization types used across the application
-- Each entry is (icon, label, type, emoji)
--
-- TODO: Support the other viz types.
visTypes :: [(Text, Text, Text, Text)]
visTypes =
  [ ("list-view", "Logs", "logs", "📋")
  , ("bar-chart", "Bar", "timeseries", "📊")
  , ("duo-line-chart", "Line", "timeseries_line", "📈")
  , ("log-patterns", "Patterns", "patterns", "🔍")
  , ("users", "Sessions", "sessions", "👥")
  -- , ("duo-pie-chart", "Pie", "pie_chart", "🥧")
  -- , ("duo-scatter-chart", "Scatter", "distribution", "📉")
  -- , ("hashtag", "Number", "stat", "🔢")
  -- , ("guage", "Guage", "", "🧮")
  -- , ("text", "Text", "", "📝")
  ]


-- | Simplified query library item with reduced DOM nodes
queryLibItem_ :: Bool -> Projects.QueryLibItem -> Html ()
queryLibItem_ isRecent qli =
  div_
    [ class_ $ "query-item px-3 py-2 hover:bg-fillWeak cursor-pointer group relative transition-colors " <> bool "hidden group-has-[#queryLibraryGroup:checked]/pg:block" "" qli.byMe
    , data_ "query" qli.queryText
    , data_ "query-id" qli.id.toText
    ]
    do
      div_ [class_ "pr-8", onclick_ $ "document.getElementById('filterElement').handleAddQuery(this.closest('.query-item').dataset.query); " <> hidePopoverJS] do
        div_ [class_ "flex items-baseline gap-2 mb-1"] do
          whenJust qli.title (\title -> span_ [class_ "font-medium text-sm"] $ toHtml title <> " •")
          small_ [class_ "text-textWeak text-xs whitespace-nowrap"]
            $ toHtml (displayTimestamp $ formatUTC qli.createdAt)
            >> when qli.byMe " • by me"
        code_ [class_ "queryText text-xs block whitespace-pre-wrap break-words opacity-75"] $ toHtml qli.queryText

      div_ [class_ "query-actions absolute top-0 right-3 opacity-0 group-hover:opacity-100 group-focus-within:opacity-100 focus-within:opacity-100 flex gap-1"] do
        actionBtn_ "Run this query" "play" [onclick_ $ "event.preventDefault(); document.getElementById('filterElement').handleAddQuery(this.closest('.query-item').dataset.query, true); " <> hidePopoverJS]
        actionBtn_ "Copy query to clipboard" "copy" [onclick_ "event.preventDefault(); navigator.clipboard.writeText(this.closest('.query-item').dataset.query).then(() => { document.body.dispatchEvent(new CustomEvent('successToast', {detail: {value: ['Query copied to clipboard']}})); })"]
        when qli.byMe
          $ if isRecent
            then
              actionBtn_
                "Save as named query"
                "floppy-disk"
                [onclick_ "event.preventDefault(); document.getElementById('saveQueryMdl').dataset.pendingQuery = this.closest('.query-item').dataset.query; document.getElementById('queryLibId').value = ''; document.getElementById('saveQueryMdl').checked = true;"]
            else do
              actionBtn_
                "Edit query title"
                "pen-to-square"
                [onclick_ $ "event.preventDefault(); document.getElementById('queryLibId').value = '" <> qli.id.toText <> "'; document.getElementById('saveQueryMdl').checked = true;"]
              actionBtn_
                "Delete query"
                "trash"
                [ hxDelete_ $ "/p/" <> qli.projectId.toText <> "/log_explorer/queries/" <> qli.id.toText
                , hxTarget_ "#queryLibraryPopover"
                , hxSwap_ "innerHTML"
                , hxPushUrl_ "false"
                ]
  where
    actionBtn_ :: Text -> Text -> [Attribute] -> Html ()
    actionBtn_ tip icon attrs =
      button_ ([type_ "button", class_ "inline-flex items-center justify-center min-w-6 min-h-6 hover:bg-fillWeak rounded cursor-pointer", data_ "tippy-content" tip] <> attrs)
        $ faSprite_ icon "regular" "h-3 w-3"


-- | Popular search chips + query library dropdown, unified as one component.
-- When no query is active, shows "Try:" chips inline. "more" opens the full library dropdown.
popularSearchChips_ :: Projects.ProjectId -> Bool -> Html ()
popularSearchChips_ pid showChips =
  div_ [class_ "max-md:hidden group-has-[.ai-search:checked]/fltr:hidden inline-flex gap-1.5 text-xs items-center", id_ "queryLibraryParentEl"] do
    when showChips
      $ span_
        [ class_ "inline-flex gap-1.5 items-center"
        , id_ "popular-search-chips"
        , [__|on 'update-query' from window if (event.detail.value or '').trim() is not '' add .hidden to me else remove .hidden from me|]
        ]
        do
          span_ [class_ "text-textDisabled"] "Try:"
          forM_ (take 3 popularQueries) \(q, l, _) ->
            button_
              [ type_ "button"
              , -- py-1, not py-0.5: these chips were 22px tall, under the 24px WCAG 2.5.8
                -- minimum pointer target.
                class_ "px-2 py-1 rounded-md bg-fillWeaker border border-strokeWeak hover:border-strokeBrand-weak hover:bg-fillBrand-weak text-textWeak hover:text-textBrand cursor-pointer transition-colors"
              , onclick_ $ applyQueryJS q
              ]
              $ toHtml l
    button_
      [ type_ "button"
      , class_ "px-1.5 py-0.5 text-textBrand hover:underline cursor-pointer inline-flex items-center gap-1"
      , term "popovertarget" "queryLibraryPopover"
      , style_ "anchor-name: --querylib-anchor"
      , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer/queries"
      , hxTrigger_ "click once"
      , hxTarget_ "#queryLibraryPopover"
      , hxSwap_ "innerHTML"
      , hxIndicator_ "#queryLibraryLoader"
      ]
      do
        "Library"
        faSprite_ "chevron-down" "regular" "w-2.5 h-2.5"
    div_
      [ id_ "queryLibraryPopover"
      , term "popover" "auto"
      , class_ "bg-bgBase rounded-xl border-2 border-strokeStrong shadow-lg w-[480px] max-w-[90vw] min-h-16 overflow-hidden z-50 mt-1"
      , style_ "inset: unset; top: anchor(bottom); right: anchor(right); position-try-fallbacks: flip-block, flip-inline; position-anchor: --querylib-anchor"
      ]
      $ div_ [id_ "queryLibraryLoader", class_ "htmx-indicator h-16 flex items-center justify-center"]
      $ span_ [class_ "loading loading-spinner loading-sm text-textBrand", role_ "status", Aria.label_ "Loading query library"] ""


-- | Merge pre-computed facet values into the schema so the query editor shows real autocomplete values
enrichSchemaWithFacets :: Schema.Schema -> FacetData -> AE.Value
enrichSchemaWithFacets schema (FacetData facetMap) =
  AE.toJSON $ schema{Schema.fields = HM.foldlWithKey' mergeField schema.fields facetMap}
  where
    mergeField acc facetKey facetVals =
      let dotKey = T.replace "___" "." $ T.replace "severity___severity_" "severity." facetKey
          vals = Just $ map (.value) facetVals
       in Map.alter (Just . maybe (Schema.FieldInfo "string" "" vals) (\fi -> fi{Schema.examples = vals})) dotKey acc


-- | Initialization code for the query editor that sets up schema data, query library, and popular searches
queryEditorInitializationCode :: Maybe Text -> Projects.ProjectId -> Html ()
queryEditorInitializationCode vizTypeM pid = do
  let
    -- The (~365KB) enriched span schema is fetched from a dedicated endpoint
    -- rather than inlined, so it's out of the page payload and re-encode path.
    -- Cached in a window promise so it's fetched once per SPA session (reused
    -- across HTMX morph swaps), not on every render.
    schemaUrl = "/p/" <> pid.toText <> "/log_explorer/schema"
    popularQueriesJson = decodeUtf8 $ AE.encode Schema.popularOtelQueriesJson
    vizType = fromMaybe "logs" vizTypeM
  script_
    [text|
    // Set initial visualization type
    window.currentVisualizationType = "$vizType";
    
    // Function to update viz type in URL without reloading the page
    window.updateVizTypeInUrl = function(vizType, shouldUpdateUrl = true) {
      // Update the current visualization type
      window.currentVisualizationType = vizType;
      requestAnimationFrame(() => {
        // Only update URL if we're not in widget mode and shouldUpdateUrl is true
        const editor = document.getElementById('filterElement');
        const isWidgetMode = editor && editor.hasAttribute('target-widget-preview');
        
        if (shouldUpdateUrl && !isWidgetMode) {
          const url = new URL(window.location);
          url.searchParams.set('viz_type', vizType);
          history.replaceState({}, '', url.toString());
        }
        
        // Call the query editor's handleVisualizationChange method to update the query
        if (editor?.handleVisualizationChange) {
          const vizTypeMap = { 'bar': 'timeseries', 'line': 'timeseries_line' };
          editor.handleVisualizationChange(vizTypeMap[vizType] || vizType);
        }
      });
    };

    // Called by the AI-search response handler and query-builder.ts to switch viz type.
    window.handleVisualizationUpdate = function(vizType, widgetId) {
      window.requestAnimationFrame(() => {
        updateVizTypeInUrl(vizType);
        document.querySelector(`#visualizationTabs input[value='$${vizType}']`).checked = true;
        window.widgetJSON.type = vizType;
        document.getElementById(widgetId || 'visualization-widget-container').dispatchEvent(new Event('update-widget'));
      });
    };

    // Single owner for "apply this query to the editor". The query library closes AI search
    // on the way out (picking a saved query ends the AI interaction), but an AI response
    // keeps the panel and its prompt on screen so the phrasing can be refined and re-run —
    // collapsing it there reads as the app having thrown the question away.
    window.applyQuery = function(q, replace = true, closeAiSearch = true) {
      const chk = document.getElementById('ai-search-chkbox');
      if (chk && closeAiSearch) chk.checked = false;
      document.getElementById('filterElement')?.handleAddQuery(q, replace);
    };

    // Routes one /ai_search response into the subsystems it can touch. Lives here, not
    // inline on the input, because every arm is a call into a JS global defined above —
    // the element keeps a one-expression hook and this stays readable and testable.
    window.applyAiSearchResult = function(evt, el) {
      const ctx = evt.detail && evt.detail.ctx;
      const text = (ctx && ctx.text) || '';
      if (!ctx || !(ctx.response && ctx.response.status < 400)) {
        if (text.includes('INVALID_QUERY_ERROR')) {
          document.body.dispatchEvent(new CustomEvent('errorToast', { bubbles: true,
            detail: { value: ['Could not generate a query. Try being more specific, e.g. "show errors from payment-service in the last 2 hours"'] } }));
        }
        return;
      }
      let result;
      try { result = JSON.parse(text); } catch (e) { return; }
      if (result.time_range) window.updateTimePicker(result.time_range);
      if (result.query) window.applyQuery(result.query, true, false);
      // Native dispatch, not htmx.trigger: both listeners (log-list.ts, widgets.ts) are
      // plain addEventListener('submit'), so this needs no htmx API surface.
      else if (result.time_range) document.getElementById('log_explorer_form')?.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));
      if (result.visualization_type) {
        window.handleVisualizationUpdate(result.visualization_type, el.dataset.containerId || 'visualization-widget-container');
      }
    };

    // Initialise after the interaction-triggered query-editor chunk upgrades the element.
    customElements.whenDefined('query-editor').then(function initEditor() {
      const editor = document.getElementById('filterElement');
      if (!editor || !window.schemaManager?.setSchemaData) return;
      editor.setQueryLibrary?.(window.queryLibraryData || []);
      const loadSchema = () => {
        window.__spanSchemaPromise = window.__spanSchemaPromise || fetch("$schemaUrl", {headers: {Accept: "application/json"}, credentials: "include"}).then(r => r.json());
        window.__spanSchemaPromise.then(s => {
          window.schemaManager.setSchemaData('spans', s);
          const qb = document.querySelector('query-builder');
          if (qb?.refreshFieldSuggestions) qb.refreshFieldSuggestions();
        }).catch(e => console.warn('[query-editor] schema fetch failed', e));
      };
      if (window.__spanSchemaPromise) loadSchema();
      else editor.addEventListener('focusin', loadSchema, {once: true});
      if (editor.setPopularSearches) editor.setPopularSearches($popularQueriesJson);
    });

    // Bucket-bar click handler for the sessions header. Defined here (not in the
    // header markup) so the header stays script-free and can be injected via
    // innerHTML from the sessions data response. Dispatches the same update-query
    // event the log-list uses for chart-zoom so the table refetches.
    window.__sessionsBucketFilter = function(fromEpoch, toEpoch) {
      if (!Number.isFinite(fromEpoch) || !Number.isFinite(toEpoch) || fromEpoch <= 0) return;
      const from = new Date(fromEpoch * 1000).toISOString();
      const to = new Date(toEpoch * 1000).toISOString();
      if (window.updateTimePicker) {
        window.updateTimePicker({ from, to }, { skipSetParams: true });
      } else {
        console.warn('[sessions-header] updateTimePicker missing; picker UI will not reflect filter');
      }
      const p = new URLSearchParams(window.location.search);
      p.set('from', from); p.set('to', to); p.delete('since');
      const url = window.location.pathname + '?' + p.toString() + window.location.hash;
      window.history.replaceState({}, '', url);
      document.dispatchEvent(new CustomEvent('update-query', { bubbles: true, detail: { source: 'sessions-header-bar', timeRange: from + ' \u2192 ' + to } }));
    };

    // Fill the over-time chart's per-bar tooltips (sessions + patterns) in the
    // browser's local timezone, so they match the table and time picker. Runs
    // after the header HTML is injected into #page-summary-region. Reads
    // data-bucket-start/width off the container and data-bi/data-count off each
    // bar. Axis labels are server-rendered <local-time> elements — only the
    // tippy tooltip strings need JS (attributes can't host an element).
    window.formatSummaryChart = function(root) {
      const chart = root && root.querySelector('[data-summary-chart]');
      if (!chart) return;
      const start = +chart.dataset.bucketStart, width = +chart.dataset.bucketWidth;
      if (!Number.isFinite(start) || !Number.isFinite(width) || width <= 0) return;
      const bars = Array.prototype.slice.call(chart.querySelectorAll('[data-bi]'));
      if (!bars.length) return;
      const firstEpoch = start + (+bars[0].dataset.bi) * width;
      const endEpoch = start + ((+bars[bars.length - 1].dataset.bi) + 1) * width;
      const spanDays = (endEpoch - firstEpoch) / 86400;
      const opts = spanDays >= 1
        ? { month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit' }
        : { hour: '2-digit', minute: '2-digit' };
      const fmt = e => new Date(e * 1000).toLocaleString([], opts);
      // A single bar means the range fits one bucket (e.g. patterns' hourly
      // rollup at <1h); append the container note so a hover explains why.
      const note = bars.length === 1 ? (chart.dataset.note || '') : '';
      bars.forEach(bar => {
        const from = start + (+bar.dataset.bi) * width;
        const base = bar.getAttribute('data-count') || '';
        bar.setAttribute('data-tippy-content', fmt(from) + ' \u2013 ' + fmt(from + width) + (base ? ' \u00b7 ' + base : '') + (note ? ' \u00b7 ' + note : ''));
      });
    };

    // Swap the #page-summary-region when the viz tab change crosses the summary
    // boundary. Sessions and patterns both render a data-injected summary header
    // (skeleton now, real HTML delivered with the data refetch — no extra scan);
    // every other viz type renders the chart+latency region via HTMX.
    window.swapSessionsRegionIfNeeded = function(newViz, prevViz) {
      const isSummary = v => v === 'sessions' || v === 'patterns';
      if (!isSummary(newViz) && !isSummary(prevViz)) return;
      const region = document.getElementById('page-summary-region');
      if (!region) return;
      const tpl = document.getElementById(isSummary(newViz) ? 'sessions-summary-skeleton' : 'chart-summary-skeleton');
      if (tpl) { region.setAttribute('aria-busy', 'true'); region.innerHTML = tpl.innerHTML; }
      if (isSummary(newViz)) return; // summary arrives with the data refetch; don't scan again
      if (!window.htmx) return;
      const url = new URL(window.location);
      url.searchParams.set('viz_type', newViz);
      window.htmx.ajax('GET', url.pathname + url.search, {
        target: '#page-summary-region',
        select: '#page-summary-region',
        swap: 'outerHTML'
      });
    };

    // Inline parse error: the markup (icon + message span) is rendered by Lucid;
    // JS only fills the text and toggles visibility.
    // Sets one attribute; the border and the message row follow from it in CSS.
    // A repeated set of the same message is a no-op so a refetch can't twitch the page.
    window.__setQueryParseError = function(msg) {
      msg = msg || '';
      const el = document.getElementById('query-parse-error');
      if (!el || el.dataset.msg === msg) return;
      el.dataset.msg = msg;
      const m = document.getElementById('query-parse-error-msg');
      if (m) m.textContent = msg;
      document.getElementById('queryBox')?.setAttribute('data-query-state', msg ? 'error' : 'ok');
    };
    window.showQueryParseError = msg => window.__setQueryParseError(msg);
    window.clearQueryParseError = () => window.__setQueryParseError('');
    // The editor re-validates right after this event and re-asserts a still-valid
    // error, so clearing here would only flash the strip off and on.
    window.addEventListener('update-query', () => window.clearQueryParseError());
    // Listen for server-sent parse error events (via HX-Trigger header)
    document.body.addEventListener('showParseError', (e) => {
      const detail = e.detail;
      const msg = Array.isArray(detail) ? detail[0] : (detail?.value || detail || 'Invalid query syntax');
      window.showQueryParseError(msg);
    });
    |]
