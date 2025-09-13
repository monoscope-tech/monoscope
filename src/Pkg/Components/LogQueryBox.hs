module Pkg.Components.LogQueryBox (logQueryBox_, visTypes, queryLibrary_, queryEditorInitializationCode, LogQueryBoxConfig (..), visualizationTabs_) where

import Data.Aeson qualified as AE
import Data.Default
import Data.Text qualified as T ()
import Data.Vector qualified as V
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import NeatInterpolation (text)
import Pkg.Components.Modals qualified as Components
import Relude
import Utils (displayTimestamp, faSprite_, formatUTC, onpointerdown_)


-- | Configuration record for the log query box component
data LogQueryBoxConfig = LogQueryBoxConfig
  { pid :: Projects.ProjectId
  , currentRange :: Maybe (Text, Text)
  , source :: Maybe Text
  , targetSpan :: Maybe Text
  , query :: Maybe Text
  , vizType :: Maybe Text
  , queryLibRecent :: V.Vector Projects.QueryLibItem
  , queryLibSaved :: V.Vector Projects.QueryLibItem
  , updateUrl :: Bool
  , alert :: Bool
  -- ^ Whether to update the URL when the query changes
  , targetWidgetPreview :: Maybe Text
  -- ^ ID of the widget preview element to update when the query changes
  }
  deriving (Generic, Show)
  deriving anyclass (Default)


-- | Reusable log query box component that can be used in both Logs and Dashboards pages
-- This component provides a unified interface for querying logs and visualizing data
logQueryBox_ :: LogQueryBoxConfig -> Html ()
logQueryBox_ config = do
  Components.modal_ "saveQueryMdl" "" $ form_
    [ class_ "flex flex-col p-3 gap-3"
    , hxGet_ $ "/p/" <> config.pid.toText <> "/log_explorer?layout=SaveQuery"
    , hxVals_ "js:{query:window.getQueryFromEditor()}"
    , hxTarget_ "#queryLibraryParentEl"
    , hxSwap_ "outerHTML"
    , hxSelect_ "#queryLibraryParentEl"
    , hxPushUrl_ "false"
    ]
    do
      strong_ "Please input a title for your query"
      input_ [type_ "hidden", value_ "", name_ "queryLibId", id_ "queryLibId"]
      input_ [class_ "input input-md", placeholder_ "query title", name_ "queryTitle"]
      button_ [type_ "submit", class_ "btn cursor-pointer bg-linear-to-b from-[#067cff] to-[#0850c5] text-white"] "Save"
  form_
    [ id_ "log_explorer_form"
    , class_ "flex flex-col gap-1 w-full max-w-full"
    , [__| on keydown if event.key is 'Enter' halt |]
    ]
    do
      div_ [class_ "flex flex-col gap-2 items-stretch justify-center group/fltr"] do
        div_ [class_ "p-1 flex-1 flex flex-col gap-2  bg-fillWeaker rounded-lg border border-strokeWeak group-has-[.ai-search:checked]/fltr:border-2 group-has-[.ai-search:checked]/fltr:border-iconBrand group-has-[.ai-search:checked]/fltr:shadow-xs shadow-strokeBrand-weak"] do
          input_
            $ [ class_ "hidden ai-search"
              , type_ "checkbox"
              , id_ "ai-search-chkbox"
              , [__|on change if me.checked then call #ai-search-input.focus() end
                  on keydown[key=='Space' and shiftKey] from document set #ai-search-chkbox.checked to true
                  |]
              ]
            <> [checked_ | isJust config.targetWidgetPreview]
          script_
            [text|
            document.addEventListener('keydown', function(e) {
              if (e.key === "?" && !e.ctrlKey && !e.metaKey && !e.altKey && (e.target.tagName !== 'INPUT') && (e.target.tagName !== 'TEXTAREA') && (e.target.contentEditable !== 'true')) {
                e.preventDefault();
                e.stopPropagation();
                document.getElementById("ai-search-chkbox").checked = true;
                document.getElementById("ai-search-input").focus()
                document.getElementById("ai-search-input").value=""
              }
            });
            window.handleVisualizationUpdate = function(vizType, widgetId) {
              console.log("handleVisualizationUpdate => ", vizType);
              window.requestAnimationFrame(() => {
                console.log("handleVisualizationUpdate => ", vizType);
                updateVizTypeInUrl(vizType);
                document.querySelector(`#visualizationTabs input[value='$${vizType}']`).checked = true;
                window.widgetJSON.type = vizType;
                const containerId = widgetId || 'visualization-widget-container';
                document.getElementById(containerId).dispatchEvent(new Event('update-widget'));
              });
            }
            |]
          div_ [class_ "w-full gap-2 items-center px-2 hidden group-has-[.ai-search:checked]/fltr:flex"] do
            faSprite_ "sparkles" "regular" "h-4 w-4 inline-block text-iconBrand"
            input_
              [ class_ "border-0 w-full flex-1 p-2 outline-none peer"
              , placeholder_ "Ask. Eg: Logs with errors. Hit Enter to submit"
              , id_ "ai-search-input"
              , autofocus_
              , required_ "required"
              , name_ "input"
              , hxPost_ $ "/p/" <> config.pid.toText <> "/log_explorer/ai_search"
              , hxTrigger_ "input[this.value.trim().length > 0] changed delay:1s"
              , hxSwap_ "none"
              , hxExt_ "json-enc"
              , term "hx-validate" "false"
              , hxIndicator_ "#ai-search-loader"
              , term "data-container-id" (fromMaybe "visualization-widget-container" config.targetWidgetPreview)
              , [__|on keydown[key=='Escape'] set #ai-search-chkbox.checked to false
                   on keydown[key=='Enter'] 
                     if my.value.trim().length > 0 
                       then halt then trigger htmx:trigger 
                     end
                   on htmx:afterRequest 
                     if event.detail.successful 
                       then 
                         call JSON.parse(event.detail.xhr.responseText) set :result to it
                         if :result.query then call #filterElement.handleAddQuery(:result.query, true) end
                         if :result.visualization_type
                           then
                             set vizType to :result.visualization_type
                             set widgetId to (@data-container-id or 'visualization-widget-container')
                             call window.handleVisualizationUpdate(vizType, widgetId)
                         end
                     else
                       if event.detail.xhr.responseText and event.detail.xhr.responseText.includes('INVALID_QUERY_ERROR')
                         then
                           send errorToast(value:['Could not understand your query. Please try rephrasing it or use a more specific request.']) to <body/>
                       end
                     end|]
              ]
            span_ [class_ "htmx-indicator", id_ "ai-search-loader"] $ faSprite_ "spinner" "regular" "w-4 h-4 animate-spin"
            a_
              [ class_ "px-3 py-0.5 inline-flex gap-2 items-center cursor-pointer border text-textDisabled shadow-strokeBrand-weak hover:border-strokeBrand-weak rounded-sm peer-valid:border-strokeBrand-strong peer-valid:text-textBrand peer-valid:shadow-md"
              , onpointerdown_ "htmx.trigger('#ai-search-input', 'htmx:trigger')"
              ]
              do
                faSprite_ "arrow-right" "regular" "h-4 w-4"
                "Submit"
            label_ [Lucid.for_ "ai-search-chkbox", class_ "cursor-pointer p-1", data_ "tippy-content" "Collapse APItoolkit AI without losing your query"] $ faSprite_ "arrows-minimize" "regular" "h-4 w-4 inline-block text-iconBrand"

          div_ [class_ "w-full flex flex-1 gap-2 justify-between items-stretch min-w-0"] do
            unless (isJust config.targetWidgetPreview)
              $ queryLibrary_ config.pid config.queryLibSaved config.queryLibRecent

            div_ [id_ "queryBuilder", class_ "w-full flex-1 flex items-center min-w-0"]
              $ termRaw
                "query-editor"
                ( [id_ "filterElement", class_ "w-full h-full flex items-center", term "default-value" (fromMaybe "" config.query)]
                    <> maybeToList (term "target-widget-preview" <$> config.targetWidgetPreview)
                    <> [term "widget-editor" "true" | isJust config.targetWidgetPreview]
                )
                ("" :: Text)

            unless (isJust config.targetWidgetPreview) $ do
              div_ [class_ "gap-[2px] flex items-center"] do
                span_ [class_ "text-textWeak"] "in"
                select_
                  [ class_ "ml-1 select select-sm w-full max-w-xs h-full bg-transparent border-strokeStrong"
                  , name_ "target-spans"
                  , id_ "spans-toggle"
                  , onchange_ "this.form.dispatchEvent(new Event('submit', {bubbles: true}))"
                  ]
                  do
                    let target = fromMaybe "all-spans" config.targetSpan
                    option_ (value_ "all-spans" : ([selected_ "true" | target == "all-spans"])) "All spans"
                    option_ (value_ "root-spans" : ([selected_ "true" | target == "root-spans"])) "Trace Root Spans"
                    option_ (value_ "service-entry-spans" : ([selected_ "true" | target == "service-entry-spans"])) "Service Entry Spans"

              div_ [class_ "dropdown dropdown-hover dropdown-bottom dropdown-end"] do
                div_ [class_ "rounded-lg px-3 py-2 text-textStrong inline-flex items-center border border-strokeStrong h-full", tabindex_ "0", role_ "button"] $ faSprite_ "floppy-disk" "regular" "h-5 w-5 text-iconNeutral"
                ul_ [tabindex_ "0", class_ "dropdown-content border menu bg-base-100 rounded-box z-1 w-60 p-2 shadow-lg"] do
                  li_ $ label_ [Lucid.for_ "saveQueryMdl"] "Save query to Query Library"
            button_
              [ type_ "submit"
              , class_ "leading-none rounded-lg px-3 py-2 cursor-pointer !h-auto btn btn-primary"
              , onpointerdown_ "this.form.dispatchEvent(new Event('submit', {bubbles: true}))"
              ]
              do
                faSprite_ "magnifying-glass" "regular" "h-4 w-4 inline-block"
      div_ [class_ "flex items-between justify-between"] do
        div_ [class_ "flex items-center gap-2"] do
          visualizationTabs_ config.vizType config.updateUrl config.targetWidgetPreview config.alert
          span_ [class_ "text-textDisabled mx-2 text-xs"] "|"
          termRaw "query-builder" [term "query-editor-selector" "#filterElement"] ("" :: Text)

        -- Results will be rendered by the virtual table component

        div_ [class_ "flex justify-end gap-2"] do
          fieldset_ [class_ "fieldset"] $ label_ [class_ "label space-x-1 hidden group-has-[#viz-logs:checked]/pg:block"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded-sm toggle-chart"] >> span_ "hide timeline"
          fieldset_ [class_ "fieldset"] $ label_ [class_ "label space-x-1"] do
            input_
              $ [ type_ "checkbox"
                , id_ "create-alert-toggle"
                , class_ "checkbox checkbox-sm rounded-sm"
                , [__|on change 
                     if me.checked
                       -- Force switch to chart visualization when creating alert
                       set #viz-timeseries.checked to true
                       call updateVizTypeInUrl('timeseries', true)
                       set widgetJSON.type to 'timeseries'
                       send 'update-widget' to #visualization-widget-container
                     end
                  |]
                ]
              <> [checked_ | config.alert]
            span_ "create alert"

  -- Include initialization code for the query editor
  queryEditorInitializationCode config.queryLibRecent config.queryLibSaved config.vizType


-- | Helper for visualizing the data with different chart types
visualizationTabs_ :: Maybe Text -> Bool -> Maybe Text -> Bool -> Html ()
visualizationTabs_ vizTypeM updateUrl widgetContainerId alert =
  div_ [class_ "tabs tabs-box tabs-outline tabs-xs bg-fillWeak p-1 rounded-lg", id_ "visualizationTabs", role_ "tablist"] do
    let defaultVizType = if alert then "viz-timeseries" else fromMaybe "logs" vizTypeM
        containerSelector = fromMaybe "visualization-widget-container" widgetContainerId
    forM_ visTypes $ \(icon, label, vizType, emoji) -> label_ [class_ "tab !shadow-none !border-strokeWeak flex gap-1"] do
      input_
        $ [ type_ "radio"
          , name_ "visualization"
          , id_ $ "viz-" <> vizType
          , value_ vizType
          , term "data-update-url" (if updateUrl then "true" else "false")
          , term "data-container-id" containerSelector
          , [__| on change
                      if my.checked
                        call updateVizTypeInUrl(my.value, @data-update-url === 'true')
                        set widgetJSON.type to my.value
                        send 'update-widget' to #{@data-container-id}
                      end
                   |]
          ]
        <> [checked_ | vizType == defaultVizType]
      span_ [class_ "text-iconNeutral leading-none"] $ toHtml emoji
      span_ [] $ toHtml label


-- | Query library component for saved and recent queries
queryLibrary_ :: Projects.ProjectId -> V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
queryLibrary_ pid queryLibSaved queryLibRecent = div_ [class_ "dropdown dropdown-bottom dropdown-start", id_ "queryLibraryParentEl"] do
  div_
    [class_ "cursor-pointer relative text-textWeak rounded-lg border border-strokeStrong h-full flex gap-2 items-center px-2 mb-2", tabindex_ "0", role_ "button"]
    (toHtml "Presets" >> faSprite_ "chevron-down" "regular" "w-3 h-3")
  div_ [class_ "dropdown-content z-20"] $ div_ [class_ "tabs tabs-box tabs-md tabs-outline items-center bg-fillWeak p-0 h-full", role_ "tablist", id_ "queryLibraryTabListEl"] do
    tabPanel_ "Saved" (queryLibraryContent_ "Saved" queryLibSaved)
    tabPanel_ "Recent" (queryLibraryContent_ "Recent" queryLibRecent)
  where
    tabPanel_ :: Text -> Html () -> Html ()
    tabPanel_ label content = do
      input_ $ [type_ "radio", name_ "querylib", role_ "tab", class_ "tab", Aria.label_ label] <> [checked_ | label == "Saved"]
      div_ [role_ "tabpanel", class_ "tab-content bg-bgBase shadow-lg rounded-box h-full max-h-[60dvh] w-[40vw] space-y-2 overflow-y-scroll"] content

    queryLibraryContent_ :: Text -> V.Vector Projects.QueryLibItem -> Html ()
    queryLibraryContent_ label items = do
      searchBar_ label
      div_ [class_ $ "border divide-y rounded-xl p-3 dataLibContent" <> label] $ V.forM_ items queryLibItem_

    searchBar_ :: Text -> Html ()
    searchBar_ label = div_ [class_ "flex gap-2 sticky top-0 p-3 bg-bgBase z-20"] do
      label_ [class_ "input input-md flex items-center gap-2 flex-1"] do
        faSprite_ "magnifying-glass" "regular" "h-4 w-4 opacity-70"
        input_
          [ type_ "text"
          , class_ "grow"
          , placeholder_ "Search"
          , term "data-filterParent" $ "dataLibContent" <> label
          , [__|on keyup
                 if the event's key is 'Escape' set my value to '' then trigger keyup
                 else show <.group/> in .{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()|]
          ]
      when (label == "Saved") do
        label_ [class_ "tabs tabs-md tabs-box tabs-outline bg-fillWeak text-textInverse-weak shrink items-center", role_ "tablist"] do
          input_ [class_ "hidden", type_ "checkbox", id_ "queryLibraryGroup"]
          div_ [role_ "tab", class_ "tab h-full bg-fillWeaker group-has-[#queryLibraryGroup:checked]/pg:bg-transparent", term "data-tippy-content" "My Queries"] $ faSprite_ "user" "solid" "w-5 h-5"
          div_ [role_ "tab", class_ "tab h-full group-has-[#queryLibraryGroup:checked]/pg:bg-fillWeaker", term "data-tippy-content" "All team Queries"] $ faSprite_ "users" "solid" "w-5 h-5"


-- | Visualization types used across the application
-- Each entry is (icon, label, type, emoji)
visTypes :: [(Text, Text, Text, Text)]
visTypes =
  [ ("list-view", "Logs", "logs", "📋")
  , ("bar-chart", "Bar", "timeseries", "📊")
  , ("duo-line-chart", "Line", "timeseries_line", "📈")
  -- , ("duo-pie-chart", "Pie", "pie_chart", "🥧")
  -- , ("duo-scatter-chart", "Scatter", "distribution", "📉")
  -- , ("hashtag", "Number", "stat", "🔢")
  -- , ("guage", "Guage", "", "🧮")
  -- , ("text", "Text", "", "📝")
  ]


-- | Individual query library item rendering
queryLibItem_ :: Projects.QueryLibItem -> Html ()
queryLibItem_ qli =
  div_ [class_ $ "clear p-3 space-y-2 hover:bg-fillWeaker cursor-pointer group " <> if qli.byMe then "" else "hidden group-has-[#queryLibraryGroup:checked]/pg:block"] do
    div_ [class_ "inline-flex gap-2 float-right"] do
      div_ [class_ "flex opacity-0 transition-opacity duration-300 group-hover:opacity-100 gap-2"] do
        a_
          [ class_ "tooltip"
          , term "data-tip" "run query"
          , term "data-query" qli.queryText
          , [__| on click call #filterElement.handleAddQuery({detail: JSON.parse(@data-query)})|]
          ]
          $ faSprite_ "play" "regular" "h-4 w-4"
        a_ [class_ "tooltip", term "data-tip" "copy query to clipboard", [__|install Copy(content: (next <.queryText/> ))|]] $ faSprite_ "copy" "regular" "h-4 w-4"
        when qli.byMe $ a_ [class_ "tooltip", term "data-tip" "edit query title", [__|on click set #queryLibId.value to @data-queryId then set #saveQueryMdl.checked to true|], term "data-queryId" qli.id.toText] $ faSprite_ "pen-to-square" "regular" "h-4 w-4"
        when qli.byMe
          $ a_
            [ class_ "tooltip"
            , term "data-tip" "delete query"
            , hxGet_ $ "/p/" <> qli.projectId.toText <> "/log_explorer?layout=DeleteQuery&queryLibId=" <> qli.id.toText
            , hxVals_ "js:{query:window.getQueryFromEditor()}"
            , hxTarget_ "#queryLibraryTabListEl"
            , hxSwap_ "outerHTML"
            , hxSelect_ "#queryLibraryTabListEl"
            , hxPushUrl_ "false"
            ]
          $ faSprite_ "trash-can" "regular" "h-4 w-4"
      label_ [class_ ""] do
        input_ [class_ "hidden", type_ "checkbox"]
        span_ [class_ ""] $ faSprite_ "ellipsis-vertical" "regular" "h-4 w-4"
        ul_ [class_ "hidden peer-checked:block z-30"] do
          li_ "Send query to alert"
          li_ "Send query to a dashboard"
    strong_ $ whenJust qli.title toHtml
    pre_
      $ code_ [class_ "language-js bg-transparent! queryText whitespace-pre-wrap break-words"]
      $ toHtml qli.queryText
    div_ [class_ "gap-3 flex"] $ time_ [datetime_ "", term "data-tippy-content" "created on"] (toHtml $ displayTimestamp $ formatUTC qli.createdAt) >> when qli.byMe " by me"


-- | Initialization code for the query editor that sets up schema data, query library, and popular searches
queryEditorInitializationCode :: V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Maybe Text -> Html ()
queryEditorInitializationCode queryLibRecent queryLibSaved vizTypeM = do
  let queryLibData = queryLibRecent <> queryLibSaved
      queryLibDataJson = decodeUtf8 $ AE.encode queryLibData
      schemaJson = decodeUtf8 $ AE.encode Schema.telemetrySchemaJson
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
          history.replaceState({}, '', url);
        }
        
        // Call the query editor's handleVisualizationChange method to update the query
        if (editor?.handleVisualizationChange) {
          const vizTypeMap = { 'bar': 'timeseries', 'line': 'timeseries_line' };
          editor.handleVisualizationChange(vizTypeMap[vizType] || vizType);
        }
      });
    };
    
    document.addEventListener('DOMContentLoaded', () => {
      const editor = document.getElementById('filterElement');
      if (editor) {
        if (editor.setQueryLibrary) {
          const queryLibraryData = $queryLibDataJson;
          editor.setQueryLibrary(queryLibraryData);
        }
        
        if (window.schemaManager && window.schemaManager.setSchemaData) {
          const schemaData = $schemaJson;
          window.schemaManager.setSchemaData('spans', schemaData);
          
          // Force refresh field suggestions in the query-builder component
          // This makes sure any field inputs get the latest schema data
          const queryBuilder = document.querySelector('query-builder');
          if (queryBuilder && typeof queryBuilder.refreshFieldSuggestions === 'function') {
            queryBuilder.refreshFieldSuggestions();
          }
        }
        
        if (editor.setPopularSearches) {
          const popularQueries = $popularQueriesJson;
          editor.setPopularSearches(popularQueries);
        }
      }
    });
    |]
