module Pages.Log where

import Config
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.ByteString.Lazy qualified as BS
import Data.Default (def)
import Data.Digest.XXHash
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (
  UTCTime,
  ZonedTime,
  addUTCTime,
  getCurrentTime,
  secondsToNominalDiffTime,
  utc,
  utcToZonedTime,
 )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import Data.Vector (Vector, iforM_)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Base
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Lucid.Svg (use_)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Numeric (showHex)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pages.NonMember
import Pkg.Components (loader)
import Relude

import Data.Time.Clock (UTCTime)
import Data.Time.Format
import System.Clock
import Utils
import Witch (from)


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


parseTimestamp :: String -> Maybe ZonedTime
parseTimestamp = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%qZ"


iso8601ToZoned :: Text -> Maybe ZonedTime
iso8601ToZoned val = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ val) :: Maybe UTCTime)


apiLog :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
apiLog sess pid queryM cols' cursorM sinceM fromM toM hxRequestM hxBoostedM = do
  let cols = T.splitOn "," (fromMaybe "" cols')
  let query = fromMaybe "" queryM
  let cursorM' = case cursorM of
        Nothing -> Nothing
        Just "" -> Nothing
        Just a -> Just a
  now <- liftIO getCurrentTime
  let (fromD, toD, currentRange) = case sinceM of
        Just "1H" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime 3600) now, Just $ utcToZonedTime utc now, Just "Last Hour")
        Just "24H" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24) now, Just $ utcToZonedTime utc now, Just "Last 24 Hours")
        Just "7D" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 7) now, Just $ utcToZonedTime utc now, Just "Last 7 Days")
        Just "14D" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 14) now, Just $ utcToZonedTime utc now, Just "Last 14 Days")
        Nothing -> do
          let f = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromM) :: Maybe UTCTime)
          let t = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toM) :: Maybe UTCTime)
          let start = toText . formatTime defaultTimeLocale "%F %T" <$> f
          let end = toText . formatTime defaultTimeLocale "%F %T" <$> t
          let range = case (start, end) of
                (Just s, Just e) -> Just (s <> "-" <> e)
                _ -> Nothing
          (f, t, range)
        _ -> do
          let f = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromM) :: Maybe UTCTime)
          let t = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toM) :: Maybe UTCTime)
          let start = toText . formatTime defaultTimeLocale "%F %T" <$> f
          let end = toText . formatTime defaultTimeLocale "%F %T" <$> t
          let range = case (start, end) of
                (Just s, Just e) -> Just (s <> "-" <> e)
                _ -> Nothing
          (f, t, range)
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, dbResp) <- liftIO
        $ withPool pool do
          project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
          dbResp <- RequestDumps.selectRequestDumpByProject pid query cursorM' fromD toD
          _ <- RequestDumps.selectLogTable pid query (iso8601ToZoned =<< cursorM') (fromD, toD)
          pure (project, dbResp)

      reqChartTxt <- liftIO $ withPool pool $ RequestDumps.throughputBy pid Nothing Nothing Nothing Nothing Nothing (3 * 60) Nothing queryM (fromD, toD)
      let (requests, resultCount) = dbResp
          reqLastCreatedAtM = (^. #createdAt) <$> viaNonEmpty last (Vector.toList requests) -- FIXME: unoptimal implementation, converting from vector to list for last
          cursorTempM = toText . formatTime defaultTimeLocale "%F %T" <$> reqLastCreatedAtM
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' cursorTempM sinceM fromM toM

      case (hxRequestM, hxBoostedM) of
        (Just "true", Nothing) -> pure $ do
          span_ [id_ "result-count", hxSwapOob_ "outerHTML"] $ show resultCount
          reqChart reqChartTxt True
          logItemRows pid requests cols nextLogsURL
        _ -> do
          let bwconf =
                (def :: BWConfig)
                  { sessM = Just sess
                  , currProject = project
                  , pageTitle = "API Log Explorer"
                  }
          let resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing Nothing Nothing Nothing
          pure $ bodyWrapper bwconf $ apiLogsPage pid resultCount requests cols reqChartTxt nextLogsURL resetLogsURL currentRange




timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("1H", "Last Hour")
  , ("24H", "Last 24 Hours")
  , ("7D", "Last 7 days")
  , ("14D", "Last 14 days")
  ]


apiLogsPage :: Projects.ProjectId -> Int -> Vector RequestDumps.RequestDumpLogItem -> [Text] -> Text -> Text -> Text -> Maybe Text -> Html ()
apiLogsPage pid resultCount requests cols reqChartTxt nextLogsURL resetLogsURL currentRange = do
  section_ [class_ "mx-auto px-10 py-2 gap-2 flex flex-col h-[98%] overflow-hidden "] do
    div_
      [ style_ "z-index:26; width: min(90vw, 800px)"
      , class_ "fixed hidden right-0 bg-white overflow-y-scroll h-[calc(100%-60px)] border-l border-l-2 shadow"
      , id_ "expand-log-modal"
      ]
      do
        div_ [class_ "relative ml-auto w-full", style_ ""] do
          div_ [class_ "flex justify-end  w-full p-4 "] do
            button_ [[__|on click add .hidden to #expand-log-modal|]] do
              img_ [class_ "h-8", src_ "/assets/svgs/close.svg"]
          let postP = "/p/" <> pid.toText <> "/share/"
          form_
            [ hxPost_ postP
            , hxSwap_ "innerHTML"
            , hxTarget_ "#copy_share_link"
            , id_ "share_log_form"
            ]
            do
              input_ [type_ "hidden", value_ "1 hour", name_ "expiresIn", id_ "expire_input"]
              input_ [type_ "hidden", value_ "", name_ "reqId", id_ "req_id_input"]
          div_ [id_ "log-modal-content-loader", class_ "bg-white rounded-lg shadow p-4 absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2"] do
            loader
          div_ [class_ "px-2", id_ "log-modal-content"] pass
    form_
      [ class_ "card-round w-full text-sm"
      , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
      , hxPushUrl_ "true"
      , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, cols:params().cols}"
      , hxTarget_ "#log-item-table-body"
      , id_ "log_explorer_form"
      , hxIndicator_ "#query-indicator"
      ]
      do
        nav_ [class_ "flex flex-row p-2 content-end justify-between items-baseline border-slate-100"] do
          a_ [class_ "inline-block"] "Query"
          div_ [class_ "flex gap-10 items-center"] do
            div_ [class_ "relative p-1 "] do
              div_ [class_ "relative"] do
                input_ [type_ "hidden", id_ "since_input"]
                input_ [type_ "hidden", id_ "custom_range_input"]
                a_
                  [ class_ "relative px-3 py-2 border border-1 border-black-200 space-x-2 bg-blue-100 text-blue-500  inline-block relative cursor-pointer rounded-md"
                  , [__| on click toggle .hidden on #timepickerBox|]
                  ]
                  do
                    mIcon_ "clock" "h-4 w-4"
                    span_ [class_ "inline-block", id_ "currentRange"] $ toHtml (fromMaybe "Last 14 Days" currentRange)
                    faIcon_ "fa-chevron-down" "fa-light fa-chevron-down" "h-4 w-4 inline-block"
                div_ [id_ "timepickerBox", class_ "hidden absolute z-10 mt-1  rounded-md flex"] do
                  div_ [class_ "inline-block w-84 overflow-auto bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm"] do
                    timePickerItems
                      & mapM_ \(val, title) ->
                        a_
                          [ class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 "
                          , term "data-value" val
                          , term "data-title" title
                          , [__| on click set #custom_range_input's value to my @data-value then log my @data-value 
                                         then toggle .hidden on #timepickerBox 
                                         then set #currentRange's innerText to my @data-title
                                         then htmx.trigger("#log_explorer_form", "submit")|]
                          ]
                          $ toHtml title
                    a_ [class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 ", [__| on click toggle .hidden on #timepickerSidebar |]] "Custom date range"
                  div_ [class_ "inline-block relative hidden", id_ "timepickerSidebar"] do
                    div_ [id_ "startTime", class_ "hidden"] ""
            div_
              [class_ "flex items-center gap-2"]
              do
                label_ [class_ "relative inline-flex items-center cursor-pointer"] do
                  input_ [type_ "checkbox", value_ "", class_ "sr-only peer", id_ "toggleQueryEditor", onclick_ "toggleQueryBuilder()"]
                  div_ [class_ "w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 dark:peer-focus:ring-blue-800 rounded-full peer dark:bg-gray-700 peer-checked:after:translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:left-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all dark:border-gray-600 peer-checked:bg-blue-600"] pass
                  span_ [class_ "ml-3 text-sm font-medium text-gray-900 dark:text-gray-300"] "Use editor"
            button_
              [type_ "submit", class_ "cursor-pointer inline-block space-x-1 bg-blue-100 hover:bg-blue-200 blue-800 py-1 px-2 rounded-lg"]
              do
                faIcon_ "fa-sparkles" "fa-sharp fa-regular fa-sparkles" "h-3 w-3 inline-block"
                span_ "Run query"
        div_ do
          div_ [class_ "bg-gray-200"] do
            div_ [id_ "queryEditor", class_ "h-14 hidden overflow-hidden bg-gray-200"] pass
          div_ [id_ "queryBuilder", class_ "mb-4"] do
            termRaw "filter-element" [id_ "filterElement"] ("" :: Text)

    div_ [class_ "card-round w-full grow divide-y flex flex-col text-sm h-full overflow-y-hidden overflow-x-hidden"] do
      div_ [class_ "pl-3 py-1 space-x-5 flex flex-row justify-between"] do
        a_ [class_ "cursor-pointer inline-block pr-3 space-x-2 bg-blue-50 hover:bg-blue-100 blue-800 p-1 rounded-md", [__|on click toggle .hidden on #reqsChartParent|]] do
          faIcon_ "fa-chart-bar" "fa-regular fa-chart-bar" "h-3 w-3 inline-block"
          span_ [] "toggle chart"
      reqChart reqChartTxt False
      div_ [class_ "pl-3 py-2 space-x-5 flex flex-row justify-between"] do
        div_ [class_ "inline-block space-x-3"] do
          strong_ "Query results"
          span_ [class_ "space-x-1"] do
            span_ [id_ "result-count"] $ show resultCount
            span_ " log entries"
        a_
          [ class_ "cursor-pointer inline-block pr-3 space-x-2"
          , hxGet_ resetLogsURL
          , hxTarget_ "#log-item-table-body"
          , hxSwap_ "innerHTML scroll:#log-item-table-body:top"
          , hxIndicator_ "#query-indicator"
          ]
          do
            svg_ [class_ "w-4 h-4 icon text-slate-500 inline-block"] $ use_ [href_ "/assets/svgs/sprite/sprite.svg#refresh"]
            span_ [] "refresh"

      jsonTreeAuxillaryCode pid
      div_ [class_ "table-fixed grow w-full min-w-full h-full divide-y flex flex-col monospace overflow-hidden"] do
        div_ [class_ "text-xs bg-gray-100 gray-400"] do
          div_ [class_ "flex flex-row text-left space-x-4"] do
            span_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-8"] ""
            span_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-36"] "TIMESTAMP"
            span_ [class_ "font-normal inline-block py-1.5 p-1 px-2 grow"] "SUMMARY"
        div_ [class_ "htmx-indicator query-indicator", id_ "query-indicator"] do
          loader
        div_ [class_ "grow overflow-y-scroll h-full whitespace-nowrap text-sm divide-y overflow-x-hidden", id_ "log-item-table-body"] do
          logItemRows pid requests cols nextLogsURL
  script_
    [text|
    const picker = new easepick.create({
      element: '#startTime',
      css: [
        'https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.css',
      ],
      inline: true,
      plugins: ['RangePlugin', 'TimePlugin'],
      autoApply: false,
      setup(picker) {
        picker.on('select', (e) => {
          const start = JSON.stringify(e.detail.start).slice(1, -1);
          const end = JSON.stringify(e.detail.end).slice(1, -1);
          const rangeInput = document.getElementById("custom_range_input")
          rangeInput.value = start + "/" + end
          document.getElementById("timepickerBox").classList.toggle("hidden")
          document.getElementById("currentRange").innerText = start.split("T")[0] + " - " + end.split("T")[0]
          htmx.trigger("#log_explorer_form", "submit")
        });
      },
    });
    window.picker = picker;

function changeTab(tabId, parent) {
  const p = document.getElementById(parent);
  const tabLinks = p.querySelectorAll('.sdk_tab');
  tabLinks.forEach(link => link.classList.remove('sdk_tab_active'));
  const clickedTabLink = document.getElementById(tabId);
  clickedTabLink.classList.add('sdk_tab_active')
  const tabContents = p.querySelectorAll('.sdk_tab_content');
  tabContents.forEach(content => {
    content.classList.add("hidden")
    content.classList.remove ("sdk_tab_content_active")
  });
  const tabContent = document.getElementById(tabId + '_json');
  tabContent.classList.remove("hidden")
  setTimeout(()=>{tabContent.classList.add("sdk_tab_content_active")},10)
}

function toggleExpireOptions (event) {
    event.preventDefault()
    event.stopPropagation()
    const container = document.querySelector('#expire_container')
    if(container) {
     container.classList.toggle('hidden')
    }
}

function getShareLink(event) {
  event.target.innerText = "Generating..."
  const reqId = event.target.getAttribute ("data-req-id")
  document.querySelector('#req_id_input').value = reqId
  htmx.trigger('#share_log_form','submit')
}

function expireChanged(event) {
    event.preventDefault()
    event.stopPropagation()
    const current = document.querySelector('#toggle_expires_btn')
    if(current && current.firstChild) {
       current.firstChild.innerText = "Expires in: " + event.target.getAttribute("data-expire-value")
       document.querySelector("#expire_input").value = event.target.getAttribute("data-expire-value")
    }
}
  document.getElementById("log_explorer_form").addEventListener("keydown", function (e) {
            if (e.key === "Enter") {
                e.preventDefault();
            }
        });
  |]
reqChart :: Text -> Bool -> Html ()
reqChart reqChartTxt hxOob = do
  div_ [id_ "reqsChartParent", class_ "p-5", hxSwapOob_ $ show hxOob] do
    div_ [id_ "reqsChartsEC", class_ "", style_ "height:100px"] ""
    script_
      [text| throughputEChart("reqsChartsEC", $reqChartTxt, [], true) |]


logItemRows :: Projects.ProjectId -> Vector RequestDumps.RequestDumpLogItem -> [Text] -> Text -> Html ()
logItemRows pid requests cols nextLogsURL = do
  requests & traverse_ \req -> do
    let logItemPath = RequestDumps.requestDumpLogItemUrlPath pid req
    let endpoint_hash = toText $ showHex (xxHash $ encodeUtf8 $ UUID.toText pid.unProjectId <> req.host <> T.toUpper req.method <> req.urlPath) ""
    let logItemEndpointUrl = "/p/" <> pid.toText <> "/log_explorer/endpoint/" <> endpoint_hash
    let errorClass = if req.errorsCount > 0 then "w-1 bg-red-500" else "bg-transparent"
    let (requestTypeHtml_, requestTypeHover_) =
          if show req.requestType == "Outgoing"
            then (faIcon_ "fa-arrow-up-right" "fa-solid fa-arrow-up-right" "h-3 w-3 text-green-400", "Outgoing request")
            else (faIcon_ "fa-arrow-down-left" "fa-solid fa-arrow-down-left" "h-3 w-3 text-gray-400", "Incoming request")
    div_
      [ class_ "flex flex-row divide-x  cursor-pointer "
      , term "data-log-item-path" logItemPath
      , [__|on click LogItemExpandable(me)|]
      ]
      do
        div_ [class_ "flex-none inline-block w-10 flex justify-start items-center"] do
          a_ [class_ $ "inline-block h-full " <> errorClass, term "data-tippy-content" $ show req.errorsCount <> " errors attached to this request"] ""
          button_
            [ class_ "ml-1 expand-button"
            , term "data-log-item-path" (logItemPath <> "/detailed")
            , [__|on click halt the event then remove .hidden from #expand-log-modal then
                  remove .hidden from #log-modal-content-loader
                  fetch `${@data-log-item-path}` as html then put it into #log-modal-content
                  add .hidden to #log-modal-content-loader
                  _hyperscript.processNode(document.querySelector('#log-modal-content'))
                  htmx.process(document.querySelector('#log-modal-content'))
                  end
            |]
            ]
            do
              faSprite_ "link" "solid" "h-3 w-3 text-blue-500"
          faSprite_ "chevron-right" "solid" "h-2 w-2 ml-2"
        div_ [class_ "flex-none inline-block p-1 px-2 w-36 overflow-hidden"] $ toHtml @String $ formatTime defaultTimeLocale "%F %T" (req ^. #createdAt)
        div_ [class_ "flex items-center p-1 px-2 grow"] do
          span_ [class_ "w-3 text-center mr-2", term "data-tippy-content" requestTypeHover_] requestTypeHtml_
          let reqJSON = AE.toJSON req
          let colValues = concatMap (\col -> findValueByKeyInJSON (T.splitOn "." col) reqJSON) cols
          -- FIXME: probably inefficient implementation and should be optimized
          forM_ (zip cols colValues) \(col, colValue) ->
            div_ [class_ "relative inline-block log-item-field-parent", term "data-field-path" col] do
              a_
                [ class_ "cursor-pointer mx-1 inline-block bg-blue-100 hover:bg-blue-200 text-blue-900 px-3 rounded-lg monospace log-item-field-anchor log-item-field-value"
                , term "data-field-path" col
                , term "data-field-value" colValue
                , [__|install LogItemMenuable|]
                ]
                $ toHtml colValue
          let cls = "mx-1 inline-block px-3 rounded-lg monospace min-w-[4rem] text-center " :: Text
          span_ [class_ $ cls <> getMethodColor req.method] $ toHtml $ req.method
          a_
            [ hxGet_ logItemEndpointUrl
            , term "data-tippy-content" "Go to endpoint"
            , onclick_ "noPropa(event)"
            , class_ $ cls <> " bg-gray-100 border border-gray-300 "
            ]
            $ toHtml req.urlPath
          span_ [class_ $ cls <> getStatusColor req.statusCode] $ show req.statusCode
          span_ [class_ $ cls <> " bg-gray-50 border border-gray-300 "] $ toHtml req.rawUrl
          let reqBody = decodeUtf8 $ AE.encode req.requestBody
          let respBody = decodeUtf8 $ AE.encode req.responseBody
          p_ [class_ "inline-block"] $ toHtml $ T.take 300 [text| request_body=$reqBody response_body=$respBody|]

    div_ [class_ "hidden w-full flex px-2 py-8 justify-center item-loading"] do
      loader
  when (Vector.length requests > 199) $ a_
    [ class_ "cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center"
    , hxTrigger_ "click"
    , hxSwap_ "outerHTML"
    , hxGet_ nextLogsURL
    ]
    do
      div_ [class_ "htmx-indicator query-indicator"] do
        loader
      "LOAD MORE"


-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Html ()
jsonTreeAuxillaryCode pid = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu text-sm origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-white ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none z-10", role_ "menu", tabindex_ "-1"] do
      div_ [class_ "py-1", role_ "none"] do
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-0"
          , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
          , hxPushUrl_ "true"
          , hxVals_ "js:{query:params().query,cols:toggleColumnToSummary(event)}"
          , hxSwap_ "innerHTML scroll:#log-item-table-body:top"
          , hxTarget_ "#log-item-table-body"
          , hxIndicator_ "#query-indicator"
          , [__|init
                  set fp to (closest @data-field-path)
                  if isFieldInSummary(fp) then set my innerHTML to 'Remove field from summary' end|]
          ]
          "Add field to Summary"
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-1"
          , [__|on click 
                  if 'clipboard' in window.navigator then 
                    call navigator.clipboard.writeText((previous <.log-item-field-value/>)'s innerText)
                    send successToast(value:['Value has been added to the Clipboard']) to <body/>
                    halt
                  end|]
          ]
          "Copy field value"
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-2"
          , onclick_ "filterByField(event, '=')"
          ]
          "Filter by field"
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-3"
          , onclick_ "filterByField(event, '!=')"
          ]
          "Exclude field"
  script_
    [type_ "text/hyperscript"]
    [text|
      behavior LogItemMenuable
        on click
          if I match <.with-context-menu/> then
            remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu />
          else
            remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu /> then
            get #log-item-context-menu-tmpl.innerHTML then put it after me then add .with-context-menu to me then 
            _hyperscript.processNode(document.querySelector('.log-item-context-menu'))
            htmx.process(document.querySelector('.log-item-context-menu'))
          end
          halt
        end
      end

      def LogItemExpandable(me)
          if I match <.expanded-log/> then 
            remove next <.log-item-info/> then 
            remove .expanded-log from me
          else
            add .expanded-log to me
            remove .hidden from next <.item-loading />
            fetch `$${@data-log-item-path}` as html then put it after me then
             add .hidden to next <.item-loading />
            _hyperscript.processNode(next <.log-item-info />) then
          end 
      end
    |]

  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.js"] ("" :: Text)
  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/javascript/javascript.min.js"] ("" :: Text)

  script_
    [text|
    function downloadJson(event) {
         event.stopPropagation()
         const json = event.currentTarget.dataset.reqjson
         var blob = new Blob([json], { type: "application/json" });
         var a = document.createElement("a");
         a.href = URL.createObjectURL(blob);
         a.download = "request-data-" + (new Date().toString()) + ".json";
         a.textContent = "";
         document.body.appendChild(a);
         a.click();
         document.body.removeChild(a); 
       }

    function filterByField(event, operation) {
       const target = event.target.parentNode.parentNode.parentNode
       const path = target.getAttribute('data-field-path');
       const value = target.getAttribute('data-field-value');
       const filter = path + ' ' + operation + ' ' + value
       let editorVal = '' 
       if(window.queryBuilderValue) {
        editorVal = window.queryBuilderValue
        }else if(window.editor) {
           editorVal = window.editor.getValue()
        }
       let newVal = ''
       if (editorVal.toLowerCase().endsWith("and") || editorVal.toLowerCase().endsWith("or")) {
           newVal = editorVal + " " + filter
       }else {
        if (editorVal.length == 0) {
            newVal = filter
          }else {
            newVal = editorVal + " AND " + filter
          }
       }
       if (newVal != "") {
          const filterComp = document.querySelector('#filterElement')
          if(filterComp) {
               filterComp.setBuilderValue(newVal)
            }
          if(window.editor) {
             window.editor.setValue(newVal)
            }
          htmx.trigger("#log_explorer_form", "submit")
        }
    }

    var params = () => new Proxy(new URLSearchParams(window.location.search), {
      get: (searchParams, prop) => searchParams.get(prop)??"",
    });
    function noPropa(event) {
      event.stopPropagation();
    }
    var toggleColumnToSummary = (e)=>{
      const cols = (params().cols??"").split(",").filter(x=>x!="");
      const subject = e.target.closest('.log-item-field-parent').dataset.fieldPath; 
      if (cols.includes(subject)) {
        return [...new Set(cols.filter(x=>x!=subject))].join(",");
      } 
      cols.push(subject)
      const new_cols = [... new Set (cols)].join (",")
      return new_cols
    }
    var isFieldInSummary = field => params().cols.split(",").includes(field);
    
    var getQueryFromEditor = () => {
     const toggler = document.getElementById("toggleQueryEditor")
     if(toggler.checked) {
          return window.editor.getValue();
      }else {
          return window.queryBuilderValue || "";
      }
    }

    function getTimeRange () {
      const rangeInput = document.getElementById("custom_range_input")
      const range = rangeInput.value.split("/")
      if (range.length == 2)  {
         return {from: range[0], to: range[1], since: ''}
        }
      return {since: range[0], from: '', to: ''}
    }

    function toggleQueryBuilder() {
     document.getElementById("queryBuilder").classList.toggle("hidden")
     document.getElementById("queryEditor").classList.toggle("hidden")
     if(!window.editor) {
        var codeMirrorEditor = CodeMirror(document.getElementById('queryEditor'), {
          value: params().query || window.queryBuilderValue || '',
          mode:  "javascript",
          theme: "elegant",
          lineNumbers: true,
        });
        window.editor = codeMirrorEditor
     }
    }

    var execd = false
    document.addEventListener('DOMContentLoaded', function(){
      window.setQueryBuilderFromParams()
    })
    |]

  style_
    [text|
    .tree-children {
      display: block;
    }
    .tree-children-count { display: none; }
    .collapsed .tree-children {
      display: none !important; 
    }
    .CodeMirror {
      font-family: Arial, monospace;
      font-size: 16px;
      background-color: rgb(243 244 246);
     }
    .collapsed .tree-children-count {display: inline !important;}
    .collapsed .children {display: inline-block; padding-left:0}
    .collapsed .closing-token {padding-left:0}
  |]


-- >>> findValueByKeyInJSON ["key1", "key2"] [aesonQQ|{"kx":0, "key1":{"key2":"k2val"}}|]
-- ["\"k2val\""]
-- >>> findValueByKeyInJSON ["key1", "[]", "key2"] [aesonQQ|{"kx":0, "key1":[{"key2":"k2val"}]}|]
-- ["\"k2val\""]
findValueByKeyInJSON :: [Text] -> AE.Value -> [Text]
findValueByKeyInJSON (x : path) (AE.Object obj) = concatMap (\(_, v) -> findValueByKeyInJSON path v) (AEK.toHashMapText obj & HM.toList & filter (\(k, _) -> k == x))
findValueByKeyInJSON ("[]" : path) (AE.Array vals) = concatMap (findValueByKeyInJSON path) (Vector.toList vals)
findValueByKeyInJSON [] value = [unwrapJsonPrimValue value]
findValueByKeyInJSON _ _ = ["_"]


-- findValueByKeyInJSON _ _ = error "findValueByKeyInJSON: case should be unreachable"

unwrapJsonPrimValue :: AE.Value -> Text
unwrapJsonPrimValue (AE.Bool True) = "true"
unwrapJsonPrimValue (AE.Bool False) = "true"
unwrapJsonPrimValue (AE.String v) = "\"" <> toText v <> "\""
unwrapJsonPrimValue (AE.Number v) = toText @String $ show v
unwrapJsonPrimValue AE.Null = "null"
unwrapJsonPrimValue (AE.Object _) = "{..}"
unwrapJsonPrimValue (AE.Array items) = "[" <> show (length items) <> "]"

-- unwrapJsonPrimValue (AE.Object _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only. got object" -- should never be reached
-- unwrapJsonPrimValue (AE.Array _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only. got array" -- should never be reached
