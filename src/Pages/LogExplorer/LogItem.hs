module Pages.LogExplorer.LogItem (expandAPIlogItemH, expandAPIlogItem', apiLogItemH, ApiLogItem (..), jsonValueToHtmlTree, ApiItemDetailed (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.ByteString.Lazy qualified as BS
import Data.Char (isDigit)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.UUID qualified as UUID
import Data.Vector (iforM_)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import Network.URI (escapeURIString, isUnescapedInURI)
import Pages.Components qualified as Components
import Pages.Traces.Spans qualified as Spans
import PyF (fmt)
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, getMethodColor, getStatusColor, unwrapJsonPrimValue)
import Witch (from)


expandAPIlogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ApiItemDetailed)
expandAPIlogItemH pid rdId createdAt sourceM = do
  _ <- Sessions.sessionAndProject pid
  let source = fromMaybe "requets" sourceM
  case source of
    "spans" -> do
      spanItem <- Telemetry.spanRecordByProjectAndId pid createdAt rdId
      addRespHeaders $ case spanItem of
        Just spn -> SpanItemExpanded pid spn
        Nothing -> ItemDetailedNotFound "Span not found"
    _ -> do
      logItemM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      addRespHeaders $ case logItemM of
        Just req -> LogItemExpanded pid req True
        Nothing -> ItemDetailedNotFound "Request not found"


expandAPIlogItem' :: Projects.ProjectId -> RequestDumps.RequestDumpLogItem -> Bool -> Html ()
expandAPIlogItem' pid req modal = do
  div_ [class_ "flex flex-col w-full gap-4 pb-[100px]"] do
    div_ [class_ "w-full flex flex-col gap-2 gap-4"] do
      let methodColor = getMethodColor req.method
      let statusColor = getStatusColor req.statusCode
      div_ [class_ "flex gap-4 items-center"] do
        div_ [class_ $ "font-semibold px-2 py-1 rounded min-w-[70px] text-center h-full " <> methodColor] $ toHtml req.method
        div_ [class_ $ "text-lg font-bold px-2 " <> statusColor] $ show req.statusCode
        div_ [class_ "flex border border-gray-200 m-1 rounded-xl p-2"] do
          faSprite_ "regular-calendar-days-clock" "regular" "h-4 mr-2 w-4"
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" req.createdAt
      when modal do
        div_
          [ class_ "flex gap-2 px-4 items-center border border-dashed h-[50px]"
          , id_ "copy_share_link"
          ]
          do
            div_ [class_ "relative", style_ "width:150px"] do
              button_
                [ [__|on click toggle .hidden on #expire_container|]
                , id_ "toggle_expires_btn"
                , class_ "w-full flex gap-2 text-slate-600 justify_between items-center cursor-pointer px-2 py-1 border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200"
                ]
                do
                  p_ [style_ "width: calc(100% - 25px)", class_ "text-sm truncate ..."] "Expires in: 1 hour"
                  faSprite_ "chevron-down" "regular" "h-3 w-3"
              div_ [id_ "expire_container", class_ "absolute hidden bg-base-100 border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] do
                forM_ (["1 hour", "8 hours", "1 day"] :: [Text]) \sw -> do
                  button_
                    [ [__|on click set #toggle_expires_btn.firstChild.innerText to 'Expires in ' + event.target's @data-expire-value
                                        then set #expire_input.value to event.target's @data-expire-value|]
                    , term "data-expire-value" sw
                    , class_ "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
                    ]
                    $ toHtml sw
            button_
              [ class_ "flex flex-col gap-1 bg-blue-500 px-2 py-1 rounded text-white"
              , term "data-req-id" (show req.id)
              , term "data-req-created-at" (toText $ formatTime defaultTimeLocale "%FT%T%6QZ" req.createdAt)
              , [__|on click set #req_id_input.value to my @data-req-id
                          then set #req_created_at_input.value to my @data-req-created-at
                          then call #share_log_form.requestSubmit() |]
              ]
              "Get link"

    -- url, endpoint, latency, request size, repsonse size
    div_ [class_ "flex flex-col mt-4 p-4 justify-between w-full rounded-xl border"] do
      div_ [class_ "text-lg mb-2"] do
        span_ [class_ "text-slate-500 font-semibold"] "Endpoint: "
        span_ [] $ toHtml req.urlPath
      div_ [class_ "text-lg"] do
        span_ [class_ "text-slate-500 font-semibold"] "URL: "
        span_ [] $ toHtml req.rawUrl
      div_ [class_ "flex gap-2 mt-4"] do
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            faSprite_ "clock" "regular" "h-4 w-4 text-slate-400"
            span_ [class_ "text-md font-bold"] $ show (req.durationNs `div` 1000) <> " ms"
          p_ [class_ "text-slate-500"] "Latency"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            faSprite_ "upload" "solid" "h-4 w-4 text-slate-400"
            let reqSize = BS.length $ AE.encode req.requestBody
            span_ [class_ "text-md font-bold"] $ show (reqSize - 2) <> " bytes"
          p_ [class_ "text-slate-500"] "Request size"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            faSprite_ "download" "solid" "h-4 w-4 text-slate-400"
            let respSize = BS.length $ AE.encode req.responseBody
            span_ [class_ "text-md font-bold"] $ show (respSize - 2) <> " bytes"
          p_ [class_ "text-slate-500"] "Response size"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            faSprite_ "layer-group" "solid" "h-5 w-5 text-slate-400"
            span_ [class_ "text-md font-bold"] $ show req.sdkType
          p_ [class_ "text-slate-500"] "Framework"
    -- errors
    when (req.errorsCount > 0) $ div_ [class_ "border rounded-lg mt-8"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 gap-4 items-center"] do
        p_ [class_ "font-bold"] "Errors"
        p_ [class_ "text-sm text-red-500 font-bold"] $ show req.errorsCount
      div_ [class_ "px-4 flex gap-10 border-b text-gray-500"] do
        jsonValueToHtmlTree req.errors

    -- outgoing request details
    div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
      p_ [class_ "font-bold"] "Outgoing requests"
    div_ [class_ "grow overflow-y-auto py-2 px-1 max-h-[500px] whitespace-nowrap text-sm divide-y overflow-x-hidden"] do
      let createdAt = toText $ formatTime defaultTimeLocale "%FT%T%6QZ" req.createdAt
      let escapedQueryPartial = toText $ escapeURIString isUnescapedInURI $ toString [fmt|parent_id=="{UUID.toText req.id}" AND created_at<="{createdAt}"|]
      let events_url = "/p/" <> pid.toText <> "/log_explorer?layout=resultTable&query=" <> escapedQueryPartial
      div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""

    -- request details
    div_ [class_ "border rounded-lg mt-8", id_ "request_detail_container"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Request Details"

      div_ [class_ "tabs tabs-bordered place-content-start ", role_ "tablist"] do
        input_ [type_ "radio", name_ $ "req-details-tabx-" <> show req.id, role_ "tab", Aria.label_ "Body", class_ "tab w-max", checked_]
        div_ [class_ "tab-content grow w-full", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4  p-2 rounded-lg border break-all", id_ "req_body_json"]
          $ jsonValueToHtmlTree req.requestBody

        input_ [type_ "radio", name_ $ "req-details-tabx-" <> show req.id, role_ "tab", Aria.label_ "Headers", class_ "tab"]
        div_ [class_ "tab-content grow w-full", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border break-all", id_ "req_headers_json"]
          $ jsonValueToHtmlTree req.requestHeaders

        input_ [type_ "radio", name_ $ "req-details-tabx-" <> show req.id, role_ "tab", Aria.label_ "Query Params", class_ "tab break-keep"]
        div_ [class_ "tab-content grow w-full", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border", id_ "query_params_json"]
          $ jsonValueToHtmlTree req.queryParams

        input_ [type_ "radio", name_ $ "req-details-tabx-" <> show req.id, role_ "tab", Aria.label_ "Path Params", class_ "tab break-keep"]
        div_ [class_ "tab-content grow w-full", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border", id_ "path_params_json"]
          $ jsonValueToHtmlTree req.pathParams

    -- response details
    div_ [class_ "border rounded-lg mt-8", id_ "reponse_detail_container"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Response Details"

      div_ [class_ "tabs tabs-bordered place-content-start grid grid-flow-col", role_ "tablist"] do
        input_ [type_ "radio", name_ "resp-details-tab", role_ "tab", Aria.label_ "Body", class_ "tab", checked_]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4  p-2 rounded-lg border", id_ "res_body_json"]
          $ jsonValueToHtmlTree req.responseBody

        input_ [type_ "radio", name_ "resp-details-tab", role_ "tab", Aria.label_ "Headers", class_ "tab"]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border", id_ "res_headers_json"]
          $ jsonValueToHtmlTree req.responseHeaders


apiLogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ApiLogItem)
apiLogItemH pid rdId createdAt sourceM = do
  let source = fromMaybe "requests" sourceM
  _ <- Sessions.sessionAndProject pid
  logItem <- case sourceM of
    Just "logs" -> do
      logItem <- Telemetry.logRecordByProjectAndId pid createdAt rdId
      pure $ AE.toJSON <$> logItem
    Just "spans" -> do
      spanItem <- Telemetry.spanRecordByProjectAndId pid createdAt rdId
      pure $ AE.toJSON <$> spanItem
    _ -> do
      logItemM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      pure $ selectiveReqToJson <$> logItemM
  addRespHeaders $ case logItem of
    Just req -> ApiLogItem rdId req (requestDumpLogItemUrlPath pid rdId createdAt) source
    Nothing -> ApiLogItemNotFound $ "Invalid " <> source <> " ID"


requestDumpLogItemUrlPath :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Text
requestDumpLogItemUrlPath pid rdId timestamp = "/p/" <> pid.toText <> "/log_explorer/" <> UUID.toText rdId <> "/" <> from @String (formatShow iso8601Format timestamp)


data ApiLogItem
  = ApiLogItem UUID.UUID AE.Value Text Text
  | ApiLogItemNotFound Text


instance ToHtml ApiLogItem where
  toHtml (ApiLogItem logId req expandItemPath source) = toHtml $ apiLogItemView logId req expandItemPath source
  toHtml (ApiLogItemNotFound message) = div_ [] $ toHtml message
  toHtmlRaw = toHtml


data ApiItemDetailed
  = LogItemExpanded Projects.ProjectId RequestDumps.RequestDumpLogItem Bool
  | SpanItemExpanded Projects.ProjectId Telemetry.SpanRecord
  | ItemDetailedNotFound Text
instance ToHtml ApiItemDetailed where
  toHtml (LogItemExpanded pid log_item is_modal) = toHtml $ expandAPIlogItem' pid log_item is_modal
  toHtml (SpanItemExpanded pid span_item) = toHtml $ Spans.expandedSpanItem pid span_item
  toHtml (ItemDetailedNotFound message) = div_ [] $ toHtml message
  toHtmlRaw = toHtml


apiLogItemView :: UUID.UUID -> AE.Value -> Text -> Text -> Html ()
apiLogItemView logId req expandItemPath source = do
  div_ [class_ "flex items-center gap-2"] do
    Components.drawerWithURLContent_
      ("expand-log-drawer-" <> UUID.toText logId)
      (expandItemPath <> "/detailed?source=" <> source)
      $ span_ [class_ "btn btn-sm btn-outline"] ("Expand" >> faSprite_ "expand" "regular" "h-3 w-3")
    let reqJson = decodeUtf8 $ AE.encode req
    when (source == "requests")
      $ button_
        [ class_ "btn btn-sm btn-outline"
        , term "data-reqJson" reqJson
        , onclick_ "window.buildCurlRequest(event)"
        ]
        (span_ [] "Copy as curl" >> faSprite_ "copy" "regular" "h-3 w-3")
    when (source == "spans")
      $ Components.drawerWithURLContent_
        ("expand-log-drawer-" <> UUID.toText logId)
        (expandItemPath <> "/detailed?source=" <> source)
      $ span_ [class_ "btn btn-sm btn-outline"] "Expan Trace"

    button_
      [ class_ "btn btn-sm btn-outline"
      , onclick_ "downloadJson(event)"
      , term "data-reqJson" reqJson
      ]
      (span_ [] "Download" >> faSprite_ "arrow-down-to-line" "regular" "h-3 w-3")
  jsonValueToHtmlTree req


-- Function to selectively convert RequestDumpLogItem to JSON
selectiveReqToJson :: RequestDumps.RequestDumpLogItem -> AE.Value
selectiveReqToJson req =
  AE.object
    $ concat @[]
      [ ["created_at" .= req.createdAt]
      , ["duration_ns" .= req.durationNs]
      , ["errors" .= req.errors]
      , ["host" .= req.host]
      , ["method" .= req.method]
      , ["parent_id" .= req.parentId]
      , ["path_params" .= req.pathParams]
      , ["query_params" .= req.queryParams]
      , ["raw_url" .= req.rawUrl]
      , ["referer" .= req.referer]
      , ["request_body" .= req.requestBody]
      , ["request_headers" .= req.requestHeaders]
      , ["request_type" .= req.requestType]
      , ["response_body" .= req.responseBody]
      , ["response_headers" .= req.responseHeaders]
      , ["sdk_type" .= req.sdkType]
      , ["service_version" .= req.serviceVersion]
      , ["status_code" .= req.statusCode]
      , ["tags" .= req.tags]
      , ["url_path" .= req.urlPath]
      ]


-- >>> replaceNumbers "response_body.0.completed"
-- "response_body[*].completed"
--
replaceNumbers :: Text -> Text
replaceNumbers input = T.replace ".[*]" "[*]" $ T.intercalate "." (map replaceDigitPart parts)
  where
    parts = T.splitOn "." input
    replaceDigitPart :: Text -> Text
    replaceDigitPart part
      | T.all isDigit part = "[*]"
      | otherwise = T.concatMap replaceDigitWithAsterisk part

    replaceDigitWithAsterisk :: Char -> Text
    replaceDigitWithAsterisk ch
      | isDigit ch = "[*]"
      | otherwise = one ch


-- | jsonValueToHtmlTree takes an aeson json object and renders it as a collapsible html tree, with hyperscript for interactivity.
jsonValueToHtmlTree :: AE.Value -> Html ()
jsonValueToHtmlTree val = jsonValueToHtmlTree' ("", "", val)
  where
    jsonValueToHtmlTree' :: (Text, Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (path, key, AE.Object v) = renderParentType "{" "}" key (length v) (AEK.toHashMapText v & HM.toList & sort & mapM_ (\(kk, vv) -> jsonValueToHtmlTree' (path <> "." <> key, kk, vv)))
    jsonValueToHtmlTree' (path, key, AE.Array v) = renderParentType "[" "]" key (length v) (iforM_ v \i item -> jsonValueToHtmlTree' (path <> "." <> key, show i, item))
    jsonValueToHtmlTree' (path, key, value) = do
      let fullFieldPath = if T.isSuffixOf "[*]" path then path else path <> "." <> key
      let fullFieldPath' = fromMaybe fullFieldPath $ T.stripPrefix ".." fullFieldPath
      div_
        [ class_ "relative log-item-field-parent"
        , term "data-field-path" $ replaceNumbers fullFieldPath'
        , term "data-field-value" $ unwrapJsonPrimValue value
        ]
        $ a_
          [class_ "block hover:bg-blue-50 cursor-pointer pl-6 relative log-item-field-anchor ", [__|install LogItemMenuable|]]
          do
            span_ $ toHtml key
            span_ [class_ "text-blue-800"] ":"
            span_ [class_ "text-blue-800 ml-2.5 log-item-field-value", term "data-field-path" fullFieldPath'] $ toHtml $ unwrapJsonPrimValue value

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    renderParentType opening closing key count child = div_ [class_ (if key == "" then "" else "collapsed")] do
      a_
        [ class_ "inline-block cursor-pointer"
        , onclick_ "this.parentNode.classList.toggle('collapsed')"
        ]
        do
          span_ [class_ "log-item-tree-chevron "] "▾"
          span_ [] $ toHtml $ if key == "" then opening else key <> ": " <> opening
      div_ [class_ "pl-5 children "] do
        span_ [class_ "tree-children-count"] $ show count
        div_ [class_ "tree-children"] child
      span_ [class_ "pl-5 closing-token"] $ toHtml closing
