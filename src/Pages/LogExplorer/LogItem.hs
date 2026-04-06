module Pages.LogExplorer.LogItem (
  expandAPIlogItemH,
  ApiItemDetailed (..),
  expandedItemView,
  getServiceName,
  getServiceColor,
  getRequestDetails,
  spanHasErrors,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (isSpace)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Labeled (labeled)
import Effectful.PostgreSQL (WithConnection)
import Effectful.Reader.Static qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (atMapText)
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.Components (dateTime)
import Pkg.DeriveUtils (encodeEnumSC, unAesonTextMaybe)
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils


getServiceName :: Maybe (Map Text AE.Value) -> Text
getServiceName rs = case Map.lookup "service" (fromMaybe Map.empty rs) of
  Just (AE.Object o) -> case KEM.lookup "name" o of
    Just (AE.String s) -> s
    _ -> "Unknown"
  _ -> "Unknown"


getServiceColor :: Text -> HashMap Text Text -> Text
getServiceColor s serviceColors = fromMaybe "bg-fillNeutral-strong" $ HM.lookup s serviceColors


getRequestDetails :: Maybe (Map Text AE.Value) -> Maybe (Text, Text, Text, Int)
getRequestDetails spanRecord = do
  m <- spanRecord
  case Map.lookup "http" m of
    Just (AE.Object o) ->
      Just
        ( "HTTP"
        , case KEM.lookup "request" o of
            Just (AE.Object r) -> getText "method" r
            _ -> ""
        , case getUrl o of
            Just u -> u
            Nothing -> case Map.lookup "url" m of
              Just (AE.Object p) -> fromMaybe "/" $ getUrl p
              _ -> "/"
        , case KEM.lookup "response" o of
            Just (AE.Object r) -> getStatus r
            _ -> 0
        )
    _ -> case Map.lookup "rpc" m of
      Just (AE.Object o) -> Just ("GRPC", getText "service" o, getText "method" o, getStatus o)
      _ -> case Map.lookup "db" m of
        Just (AE.Object o) -> Just ("DB", getText "system" o, if T.null query then statement else query, getStatus o)
          where
            statement = getText "statement" o
            query = getText "query" o
        _ -> Nothing
  where
    getText :: Text -> AE.Object -> Text
    getText key v = case KEM.lookup (AEKey.fromText key) v of
      Just (AE.String s) -> s
      _ -> ""
    getInt :: Text -> AE.Object -> Maybe Int
    getInt key v = case KEM.lookup (AEKey.fromText key) v of
      Just (AE.Number n) -> toBoundedInteger n
      Just (AE.String s) -> readMaybe $ toString s
      _ -> Nothing
    getUrl :: AE.Object -> Maybe Text
    getUrl v =
      let opts = [getText "route" v, getText "path" v, getText "url" v, getText "target" v]
       in viaNonEmpty head $ Relude.filter (not . T.null) opts
    getStatus :: AE.Object -> Int
    getStatus v = fromMaybe 0 $ getInt "status_code" v


spanHasErrors :: Telemetry.SpanRecord -> Bool
spanHasErrors spanRecord = case spanRecord.events of
  AE.Array a ->
    let hasExceptionEvent event = case event of
          AE.Object obj -> KEM.lookup "event_name" obj == Just (AE.String "exception")
          _ -> False
     in Relude.any hasExceptionEvent (V.toList a)
  _ -> False


getSpanErrors :: AE.Value -> [AE.Value]
getSpanErrors evs = case evs of
  AE.Array a ->
    let events = V.toList a
        hasExceptionEvent :: AE.Value -> Bool
        hasExceptionEvent event = case event of
          AE.Object obj -> KEM.lookup "event_name" obj == Just (AE.String "exception")
          _ -> False
     in Relude.filter hasExceptionEvent events
  _ -> []


getErrorDetails :: AE.Value -> (Text, Text, Text)
getErrorDetails ae = case ae of
  AE.Object obj -> case KEM.lookup "event_attributes" obj of
    Just (AE.Object jj) -> case KEM.lookup "exception" jj of
      Just (AE.Object j) -> (fromMaybe "" $ getText "type" j, fromMaybe "" $ getText "message" j, fromMaybe "" $ getText "stacktrace" j)
      _ -> ("", "", "")
    _ -> ("", "", "")
  _ -> ("", "", "")
  where
    getText :: Text -> AE.Object -> Maybe Text
    getText key v = case KEM.lookup (AEKey.fromText key) v of
      Just (AE.String s) -> Just s
      Just vl -> Just $ show vl
      _ -> Nothing


expandAPIlogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ApiItemDetailed)
expandAPIlogItemH pid rdId timestamp sourceM = do
  _ <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  -- sourceM parameter is preserved for future use but not used in current logic
  -- Query the unified table using timestamp and id
  item <-
    if authCtx.env.enableTimefusionReads
      then labeled @"timefusion" @WithConnection $ Telemetry.logRecordByProjectAndId pid timestamp rdId
      else Telemetry.logRecordByProjectAndId pid timestamp rdId

  case item of
    Just record -> do
      -- Determine if this is a log or span based on the kind field
      case record.kind of
        Just "log" -> addRespHeaders $ LogItemExpanded pid record
        _ -> do
          -- It's a span, check if we need to fetch the apitoolkit-http-span
          aptSpan <- case getRequestDetails (unAesonTextMaybe record.attributes) of
            Just ("HTTP", _, _, _) -> do
              let trIdM = record.context >>= (.trace_id)
              -- Skip lookup if this span itself is an HTTP span
              if (record.name /= Just "apitoolkit-http-span")
                && (record.name /= Just "monoscope.http")
                && isNothing (atMapText "http.request.method" (unAesonTextMaybe record.attributes))
                then case trIdM of
                  Just trId ->
                    if authCtx.env.enableTimefusionReads
                      then labeled @"timefusion" @WithConnection $ Telemetry.spanRecordByName pid trId "monoscope.http"
                      else Telemetry.spanRecordByName pid trId "monoscope.http"
                  _ -> pure Nothing
                else pure Nothing
            _ -> pure Nothing
          addRespHeaders $ SpanItemExpanded pid record aptSpan
    Nothing -> addRespHeaders $ ItemDetailedNotFound "Record not found"


data ApiItemDetailed
  = SpanItemExpanded Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Telemetry.OtelLogsAndSpans)
  | LogItemExpanded Projects.ProjectId Telemetry.OtelLogsAndSpans
  | ItemDetailedNotFound Text


instance ToHtml ApiItemDetailed where
  toHtml (SpanItemExpanded pid spn aptSpan) = toHtml $ expandedItemView pid spn aptSpan Nothing Nothing
  toHtml (LogItemExpanded pid req) = toHtml $ expandedItemView pid req Nothing Nothing Nothing
  toHtml (ItemDetailedNotFound message) = div_ [class_ "flex flex-col items-center justify-center gap-3 py-12 px-4 text-center"] do
    faSprite_ "circle-exclamation" "regular" "w-8 h-8 text-iconNeutral opacity-60"
    h3_ [class_ "text-sm font-semibold text-textStrong"] "Record not found"
    p_ [class_ "text-xs text-textWeak max-w-xs"] $ toHtml message
    button_
      [ class_ "btn btn-sm btn-ghost text-xs"
      , Aria.label_ "Close details panel"
      , closePanelScript
      ]
      "Close"
  toHtmlRaw = toHtml


spanBadge :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
spanBadge pid path val key = do
  div_
    [ class_ "relative min-w-0"
    , term "data-field-path" path
    , term "data-field-value" $ "\"" <> T.dropAround isSpace (fromMaybe val (viaNonEmpty last (T.splitOn ":" val))) <> "\""
    ]
    do
      button_
        [ class_ "relative cursor-pointer flex gap-2 items-center text-textStrong bg-fillWeaker border border-strokeWeak text-xs rounded-lg whitespace-nowrap px-2 py-1 max-w-64"
        , term "data-tippy-content" $ key <> ": " <> val
        , [__|install LogItemMenuable|]
        ]
        $ do
          span_ [class_ "truncate"] $ toHtml val


-- Hyperscript to close the detail panel and clean up URL state
closePanelScript :: Attribute
closePanelScript =
  [__|on click add .hidden to #trace_expanded_view
      then put '0px' into #log_details_container.style.width
      then put '100%' into #logs_list_container.style.width
      then add .hidden to #resizer-details_width-wrapper
      then add .opacity-0 to #resizer-details_width-wrapper
      then add .pointer-events-none to #resizer-details_width-wrapper
      then remove .details-open from #log_details_container
      then remove .bg-fillBrand-strong from <.item-row.bg-fillBrand-strong/>
      then call updateUrlState('details_width', '', 'delete')
      then call updateUrlState('target_event', '0px', 'delete')
      then call updateUrlState('showTrace', '', 'delete')
      |]


-- Mobile-only variant: no resizer/trace cleanup needed
closePanelMobileScript :: Attribute
closePanelMobileScript =
  [__|on click remove .details-open from #log_details_container
      then put '0px' into #log_details_container.style.width
      then put '100%' into #logs_list_container.style.width
      then remove .bg-fillBrand-strong from <.item-row.bg-fillBrand-strong/>
      then call updateUrlState('target_event', '0px', 'delete')
      |]


-- Unified view for both logs and spans
expandedItemView :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> Maybe Telemetry.OtelLogsAndSpans -> Maybe Text -> Maybe Text -> Html ()
expandedItemView pid item aptSp leftM rightM = do
  let isLog = item.kind == Just "log"
      isAlert = item.kind == Just "alert"
      reqDetails = if isLog then Nothing else getRequestDetails (unAesonTextMaybe item.attributes)
  -- Mobile-only sticky back button (visible only on small screens when detail panel is full-screen)
  div_ [class_ "hidden max-md:flex sticky top-0 z-10 bg-bgBase border-b border-strokeWeak px-3 py-2.5 items-center"] do
    button_
      [ class_ "cursor-pointer flex items-center gap-1.5 text-sm font-medium text-textBrand"
      , Aria.label_ "Close details"
      , closePanelMobileScript
      ]
      do
        faSprite_ "chevron-left" "regular" "w-3.5 h-3.5"
        "Back to logs"
  div_ [class_ $ "w-full pl-3 pr-1 pb-2 relative border-l border-strokeWeak max-md:border-l-0 max-md:px-0" <> if isLog then " flex flex-col gap-2" else " pb-[50px]"] $ do
    div_ [id_ "copy_share_link"] pass
    unless isLog $ htmxOverlayIndicator_ "loading-span-list"
    htmxOverlayIndicator_ "details_indicator"
    div_ [class_ "flex flex-col gap-1.5 bg-fillWeaker py-2.5 px-3"] $ do
      -- Row 1: severity/title + date + close (compact for logs)
      div_ [class_ "flex justify-between items-start gap-2"] do
        if isLog
          then do
            let svTxt = maybe "UNSET" (\x -> maybe "UNSET" (toText . encodeEnumSC @"SL") x.severity_text) item.severity
                cls = getSeverityColor svTxt
                bodyText = case unAesonTextMaybe item.body of
                  Just (AE.String x) -> x
                  Just (AE.Object v) -> case extractMessageFromLog (AE.Object v) of
                    Just v' -> v'
                    _ -> toStrict $ encodeToLazyText (unAesonTextMaybe item.body)
                  _ -> toStrict $ encodeToLazyText (unAesonTextMaybe item.body)
            div_ [class_ "flex items-center gap-2 min-w-0"] do
              span_ [class_ $ "rounded-lg border cbadge-sm text-xs px-1.5 py-0.5 shrink-0 " <> cls] $ toHtml $ T.toUpper svTxt
              h4_ [class_ "text-textStrong truncate text-sm font-medium"] $ toHtml bodyText
          else if isAlert
            then div_ [class_ "flex items-center gap-3"] do
              h4_ [class_ "text-xl max-w-96 truncate"] $ toHtml $ fromMaybe "" item.name
              div_ [class_ "flex flex-wrap items-center gap-2"] do
                let strCls = getAlertStatusColor $ fromMaybe "" item.status_message
                span_ [class_ $ "badge badge-sm whitespace-nowrap " <> strCls] $ toHtml $ fromMaybe "" item.status_message
            else div_ [class_ "flex items-center gap-3 text-sm font-medium text-textStrong"] $ do
              case reqDetails of
                Just req -> do
                  div_ [class_ "flex flex-wrap items-center gap-2"] do
                    whenJust reqDetails $ \case
                      ("HTTP", method, path, status) -> do
                        span_ [class_ $ "relative cbadge-sm badge-" <> method <> " whitespace-nowrap", term "data-tip" ""] $ toHtml method
                        span_ [class_ $ "relative cbadge-sm badge-" <> T.take 1 (show status) <> "xx whitespace-nowrap", term "data-tip" ""] $ toHtml $ T.take 3 $ show status
                        let displayPath = if T.length path <= 1 then fromMaybe path item.name else path
                        div_ [class_ "flex items-center"] do
                          span_ [class_ "shrink-1 px-2 py-1.5 max-w-96 truncate mr-2 urlPath"] $ toHtml displayPath
                          div_ [[__| install Copy(content:.urlPath )|], Aria.label_ "Copy URL to clipboard", role_ "button", tabindex_ "0"] do
                            faSprite_ "copy" "regular" "h-8 w-8 border border-strokeWeak bg-fillWeakerer rounded-full p-2 text-iconNeutral"
                      (scheme, method, path, status) -> do
                        div_ [class_ "flex flex-wrap items-center"] do
                          span_ [class_ "flex gap-2 items-center text-textStrong bg-fillWeaker border border-strokeWeak rounded-lg whitespace-nowrap px-2 py-1"] $ toHtml method
                          span_ [class_ "px-2 py-1.5 max-w-96"] $ toHtml path
                          let extraClass = getGrpcStatusColor status
                          when (scheme /= "DB") $ span_ [class_ $ " px-2 py-1.5 border-l " <> extraClass] $ toHtml $ show status
                Nothing -> do
                  h4_ [class_ "text-xl max-w-96 truncate"] $ toHtml $ fromMaybe "" item.name
        div_ [class_ "flex gap-2 items-center shrink-0"] $ do
          dateTime (if isLog then item.timestamp else item.start_time) Nothing
          button_
            [ class_ "cursor-pointer detail-close-btn rounded-md p-1 hover:bg-fillWeak transition-colors"
            , Aria.label_ "Close item details"
            , closePanelScript
            ]
            do
              faSprite_ "xmark" "regular" "w-3.5 h-3.5 text-iconNeutral"
      -- Row 2: metadata badges
      div_ [class_ "flex gap-2 flex-wrap min-w-0"] $ do
        unless (isLog || isAlert) $ do
          spanBadge pid "duration" (toText $ getDurationNSMS $ maybe 0 fromIntegral item.duration) "Span duration"
          spanBadge pid "kind" (fromMaybe "" item.kind) "Span Kind"
        spanBadge pid "resource___service___name" (getServiceName (unAesonTextMaybe item.resource)) "Service"
        whenJust (item.context >>= (.span_id) >>= guarded (not . T.null)) $ \v ->
          spanBadge pid "context___span_id" ("Span ID: " <> v) "Span ID"
        whenJust (item.context >>= (.trace_id) >>= guarded (not . T.null)) $ \v ->
          spanBadge pid "context___trace_id" ("Trace ID: " <> v) "Trace ID"
      let actBtn = [class_ "cursor-pointer flex items-center gap-1.5 text-xs font-medium px-2 py-1 rounded-md border border-strokeWeak bg-fillWeakerer hover:bg-fillWeaker transition-colors text-textBrand"]
      div_ [class_ "flex flex-wrap gap-2 items-center"] do
        whenJust (atMapText "session.id" (unAesonTextMaybe item.attributes)) \v ->
          button_ (actBtn <> [term "data-field-path" "attributes.session.id", term "data-field-value" ("\"" <> v <> "\""), onclick_ "filterByField(event, 'Eq')"]) do
            faSprite_ "filter" "regular" "w-3 h-3"
            "Filter by session"

        unless isLog $ whenJust reqDetails $ \case
          ("HTTP", _, _, _) -> do
            let json = decodeUtf8 $ AE.encode $ AE.toJSON item
            button_ (actBtn <> [onclick_ "window.buildCurlRequest(event)", term "data-reqjson" json]) do
              faSprite_ "copy" "regular" "w-3 h-3"
              "Copy as curl"
          _ -> pass
        let createdAt = formatUTC item.timestamp
        whenJust (item.context >>= (.trace_id) >>= guarded (not . T.null)) $ \trId -> do
            let tracePath = "/p/" <> pid.toText <> "/traces/" <> trId <> "/?timestamp=" <> createdAt
            button_
              ( actBtn
                  <> [ term
                         "_"
                         [text|on click remove .hidden from #trace_expanded_view
                            then call updateUrlState('showTrace', "$trId/?timestamp=$createdAt")
                            then set #trace_expanded_view.innerHTML to #loader-tmp.innerHTML
                            then fetch $tracePath
                            then set #trace_expanded_view.innerHTML to it
                            then htmx.process(#trace_expanded_view)
                            then _hyperscript.processNode(#trace_expanded_view) then window.evalScriptsFromContent(#trace_expanded_view)|]
                     ]
              )
              do
                faSprite_ "cross-hair" "regular" "w-3 h-3"
                "View trace"
        let item_id = item.id
        let eventType = if isLog then "log" else "span"
        let monitorId = fromMaybe "" item.parent_id
        when isAlert $ a_ (actBtn <> [href_ $ "/p/" <> pid.toText <> "/monitors/" <> monitorId <> "/overview"]) do
          faSprite_ "bell" "regular" "w-3 h-3"
          "View alert"
        button_
          ( actBtn
              <> [ hxPost_ $ "/p/" <> pid.toText <> "/share/" <> item_id <> "/" <> createdAt <> "?event_type=" <> eventType
                 , hxSwap_ "innerHTML"
                 , hxTarget_ "#copy_share_link"
                 ]
          )
          do
            faSprite_ "link-simple" "regular" "w-3 h-3"
            "Share link"

    let tabContainerId = if isLog then "log-tabs-container" else "span-tabs-container"
    div_ [class_ "w-full mt-3", id_ tabContainerId] do
      let spanErrors = if isLog then [] else getSpanErrors $ fromMaybe AE.Null (unAesonTextMaybe item.events)
          isHttp = case reqDetails of
            Just ("HTTP", _, _, _) -> True
            _ -> False
          borderClass = "border-b-strokeWeak"
      let tabCls extra = "http-tab cursor-pointer border-b-2 " <> borderClass <> " px-4 py-1.5 text-sm " <> extra
          tabBtn cls target label = button_ [class_ $ tabCls cls, onclick_ $ "navigatable(this, '#" <> target <> "', '#" <> tabContainerId <> "', 't-tab-active', 'http')"] label
      div_ [class_ "flex", [__|on click halt|]] $ do
        when ((isLog || isAlert) && isJust item.body) $ tabBtn "t-tab-active" "body-content" "Body"
        when isHttp $ tabBtn "t-tab-active" "request-content" "Request"
        let attActive = if (isLog && isNothing item.body) || (not isLog && not isHttp) then "t-tab-active" else ""
        unless isAlert $ tabBtn attActive "att-content" "Attributes"
        tabBtn "" "meta-content" "Process"
        unless (isLog || null spanErrors) $ button_ [class_ $ tabCls "flex items-center gap-1", onclick_ $ "navigatable(this, '#errors-content', '#" <> tabContainerId <> "', 't-tab-active', 'http')"] do
          "Errors"
          div_ [class_ "badge badge-error badge-sm"] $ show $ length spanErrors
        unless isLog $ button_ [class_ $ tabCls "flex items-center gap-1", onclick_ $ "navigatable(this, '#logs-content', '#" <> tabContainerId <> "', 't-tab-active','http')"] $ do
          "Logs"
          div_ [class_ "badge badge-ghost badge-sm"] $ show $ numberOfEvents $ fromMaybe AE.Null (unAesonTextMaybe item.events)
        tabBtn "whitespace-nowrap" "m-raw-content" "Raw data"
        div_ [class_ $ "w-full border-b-2 " <> borderClass] pass

      div_ [class_ "mt-2 py-1 text-textWeak"] $ do
        when (isLog || isAlert) $ whenJust item.body \b ->
          div_ [class_ "http-tab-content", id_ "body-content"]
            $ jsonValueToHtmlTree (AE.toJSON b) Nothing
        div_ [class_ "hidden http-tab-content", id_ "m-raw-content"] $ do
          jsonValueToHtmlTree (AE.toJSON item) Nothing
        unless isAlert $ div_ [class_ $ "http-tab-content " <> if (isLog && isNothing item.body) || (not isLog && not isHttp) then "" else "hidden", id_ "att-content"] $ do
          case unAesonTextMaybe item.attributes of
            Just m | not (null m) -> jsonValueToHtmlTree (AE.Object $ KEM.fromMapText m) $ Just "attributes"
            _ -> div_ [class_ "text-sm text-textWeak italic py-4"] "No custom attributes on this entry"
        div_ [class_ "hidden http-tab-content", id_ "meta-content"] $ do
          jsonValueToHtmlTree (maybe (AE.object []) (AE.Object . KEM.fromMapText) (unAesonTextMaybe item.resource)) $ Just "resource"
        unless isLog $ do
          div_ [class_ "hidden http-tab-content w-full whitespace-wrap", id_ "errors-content"] $ do
            renderErrors spanErrors
          div_ [class_ "hidden http-tab-content", id_ "logs-content"] $ do
            jsonValueToHtmlTree (AE.toJSON (unAesonTextMaybe item.events)) Nothing

        unless isLog $ whenJust reqDetails $ \case
          ("HTTP", method, path, status) -> do
            let cSp = fromMaybe item aptSp
            div_ [class_ "http-tab-content nested-tab", id_ "request-content"] do
              div_ [id_ "http-content-container", class_ "flex flex-col gap-3 mt-2"] do
                div_ [class_ "bg-fillWeaker w-max rounded-lg border border-strokeWeak justify-start items-start inline-flex"] do
                  div_ [class_ "justify-start items-start flex text-sm"] do
                    let httpTab target label cls = button_ [onclick_ $ "navigatable(this, '#" <> target <> "', '#http-content-container', 't-tab-box-active')", class_ $ "http cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak" <> cls] label
                    httpTab "res_content" "Res Body" " t-tab-box-active"
                    httpTab "req_content" "Req Body" ""
                    httpTab "hed_content" "Headers" ""
                    httpTab "par_content" "Params" ""
                    httpTab "raw_content" "Request Details" ""
                div_ [] do
                  div_ [id_ "raw_content", class_ "hidden a-tab-content http"] do
                    jsonValueToHtmlTree (AE.toJSON cSp) Nothing
                  div_ [id_ "req_content", class_ "hidden a-tab-content http"] do
                    let b = case unAesonTextMaybe cSp.body of
                          Just (AE.Object bb) -> case KEM.lookup "request_body" bb of
                            Just a -> a
                            _ -> AE.object []
                          _ -> AE.object []
                    jsonValueToHtmlTree b $ Just "body.request_body"
                  div_ [id_ "res_content", class_ "a-tab-content http"] do
                    let b = case unAesonTextMaybe cSp.body of
                          Just (AE.Object bb) -> case KEM.lookup "response_body" bb of
                            Just a -> a
                            _ -> AE.object []
                          _ -> AE.object []
                    jsonValueToHtmlTree b $ Just "body.response_body"
                  div_ [id_ "hed_content", class_ "hidden a-tab-content http"] do
                    let reqHeaders = case unAesonTextMaybe cSp.attributes >>= Map.lookup "http" of
                          Just (AE.Object httpAtts) -> case KEM.lookup "request" httpAtts of
                            Just (AE.Object reqAtts) -> KEM.lookup "header" reqAtts
                            _ -> Nothing
                          _ -> Nothing
                        resHeaders = case unAesonTextMaybe cSp.attributes >>= Map.lookup "http" of
                          Just (AE.Object httpAtts) -> case KEM.lookup "response" httpAtts of
                            Just (AE.Object resAtts) -> KEM.lookup "header" resAtts
                            _ -> Nothing
                          _ -> Nothing
                    jsonValueToHtmlTree (AE.object ["request_headers" AE..= fromMaybe AE.Null reqHeaders, "response_headers" AE..= fromMaybe AE.Null resHeaders]) Nothing
                  div_ [id_ "par_content", class_ "hidden a-tab-content http"] do
                    let queryParams = case unAesonTextMaybe cSp.attributes >>= Map.lookup "http" of
                          Just (AE.Object httpAtts) -> case KEM.lookup "request" httpAtts of
                            Just (AE.Object reqAtts) -> KEM.lookup "query_params" reqAtts
                            _ -> Nothing
                          _ -> Nothing
                        pathParams = case unAesonTextMaybe cSp.attributes >>= Map.lookup "http" of
                          Just (AE.Object httpAtts) -> case KEM.lookup "request" httpAtts of
                            Just (AE.Object reqAtts) -> KEM.lookup "path_params" reqAtts
                            _ -> Nothing
                          _ -> Nothing
                    jsonValueToHtmlTree (AE.object ["query_params" AE..= fromMaybe AE.Null queryParams, "path_params" AE..= fromMaybe AE.Null pathParams]) Nothing
          _ -> pass


-- Helper functions
renderErrors :: [AE.Value] -> Html ()
renderErrors errs =
  div_ [class_ "flex flex-col mt-4 gap-4 w-full"]
    $ forM_ errs
    $ \err -> do
      let (tye, message, stacktrace) = getErrorDetails err
      div_ [class_ "w-full border border-strokeWeak rounded-lg"] $ do
        div_ [class_ "p-3"] do
          div_ [class_ "text-textError font-semibold text-sm mb-1"] $ toHtml tye
          p_ [class_ "text-sm leading-snug text-textWeak"] $ toHtml message
        unless (T.null stacktrace) $ details_ [class_ "group/st border-t border-strokeWeak"] do
          summary_ [class_ "cursor-pointer select-none flex items-center gap-1.5 px-3 py-2 text-xs font-medium text-textWeak hover:text-textStrong"] do
            faSprite_ "chevron-right" "regular" "w-3 h-3 transition-transform group-open/st:rotate-90"
            "Stack Trace"
          div_ [class_ "px-3 pb-3"]
            $ pre_ [class_ "text-xs font-mono whitespace-pre-wrap text-textWeak bg-fillWeak rounded-md p-3 max-h-64 overflow-y-auto"]
            $ toHtml stacktrace


numberOfEvents :: AE.Value -> Int
numberOfEvents (AE.Array obj) = length obj
numberOfEvents _ = 0
