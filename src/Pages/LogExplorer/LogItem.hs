module Pages.LogExplorer.LogItem (
  expandAPIlogItemH,
  ApiItemDetailed (..),
  expandedItemView,
  spanLatencyBreakdown,
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
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static qualified
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (atMapText)
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.Components (dateTime)
import Pkg.DeriveUtils (unAesonTextMaybe)
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
getServiceColor s serviceColors = fromMaybe "bg-black" $ HM.lookup s serviceColors


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
  _ <- Sessions.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  -- sourceM parameter is preserved for future use but not used in current logic
  -- Query the unified table using timestamp and id
  item <-
    if authCtx.env.enableTimefusionReads
      then labeled @"timefusion" @DB $ Telemetry.logRecordByProjectAndId pid timestamp rdId
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
              if (record.name /= Just "apitoolkit-http-span") && (record.name /= Just "monoscope.http")
                then case trIdM of
                  Just trId ->
                    if authCtx.env.enableTimefusionReads
                      then labeled @"timefusion" @DB $ Telemetry.spanRecordByName pid trId "monoscope.http"
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
  toHtml (ItemDetailedNotFound message) = div_ [] $ toHtml message
  toHtmlRaw = toHtml


spanBadge :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
spanBadge pid path val key = do
  div_
    [ class_ "relative"
    , term "data-field-path" path
    , term "data-field-value" $ "\"" <> T.dropAround isSpace (fromMaybe val (viaNonEmpty last (T.splitOn ":" val))) <> "\""
    ]
    do
      button_
        [ class_ "relative cursor-pointer flex gap-2 items-center text-textStrong bg-fillWeaker border border-strokeWeak text-xs rounded-lg whitespace-nowrap px-2 py-1"
        , term "data-tippy-content" key
        , [__|install LogItemMenuable|]
        ]
        $ do
          span_ [] $ toHtml val


-- Unified view for both logs and spans
expandedItemView :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> Maybe Telemetry.OtelLogsAndSpans -> Maybe Text -> Maybe Text -> Html ()
expandedItemView pid item aptSp leftM rightM = do
  let isLog = item.kind == Just "log"
      reqDetails = if isLog then Nothing else getRequestDetails (unAesonTextMaybe item.attributes)
  div_ [class_ $ "w-full pl-2 pb-2 relative" <> if isLog then " flex flex-col gap-2" else " pb-[50px]"] $ do
    div_ [class_ "flex justify-between items-center", id_ "copy_share_link"] pass
    unless isLog $ span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "loading-span-list"] ""
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
    div_ [class_ "flex flex-col gap-4 bg-fillWeaker py-2  px-2"] $ do
      div_ [class_ "flex justify-between items-center"] do
        div_ [class_ "flex items-center gap-4"] $ do
          h3_ [class_ "whitespace-nowrap font-semibold text-textStrong"] $ if isLog then "Trace Log" else "Trace Span"
        div_ [class_ "flex gap-4 items-center"] $ do
          dateTime (if isLog then item.timestamp else item.start_time) Nothing
          div_ [class_ "flex gap-2 items-center"] do
            button_
              [ class_ "cursor-pointer"
              , [__|on click add .hidden to #trace_expanded_view 
            then put '0px' into  #log_details_container.style.width 
            then put '100%' into #logs_list_container.style.width 
            then add .hidden to #resizer-details_width-wrapper
            then add .opacity-0 to #resizer-details_width-wrapper
            then add .pointer-events-none to #resizer-details_width-wrapper
            then remove .bg-fillBrand-strong from <.item-row.bg-fillBrand-strong/>
            then call updateUrlState('details_width', '', 'delete')
            then call updateUrlState('target_event', '0px', 'delete')
            then call updateUrlState('showTrace', '', 'delete')
            |]
              ]
              do
                faSprite_ "xmark" "regular" "w-3 h-3 text-textBrand"
    div_ [class_ "flex flex-col gap-4"] do
      if isLog
        then div_ [class_ "flex items-center gap-4"] do
          let svTxt = maybe "UNSET" (\x -> maybe "UNSET" show x.severity_text) item.severity
              cls = getSeverityColor svTxt
          span_ [class_ $ "rounded-lg border cbadge-sm text-sm px-2 py-1 shrink-0 " <> cls] $ toHtml $ T.toUpper svTxt
          h4_ [class_ "text-textStrong truncate "] $ toHtml $ case unAesonTextMaybe item.body of
            Just (AE.String x) -> x
            Just (AE.Object v) -> case extractMessageFromLog (AE.Object v) of
              Just v' -> v'
              _ -> toStrict $ encodeToLazyText (unAesonTextMaybe item.body)
            _ -> toStrict $ encodeToLazyText (unAesonTextMaybe item.body)
        else div_ [class_ "flex items-center gap-4 text-sm font-medium text-textStrong"] $ do
          case reqDetails of
            Just req -> do
              div_ [class_ "flex flex-wrap items-center gap-2"] do
                whenJust reqDetails $ \case
                  ("HTTP", method, path, status) -> do
                    span_ [class_ $ "relative cbadge-sm badge-" <> method <> " whitespace-nowrap", term "data-tip" ""] $ toHtml method
                    span_ [class_ $ "relative cbadge-sm badge-" <> T.take 1 (show status) <> "xx whitespace-nowrap", term "data-tip" ""] $ toHtml $ T.take 3 $ show status
                    div_ [class_ "flex items-center"] do
                      span_ [class_ "shrink-1 px-2 py-1.5 max-w-96 truncate mr-2 urlPath"] $ toHtml path
                      div_ [[__| install Copy(content:.urlPath )|]] do
                        faSprite_ "copy" "regular" "h-8 w-8 border border-strokeWeak bg-fillWeakerer rounded-full p-2 text-textWeak"
                  (scheme, method, path, status) -> do
                    div_ [class_ "flex flex-wrap items-center"] do
                      span_ [class_ "flex gap-2 items-center text-textStrong bg-fillWeaker border border-strokeWeak rounded-lg whitespace-nowrap px-2 py-1"] $ toHtml method
                      span_ [class_ "px-2 py-1.5 max-w-96"] $ toHtml path
                      let extraClass = getGrpcStatusColor status
                      when (scheme /= "DB") $ span_ [class_ $ " px-2 py-1.5 border-l " <> extraClass] $ toHtml $ show status
            Nothing -> do
              h4_ [class_ "text-xl max-w-96 truncate"] $ toHtml $ fromMaybe "" item.name

      div_ [class_ "flex gap-2 flex-wrap"] $ do
        unless isLog $ do
          spanBadge pid "duration" (toText $ getDurationNSMS $ maybe 0 fromIntegral item.duration) "Span duration"
          spanBadge pid "kind" (fromMaybe "" item.kind) "Span Kind"
        spanBadge pid "resource___service___name" (getServiceName (unAesonTextMaybe item.resource)) "Service"
        spanBadge pid "context___span_id" ("Span ID: " <> maybe "" (\c -> fromMaybe "" c.span_id) item.context) "Span ID"
        spanBadge pid "context___trace_id" ("Trace ID: " <> maybe "" (\z -> fromMaybe "" z.trace_id) item.context) "Trace ID"
      div_ [class_ "flex flex-wrap gap-3 items-center text-textBrand font-medium text-xs"] do
        whenJust (atMapText "session.id" (unAesonTextMaybe item.attributes)) $ \v -> do
          button_ [class_ "cursor-pointer flex items-center gap-1", term "data-field-path" "attributes.session.id", term "data-field-value" ("\"" <> v <> "\""), onpointerdown_ "filterByField(event, 'Eq')"] do
            "Filter by session"
            faSprite_ "filter" "regular" "w-3 h-3"

        unless isLog $ whenJust reqDetails $ \case
          ("HTTP", _, _, _) -> do
            let json = decodeUtf8 $ AE.encode $ AE.toJSON item
            button_
              [ class_ "cursor-pointer flex items-center gap-1"
              , term "onpointerdown" "window.buildCurlRequest(event)"
              , term "data-reqjson" json
              ]
              do
                "Copy request as curl"
                faSprite_ "copy" "regular" "w-3 h-3"
          _ -> pass
        let createdAt = formatUTC item.timestamp
        whenJust item.context $ \ctx -> do
          whenJust ctx.trace_id $ \trId -> do
            let tracePath = "/p/" <> pid.toText <> "/traces/" <> trId <> "/?timestamp=" <> createdAt
            button_
              [ class_ $ "cursor-pointer flex items-end gap-1" <> if isLog then " cursor-pointer" else ""
              , term
                  "_"
                  [text|on click remove .hidden from #trace_expanded_view
                            then call updateUrlState('showTrace', "$trId/?timestamp=$createdAt")
                            then set #trace_expanded_view.innerHTML to #loader-tmp.innerHTML
                            then fetch $tracePath
                            then set #trace_expanded_view.innerHTML to it
                            then htmx.process(#trace_expanded_view)
                            then _hyperscript.processNode(#trace_expanded_view) then window.evalScriptsFromContent(#trace_expanded_view)|]
              ]
              do
                "View parent trace"
                faSprite_ "cross-hair" "regular" "w-4 h-4"
        let item_id = item.id
        let eventType = if isLog then "log" else "span"
        button_
          [ class_ "cursor-pointer flex items-center gap-2"
          , hxPost_ $ "/p/" <> pid.toText <> "/share/" <> item_id <> "/" <> createdAt <> "?event_type=" <> eventType
          , hxSwap_ "innerHTML"
          , hxTarget_ "#copy_share_link"
          ]
          do
            "Generate shareable link"
            faSprite_ "link-simple" "regular" "w-3 h-3"

    let tabContainerId = if isLog then "log-tabs-container" else "span-tabs-container"
    div_ [class_ $ "w-full " <> if isLog then "mt-4" else "mt-8", id_ tabContainerId] do
      let spanErrors = if isLog then [] else getSpanErrors $ fromMaybe AE.Null (unAesonTextMaybe item.events)
          isHttp = case reqDetails of
            Just ("HTTP", _, _, _) -> True
            _ -> False
          borderClass = "border-b-strokeWeak"
      div_ [class_ "flex", [__|on click halt|]] $ do
        when (isLog && isJust item.body)
          $ button_ [class_ $ "a-tab cursor-pointer  border-b-2 " <> borderClass <> " px-4 py-1.5 t-tab-active", onpointerdown_ $ "navigatable(this, '#body-content', '#" <> tabContainerId <> "', 't-tab-active', '.http')"] "Body"
        when (not isLog && isHttp) $ button_ [class_ $ "a-tab cursor-pointer  border-b-2 " <> borderClass <> " px-4 py-1.5 t-tab-active", onpointerdown_ $ "navigatable(this, '#request-content', '#" <> tabContainerId <> "', 't-tab-active','.http')"] "Request"
        button_ [class_ $ "cursor-pointer a-tab border-b-2 " <> borderClass <> " px-4 py-1.5 " <> if (isLog && isNothing item.body) || (not isLog && not isHttp) then "t-tab-active" else "", onpointerdown_ $ "navigatable(this, '#att-content', '#" <> tabContainerId <> "', 't-tab-active'" <> if isLog then ")" else ",'.http')"] "Attributes"
        button_ [class_ $ "cursor-pointer a-tab border-b-2 " <> borderClass <> " px-4 py-1.5 ", onpointerdown_ $ "navigatable(this, '#meta-content', '#" <> tabContainerId <> "', 't-tab-active'" <> if isLog then ")" else ", '.http')"] "Process"
        unless (isLog || null spanErrors) $ do
          button_ [class_ $ "a-tab cursor-pointer border-b-2 " <> borderClass <> " flex items-center gap-1 nowrap px-4 py-1.5 ", onpointerdown_ $ "navigatable(this, '#errors-content', '#" <> tabContainerId <> "', 't-tab-active', '.http')"] do
            "Errors"
            div_ [class_ "badge badge-error badge-sm"] $ show $ length spanErrors
        unless isLog $ button_ [class_ $ "a-tab cursor-pointer border-b-2 " <> borderClass <> " flex items-center gap-1 px-4 py-1.5 ", onpointerdown_ $ "navigatable(this, '#logs-content', '#" <> tabContainerId <> "', 't-tab-active','.http')"] $ do
          "Logs"
          div_ [class_ "badge badge-ghost badge-sm"] $ show $ numberOfEvents $ fromMaybe AE.Null (unAesonTextMaybe item.events)
        button_ [class_ $ "a-tab cursor-pointer border-b-2 whitespace-nowrap " <> borderClass <> " px-4 py-1.5", onpointerdown_ $ "navigatable(this, '#m-raw-content', '#" <> tabContainerId <> "', 't-tab-active','.http')"] "Raw data"
        div_ [class_ $ "w-full border-b-2 " <> borderClass] pass

      div_ [class_ "my-4 py-2 text-textWeak"] $ do
        when isLog $ whenJust item.body $ \b -> do
          div_ [class_ "a-tab-content", id_ "body-content"] do
            jsonValueToHtmlTree (AE.toJSON b) Nothing
        div_ [class_ "hidden a-tab-content", id_ "m-raw-content"] $ do
          jsonValueToHtmlTree (AE.toJSON item) Nothing
        div_ [class_ $ "a-tab-content " <> if (isLog && isNothing item.body) || (not isLog && not isHttp) then "" else "hidden", id_ "att-content"] $ do
          jsonValueToHtmlTree (maybe (AE.object []) (AE.Object . KEM.fromMapText) (unAesonTextMaybe item.attributes)) $ Just "attributes"
        div_ [class_ "hidden a-tab-content", id_ "meta-content"] $ do
          jsonValueToHtmlTree (maybe (AE.object []) (AE.Object . KEM.fromMapText) (unAesonTextMaybe item.resource)) $ Just "resource"
        unless isLog $ do
          div_ [class_ "hidden a-tab-content w-full whitespace-wrap", id_ "errors-content"] $ do
            renderErrors spanErrors
          div_ [class_ "hidden a-tab-content", id_ "logs-content"] $ do
            jsonValueToHtmlTree (AE.toJSON (unAesonTextMaybe item.events)) Nothing

        unless isLog $ whenJust reqDetails $ \case
          ("HTTP", method, path, status) -> do
            let cSp = fromMaybe item aptSp
            div_ [class_ "a-tab-content nested-tab", id_ "request-content"] do
              div_ [id_ "http-content-container", class_ "flex flex-col gap-3 mt-2"] do
                div_ [class_ "bg-fillWeaker w-max rounded-lg border border-strokeWeak justify-start items-start inline-flex"] do
                  div_ [class_ "justify-start items-start flex text-sm"] do
                    button_ [onpointerdown_ "navigatable(this, '#res_content', '#http-content-container', 't-tab-box-active')", class_ "http cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak t-tab-box-active"] "Res Body"
                    button_ [onpointerdown_ "navigatable(this, '#req_content', '#http-content-container', 't-tab-box-active')", class_ "http cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Req Body"
                    button_ [onpointerdown_ "navigatable(this, '#hed_content', '#http-content-container', 't-tab-box-active')", class_ "http cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Headers"
                    button_ [onpointerdown_ "navigatable(this, '#par_content', '#http-content-container', 't-tab-box-active')", class_ "http cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Params"
                    button_ [onpointerdown_ "navigatable(this, '#raw_content', '#http-content-container', 't-tab-box-active')", class_ "http cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Request Details"
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
      div_ [class_ "w-full max-w-2xl mx-auto  border border-border rounded-lg shadow-sm"] $ do
        -- Card Header
        div_ [class_ "p-3 pb-3"] do
          div_ [class_ "text-textError font-semibold mb-2"] $ toHtml tye
          div_ [class_ "flex items-start justify-between gap-4"] do
            div_ [class_ "flex-1 min-w-0"] $ do
              h3_ [class_ "text-base text-balance text-sm leading-tight"] $ toHtml message
        div_ [class_ "px-3 pb-6 space-y-4"] $ do
          div_ [class_ "border-b border-border "] $ do
            button_ [class_ "w-full flex justify-between items-center py-2 text-sm font-medium text-left rounded"] "Stack Trace"
            div_ [id_ "stackTraceContent", class_ "rounded bg-fillWeak p-2"]
              $ div_ [class_ "bg-muted p-3 rounded-md max-h-64 overflow-y-auto mb-4"]
              $ pre_ [class_ "text-xs monospace whitespace-pre-wrap text-muted-foreground"]
              $ toHtml stacktrace


numberOfEvents :: AE.Value -> Int
numberOfEvents (AE.Array obj) = length obj
numberOfEvents _ = 0


-- Span latency breakdown visualization
spanLatencyBreakdown :: V.Vector Telemetry.SpanRecord -> Html ()
spanLatencyBreakdown spans = do
  let colors = getServiceColors $ (.spanName) <$> spans
  let totalDuration = sum $ (.spanDurationNs) <$> spans
  div_ [class_ "flex h-6 w-[150px] "] $ do
    forM_ (zip [0 ..] (V.toList spans)) \(i, sp) -> do
      let wdth = (fromIntegral sp.spanDurationNs / fromIntegral totalDuration) * 150
      let color = fromMaybe "bg-black" $ HM.lookup sp.spanName colors
      let roundr = if i == length spans - 1 then "rounded-r " else ""
          roundl = if i == 0 then "rounded-l " else ""
      div_
        [ class_ $ "h-full overflow-hidden  " <> roundl <> roundr <> color
        , style_ $ "width:" <> show wdth <> "px;"
        , term "data-tippy-content" $ "Span name: " <> sp.spanName <> " Duration: " <> toText (getDurationNSMS sp.spanDurationNs)
        , title_ $ "Span name: " <> sp.spanName <> " Duration: " <> toText (getDurationNSMS sp.spanDurationNs)
        ]
        do
          div_ [class_ "h-full w-full"] ""
