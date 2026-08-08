module Pages.LogExplorer.LogItem (
  expandAPIlogItemH,
  anchorSdkSpan,
  spanEndOrCap,
  ApiItemDetailed (..),
  expandedItemView,
  getServiceName,
  getServiceColor,
  getRequestDetails,
  spanHasErrors,
  spanBadge,
) where

import Control.Lens (filtered, has, (^..), (^?), _Just)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.Lens (key, _Array, _String)
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Foldable.WithIndex (ifor_)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Effectful.Reader.Static qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (atMapText)
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..), dateTime, emptyState_, httpTab_, jsonTab_, tabPanel_)
import Pkg.DeriveUtils (unAesonTextMaybe)
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Tracing (withSpan_)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils


getServiceName :: Maybe (Map Text AE.Value) -> Text
getServiceName rs = fromMaybe "Unknown" $ (Map.lookup "service" =<< rs) >>= (^? key "name" . _String)


getServiceColor :: Text -> HashMap Text Text -> Text
getServiceColor = HM.findWithDefault "bg-fillNeutral-strong"


getRequestDetails :: Maybe (Map Text AE.Value) -> Maybe (Text, Text, Text, Int)
getRequestDetails spanRecord = do
  m <- spanRecord
  if
    | Map.member "http" m -> Just ("HTTP", txt "http.request.method", fromMaybe "/" $ url "http" <|> url "url", status "http.response")
    | Map.member "rpc" m -> Just ("GRPC", txt "rpc.service", txt "rpc.method", status "rpc")
    | Map.member "db" m -> Just ("DB", txt "db.system", if T.null query then txt "db.statement" else query, status "db")
    | otherwise -> Nothing
  where
    txt k = fromMaybe "" $ atMapText k spanRecord
    query = txt "db.query"
    status pfx = fromMaybe 0 $ Telemetry.atMapInt (pfx <> ".status_code") spanRecord
    url pfx = viaNonEmpty head $ filter (not . T.null) [txt (pfx <> "." <> k) | k <- ["route", "path", "url", "target"]]


isHttpSpan :: Telemetry.OtelLogsAndSpans -> Bool
isHttpSpan r = any (\(t, _, _, _) -> t == "HTTP") (getRequestDetails (unAesonTextMaybe r.attributes))


-- | Exception events (event_name == "exception") within a span's events array.
getSpanErrors :: AE.Value -> [AE.Value]
getSpanErrors ae = ae ^.. _Array . traverse . filtered (\e -> (e ^? key "event_name" . _String) == Just "exception")


spanHasErrors :: Telemetry.SpanRecord -> Bool
spanHasErrors = not . null . getSpanErrors . (.events)


expandAPIlogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ApiItemDetailed)
expandAPIlogItemH pid rdId timestamp _ tabM = withSpan_ "log-explorer.detail" [] do
  _ <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let tf = Hasql.withHasqlTimefusion authCtx.env.enableTimefusionReads
  tf (Telemetry.otelRecordByProjectAndId pid timestamp rdId) >>= \case
    Nothing -> addRespHeaders $ ItemDetailedNotFound "Record not found"
    Just record
      | record.kind == Just "log" -> addRespHeaders $ LogItemExpanded pid record tabM
      | otherwise -> do
          let fetchIn window match = maybe (pure Nothing) (\trId -> tf $ Telemetry.spanRecordInTrace pid trId window match) (record.context >>= (.trace_id) >>= guarded (not . T.null))
          (anchor, sdkSpan) <-
            anchorSdkSpan
              (fetchIn (addUTCTime (-300) record.timestamp, addUTCTime 1 record.timestamp) . Telemetry.SpanBySpanId)
              (fetchIn (addUTCTime (-1) record.start_time, addUTCTime 1 (spanEndOrCap record)) (Telemetry.SpanByName Telemetry.sdkSpanStoredName))
              record
          addRespHeaders $ SpanItemExpanded pid anchor sdkSpan tabM


-- | Upper bound of a span's time window when end_time is missing (open span): cap at +300s.
spanEndOrCap :: Telemetry.OtelLogsAndSpans -> UTCTime
spanEndOrCap r = fromMaybe (addUTCTime 300 r.start_time) r.end_time


-- | Anchor the detail view on the real request span: the SDK payload span
-- ("monoscope.http") is a synthetic carrier for request/response bodies, so a
-- click on it resolves to its (HTTP) parent, and an HTTP span without captured
-- bodies borrows them from the SDK span nested inside its time window. Shared by
-- 'expandAPIlogItemH' (DB-backed lookups) and 'Pages.Telemetry.traceH' (in-memory
-- over the loaded trace).
anchorSdkSpan
  :: Monad m
  => (Text -> m (Maybe Telemetry.OtelLogsAndSpans))
  -- ^ parent lookup by span id
  -> m (Maybe Telemetry.OtelLogsAndSpans)
  -- ^ SDK-span lookup within the record's window
  -> Telemetry.OtelLogsAndSpans
  -> m (Telemetry.OtelLogsAndSpans, Maybe Telemetry.OtelLogsAndSpans)
anchorSdkSpan lookupParent lookupSdk record
  | record.name `elem` map Just Telemetry.sdkSpanNames = do
      parentM <- maybe (pure Nothing) lookupParent (record.parent_id >>= guarded (not . T.null))
      pure $ case parentM of
        Just parent | isHttpSpan parent -> (parent, Just record)
        _ -> (record, Nothing)
  | isHttpSpan record && not hasBodies = (record,) <$> lookupSdk
  | otherwise = pure (record, Nothing)
  where
    hasBodies = any (\k -> has (_Just . key k) (unAesonTextMaybe record.body)) ["request_body", "response_body"]


data ApiItemDetailed
  = SpanItemExpanded Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Telemetry.OtelLogsAndSpans) (Maybe Text)
  | LogItemExpanded Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Text)
  | ItemDetailedNotFound Text


-- | Dismissing the panel: every close affordance sends @closeDetailPanel@ to the
-- enclosing @.details-panel@; each shell (log-explorer, anomalies, trace view) owns
-- its handler on that container.
closeDetailAttrs :: [Attribute]
closeDetailAttrs = [[__|on click send closeDetailPanel to closest <.details-panel/>|]]


instance ToHtml ApiItemDetailed where
  toHtml (SpanItemExpanded pid spn aptSpan tabM) = toHtml $ expandedItemView pid spn aptSpan tabM
  toHtml (LogItemExpanded pid req tabM) = toHtml $ expandedItemView pid req Nothing tabM
  toHtml (ItemDetailedNotFound message) =
    toHtml $ emptyState_ def{icon = Just "circle-exclamation", action = ESCustom closeBtn} "Record not found" message
    where
      closeBtn =
        button_
          ( [class_ "btn btn-sm btn-ghost text-sm", Aria.label_ "Close details panel", term "data-share-hide" "1"]
              <> closeDetailAttrs
          )
          "Close"
  toHtmlRaw = toHtml


-- | Filterable pill for one field: @val@ is the raw field value (the badge owns
-- the \"label: value\" display), so the filter menu's data-field-value never has
-- to be re-parsed out of display text.
spanBadge :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
spanBadge pid path val label =
  div_
    [ class_ "relative min-w-0"
    , term "data-field-path" path
    , term "data-field-value" $ "\"" <> val <> "\""
    ]
    $ button_
      [ class_ "relative cursor-pointer flex gap-2 items-center text-textStrong bg-fillWeaker border border-strokeWeak text-xs rounded-lg whitespace-nowrap px-2 py-1 max-w-64"
      , term "data-tippy-content" $ label <> ": " <> val
      , [__|install LogItemMenuable|]
      ]
      (span_ [class_ "truncate"] $ toHtml $ label <> ": " <> val)


-- | One detail tab. @panel@ carries its own literal @group-has-[.MARKER:checked]/dtab:block@
-- class — Tailwind only compiles class strings that appear verbatim in source, so this must
-- never be built from @marker@ at runtime. Default-active tab is the first shown (not a field).
data DetailTab = DetailTab {marker :: Text, cls :: Text, label :: Html (), panel :: Html ()}


-- Unified view for both logs and spans
expandedItemView :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> Maybe Telemetry.OtelLogsAndSpans -> Maybe Text -> Html ()
expandedItemView pid item aptSp selectedTabM = do
  -- Row #1 (both views): back-to-logs (mobile only), timestamp, close.
  div_ [class_ "sticky top-[-1px] z-10 flex items-center gap-2 bg-bgBase border-b border-l border-strokeWeak max-md:border-l-0 px-2 py-1"] do
    button_
      ( [ class_ "hidden max-md:flex cursor-pointer items-center gap-1.5 text-sm font-medium text-textBrand"
        , Aria.label_ "Close details"
        , term "data-share-hide" "1"
        ]
          <> closeDetailAttrs
      )
      (faSprite_ "chevron-left" "regular" "w-3.5 h-3.5" >> "Back to logs")
    div_ [class_ "flex gap-2 items-center shrink-0 ml-auto"] do
      dateTime (if isLog then item.timestamp else item.start_time) Nothing
      div_ [class_ "flex gap-1 items-center"] do
        button_
          [ class_ "fs-details-toggle cursor-pointer rounded-md p-1 hover:bg-fillWeak transition-colors hidden md:[#apiLogsPage_&]:block [#trace_details_container_&]:hidden! tooltip tooltip-bottom"
          , Aria.label_ "Toggle fullscreen"
          , data_ "tip" "Expand panel"
          , term "data-share-hide" "1"
          , [__|on click send toggleFullscreen(mode: 'details') to #apiLogsPage|]
          ]
          do
            faSprite_ "expand" "regular" "w-3.5 h-3.5 text-iconNeutral [#apiLogsPage[data-fullscreen=details]_&]:hidden!"
            faSprite_ "compress" "regular" "hidden! w-3.5 h-3.5 text-iconNeutral [#apiLogsPage[data-fullscreen=details]_&]:block!"
        button_
          ( [ class_ "cursor-pointer detail-close-btn rounded-md p-1 hover:bg-fillWeak transition-colors tooltip tooltip-left"
            , Aria.label_ "Close item details"
            , data_ "tip" "Close · Esc"
            ]
              <> closeDetailAttrs
          )
          $ faSprite_ "xmark" "regular" "w-3 h-3 text-iconNeutral"
  div_ [class_ $ "w-full pl-3 pr-1 pb-2 relative border-l border-strokeWeak max-md:border-l-0 max-md:px-0 " <> if isLog then " flex flex-col gap-2" else " pb-[50px]"] do
    div_ [id_ "copy_share_link"] pass
    unless isLog $ htmxOverlayIndicator_ "loading-span-list"
    htmxOverlayIndicator_ "details_indicator"
    headerBlock
    div_ [class_ "w-full mt-3 group/dtab"] do
      div_ [class_ "flex", [__|on click halt the event's bubbling|]] do
        traverse_ detailTabRadio_ tabs
        div_ [class_ "w-full border-b-2 border-b-strokeWeak"] pass
      -- All panels render once; the checked radio + group-has classes pick the
      -- visible one, so tab switches are pure CSS (no round trip, no re-query).
      div_ [class_ "mt-2 py-1 text-textWeak"] $ traverse_ (.panel) tabs
  where
    isLog = item.kind == Just "log"
    isAlert = item.kind == Just "alert"
    isHttp = not isLog && isHttpSpan item
    createdAt = formatUTC item.timestamp
    pidTxt = pid.toText
    dgrp = "dtab-" <> item.id
    -- Clamp the ?tab= deep link to a marker that actually exists — an unknown
    -- value would check no radio and leave every panel hidden.
    firstMarker = case tabs of [] -> "tab-raw"; t : _ -> t.marker
    activeMarker = maybe firstMarker (.marker) $ selectedTabM >>= \s -> find ((== s) . (.marker)) tabs
    detailTabRadio_ t = label_ [class_ $ "cursor-pointer border-b-2 border-b-strokeWeak px-4 py-1.5 text-sm has-[:focus-visible]:ring-2 has-[:focus-visible]:ring-strokeBrand-strong has-[:focus-visible]:rounded-sm has-[:checked]:font-bold has-[:checked]:border-strokeBrand-strong has-[:checked]:text-textBrand " <> t.cls] do
      input_ $ [type_ "radio", name_ dgrp, class_ $ "sr-only " <> t.marker] <> [checked_ | t.marker == activeMarker]
      t.label
    events = fromMaybe AE.Null (unAesonTextMaybe item.events)
    spanErrors = if isLog then [] else getSpanErrors events

    -- Best-effort curl reconstruction from the span's HTTP attributes and any
    -- captured request body (SDK-span fallback included via aptSp).
    curlCommand =
      let cSp = fromMaybe item aptSp
          attrsM = unAesonTextMaybe cSp.attributes
          att k = atMapText k attrsM >>= guarded (not . T.null)
          method = fromMaybe "GET" (att "http.request.method")
          path = maybe "/" (\(_, _, u, _) -> u) (getRequestDetails attrsM)
          host = asum (map att ["server.address", "http.host", "net.host.name"])
          target = fromMaybe path $ att "url.full" <|> ((\h -> "https://" <> h <> path) <$> host)
          hdrVal = \case
            AE.String s -> s
            AE.Array xs -> T.intercalate ", " [s | AE.String s <- toList xs]
            v -> decodeUtf8 (AE.encode v)
          headers = case attrsM >>= Map.lookup "http" >>= (^? key "request" . key "header") of
            Just (AE.Object km) -> [(AEKey.toText k, hdrVal v) | (k, v) <- KEM.toList km]
            _ -> []
          bodyTxtM = case unAesonTextMaybe cSp.body >>= (^? key "request_body") of
            Just (AE.String s) -> guarded (not . T.null) s
            Just v | v /= AE.Null -> Just (decodeUtf8 (AE.encode v))
            _ -> Nothing
          esc = T.replace "'" "'\\''"
       in T.intercalate
            " \\\n  "
            ( ["curl -X " <> method <> " '" <> esc target <> "'"]
                <> ["-H '" <> esc (k <> ": " <> v) <> "'" | (k, v) <- headers]
                <> ["-d '" <> esc b <> "'" | b <- maybeToList bodyTxtM]
            )

    -- Summary pills (or alert title) + timestamp + close, then the span-id pill and action row.
    headerBlock = div_ [class_ "detail-header-block flex flex-col gap-1.5 bg-fillWeaker py-2.5 px-3"] do
      if isAlert
        then div_ [class_ "flex items-center gap-3 min-w-0"] do
          h4_ [class_ "text-xl max-w-96 truncate"] $ toHtml $ fromMaybe "" item.name
          span_ [class_ $ "badge badge-sm whitespace-nowrap " <> getAlertStatusColor (fromMaybe "" item.status_message)] $ toHtml $ fromMaybe "" item.status_message
        else div_ [class_ "min-w-0"] $ renderSummaryElements (summaryForDetailView (Telemetry.generateSummary item))
      -- span_id isn't carried by generateSummary; keep one pill so its filter-menu stays reachable.
      whenJust (item.context >>= (.span_id) >>= guarded (not . T.null)) \v ->
        div_ [class_ "flex gap-2 flex-wrap min-w-0"] $ spanBadge pid "context.span_id" v "Span ID"
      div_ [class_ "flex flex-wrap gap-2 items-center"] actionRow

    actionBtnBody :: Text -> Text -> Html ()
    actionBtnBody icon lbl = faSprite_ icon "regular" "w-3 h-3" >> toHtml lbl

    actionRow = do
      when isHttp do
        -- Curl text is assembled server-side (attrs + captured bodies are already
        -- in hand); the button only copies it, via the shared Copy behavior.
        let curlCls = "curl-cmd-" <> item.id
        button_ [class_ "action-btn", term "_" [text|install Copy(content:.${curlCls})|]] $ actionBtnBody "copy" "Copy as curl"
        pre_ [class_ $ curlCls <> " hidden"] $ toHtml curlCommand
      whenJust (item.context >>= (.trace_id) >>= guarded (not . T.null)) \trId ->
        -- The trace overlay's loadTrace handler owns skeleton, fetch, and fullscreen
        -- (see apiLogsPage.traceOverlay); this button only sends the event + URL state.
        button_
          [ class_ "action-btn"
          , term "data-share-hide" "1"
          , term
              "_"
              [text|on click send loadTrace(url: '/p/${pidTxt}/traces/${trId}/?timestamp=${createdAt}') to #trace_expanded_view
                     then call updateUrlState('showTrace', "${trId}/?timestamp=${createdAt}")|]
          ]
          (actionBtnBody "cross-hair" "View trace")
      when isAlert
        $ a_ [class_ "action-btn", href_ $ "/p/" <> pid.toText <> "/monitors/" <> fromMaybe "" item.parent_id <> "/overview"]
        $ actionBtnBody "bell" "View alert"
      button_ [class_ "action-btn", term "data-share-hide" "1", hxPost_ ("/p/" <> pid.toText <> "/share/" <> item.id <> "/" <> createdAt <> "?event_type=" <> (if isLog then "log" else "span")), hxSwap_ "innerHTML", hxTarget_ "#copy_share_link"]
        $ actionBtnBody "link-simple" "Share link"

    -- The tab set as data: [shown?] marker, extra button class, label, panel (with its literal vis class).
    tabs =
      catMaybes
        [ tab
            ((isLog || isAlert) && isJust item.body)
            "tab-body"
            ""
            "Body"
            (whenJust item.body \b -> jsonTab_ "group-has-[.tab-body:checked]/dtab:block" "body-content" (AE.toJSON b) Nothing)
        , tab isHttp "tab-req" "" "Request" reqPanel
        , tab (not isAlert) "tab-att" "" "Attributes" attPanel
        , tab
            True
            "tab-meta"
            ""
            "Process"
            (jsonTab_ "group-has-[.tab-meta:checked]/dtab:block" "meta-content" (maybe (AE.object []) (AE.Object . KEM.fromMapText) (unAesonTextMaybe item.resource)) (Just "resource"))
        , tab
            (not isLog && not (null spanErrors))
            "tab-errors"
            "flex items-center gap-1"
            (badge "Errors" "badge badge-error badge-sm" (length spanErrors))
            (tabPanel_ "group-has-[.tab-errors:checked]/dtab:block w-full whitespace-wrap" "errors-content" (renderErrors spanErrors))
        , tab
            (not isLog)
            "tab-logs"
            "flex items-center gap-1"
            (badge "Logs" "badge badge-ghost badge-sm" (maybe 0 length (events ^? _Array)))
            (jsonTab_ "group-has-[.tab-logs:checked]/dtab:block" "logs-content" events Nothing)
        , tab True "tab-raw" "whitespace-nowrap" "Raw data" (jsonTab_ "group-has-[.tab-raw:checked]/dtab:block" "m-raw-content" (AE.toJSON item) Nothing)
        ]
      where
        tab shown m c l p = DetailTab m c l p <$ guard shown
        badge l c n = toHtml @Text l >> div_ [class_ c] (show n)
        attPanel = tabPanel_ "group-has-[.tab-att:checked]/dtab:block" "att-content" $ case unAesonTextMaybe item.attributes of
          Just m | not (null m) -> jsonValueToHtmlTree (AE.Object $ KEM.fromMapText m) $ Just "attributes"
          _ -> div_ [class_ "text-sm text-textWeak italic py-4"] "No custom attributes on this entry"
        reqPanel = tabPanel_ "group-has-[.tab-req:checked]/dtab:block" "request-content" $ div_ [id_ "http-content-container", class_ "group/htab flex flex-col gap-3 mt-2"] do
          let cSp = fromMaybe item aptSp
              bodyField k = fromMaybe (AE.object []) $ unAesonTextMaybe cSp.body >>= (^? key k)
              httpAttrs = fromMaybe AE.Null $ unAesonTextMaybe cSp.attributes >>= Map.lookup "http"
              hp ks = fromMaybe AE.Null $ foldlM (\v k -> v ^? key k) httpAttrs ks
              notEmpty v = v `notElem` ([AE.Null, AE.object [], AE.Array mempty, AE.String ""] :: [AE.Value])
              -- Default to the first sub-tab that has content (Request Details always does).
              defaultTab =
                maybe "htab-raw" fst
                  $ find
                    snd
                    ( [ ("htab-res", notEmpty (bodyField "response_body"))
                      , ("htab-req", notEmpty (bodyField "request_body"))
                      , ("htab-hed", any (notEmpty . hp) [["request", "header"], ["response", "header"]])
                      , ("htab-par", any (notEmpty . hp) [["request", "query_params"], ["request", "path_params"]])
                      ]
                        :: [(Text, Bool)]
                    )
          div_ [class_ "bg-fillWeaker w-max rounded-lg border border-strokeWeak justify-start items-start inline-flex"]
            $ div_ [class_ "justify-start items-start flex text-sm"]
            $ forM_ ([("htab-res", "Res Body"), ("htab-req", "Req Body"), ("htab-hed", "Headers"), ("htab-par", "Params"), ("htab-raw", "Request Details")] :: [(Text, Html ())]) \(m, l) ->
              httpTab_ ("htab-" <> item.id) m (m == defaultTab) l
          div_ []
            $ forM_
              ( [ ("group-has-[.htab-raw:checked]/htab:block", "raw_content", AE.toJSON cSp, Nothing)
                , ("group-has-[.htab-req:checked]/htab:block", "req_content", bodyField "request_body", Just "body.request_body")
                , ("group-has-[.htab-res:checked]/htab:block", "res_content", bodyField "response_body", Just "body.response_body")
                , ("group-has-[.htab-hed:checked]/htab:block", "hed_content", AE.object ["request_headers" AE..= hp ["request", "header"], "response_headers" AE..= hp ["response", "header"]], Nothing)
                , ("group-has-[.htab-par:checked]/htab:block", "par_content", AE.object ["query_params" AE..= hp ["request", "query_params"], "path_params" AE..= hp ["request", "path_params"]], Nothing)
                ]
                  :: [(Text, Text, AE.Value, Maybe Text)]
              )
              \(visCls, eid, val, filt) -> jsonTab_ visCls eid val filt


renderErrors :: [AE.Value] -> Html ()
renderErrors errs =
  div_ [class_ "flex flex-col mt-4 gap-3 w-full"] $ ifor_ errs \idx err ->
    div_ [class_ "w-full border border-strokeError-strong/40 rounded-lg overflow-hidden bg-fillError-weak/30"] do
      let (tye, message, stacktrace) = getErrorDetails err
          copyId = "exc-msg-" <> show idx
      -- Header: red strip with type, position, copy
      div_ [class_ "flex items-center justify-between gap-3 px-3 py-2 bg-fillError-weak border-b border-strokeError-strong/40"] do
        div_ [class_ "flex items-center gap-2 min-w-0"] do
          faSprite_ "circle-exclamation" "solid" "w-4 h-4 text-iconError shrink-0"
          span_ [class_ "font-semibold text-sm text-textError truncate"] $ toHtml $ if T.null tye then "Exception" else tye
          when (length errs > 1)
            $ span_ [class_ "text-2xs font-medium text-textWeak px-1.5 py-0.5 rounded bg-bgBase border border-strokeWeak shrink-0"]
            $ toHtml @Text (show idx <> " / " <> show (length errs))
        unless (T.null message)
          $ button_
            [ class_ "shrink-0 cursor-pointer flex items-center gap-1 text-xs px-2 py-0.5 rounded text-textWeak hover:text-textStrong hover:bg-bgBase/60 transition-colors"
            , Aria.label_ "Copy exception message"
            , term "_" [text|install Copy(content:.${copyId})|]
            ]
            (faSprite_ "copy" "regular" "w-3 h-3" >> "Copy")
      unless (T.null message)
        $ pre_ [class_ $ copyId <> " text-xs font-mono whitespace-pre-wrap break-words text-textStrong px-3 py-2.5 leading-relaxed"] (toHtml message)
      unless (T.null stacktrace) $ details_ [class_ "group/st border-t border-strokeError-strong/30"] do
        summary_ [class_ "cursor-pointer select-none flex items-center gap-1.5 px-3 py-2 text-xs font-medium text-textWeak hover:text-textStrong"] do
          faSprite_ "chevron-right" "regular" "w-3 h-3 transition-transform group-open/st:rotate-90"
          "Stack trace"
          span_ [class_ "text-2xs text-textWeak/70"] $ toHtml @Text $ "(" <> show (length (lines stacktrace)) <> " frames)"
        div_ [class_ "px-3 pb-3"]
          $ pre_ [class_ "text-xs font-mono whitespace-pre text-textWeak bg-bgBase border border-strokeWeak rounded-md p-3 max-h-72 overflow-auto leading-snug"]
          $ toHtml stacktrace
  where
    getErrorDetails :: AE.Value -> (Text, Text, Text)
    getErrorDetails ae = (fld "type", fld "message", fld "stacktrace")
      where
        fld k = fromMaybe "" $ ae ^? key "event_attributes" . key "exception" . key k . _String
