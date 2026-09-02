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
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), dateTime, detailTab_, emptyState_, httpTab_, stackTrace_, tabPanel_)
import Pkg.DeriveUtils (unAesonTextMaybe)
import Pkg.StackTrace qualified as StackTrace
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Tracing (withSpan_)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils


getServiceName :: Maybe (Map Text AE.Value) -> Text
getServiceName rs = fromMaybe "Unknown" $ (Map.lookup "service" =<< rs) >>= (^? key "name" . _String)


getServiceColor :: Text -> HashMap Text Text -> Text
getServiceColor = HM.findWithDefault "bg-fillStrong"


getRequestDetails :: Maybe (Map Text AE.Value) -> Maybe (Text, Text, Text, Int)
getRequestDetails spanRecord = do
  m <- spanRecord
  if
    | Map.member "http" m -> Just ("HTTP", txt "http.request.method", fromMaybe "/" $ url "http" <|> url "url", status "http.response")
    | Map.member "rpc" m -> Just ("GRPC", txt "rpc.service", txt "rpc.method", status "rpc")
    -- Modern semconv key first, deprecated one as the fallback for pre-migration rows.
    | Map.member "db" m -> Just ("DB", fromMaybe "" $ firstOf ["db.system.name", "db.system"], fromMaybe "" $ firstOf ["db.query.text", "db.statement"], status "db")
    | otherwise -> Nothing
  where
    txt k = fromMaybe "" $ atMapText k spanRecord
    firstOf = viaNonEmpty head . filter (not . T.null) . map txt
    status pfx = fromMaybe 0 $ Telemetry.atMapInt (pfx <> ".status_code") spanRecord
    url pfx = firstOf [pfx <> "." <> k | k <- ["route", "path", "url", "target"]]


isHttpSpan :: Telemetry.OtelLogsAndSpans -> Bool
isHttpSpan r = any (\(t, _, _, _) -> t == "HTTP") (getRequestDetails (unAesonTextMaybe r.attributes))


-- | Exception events (event_name == "exception") within a span's events array.
getSpanErrors :: AE.Value -> [AE.Value]
getSpanErrors ae = ae ^.. _Array . traverse . filtered (\e -> (e ^? key "event_name" . _String) == Just "exception")


spanHasErrors :: Telemetry.SpanRecord -> Bool
spanHasErrors = not . null . getSpanErrors . (.events)


expandAPIlogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Bool -> ATAuthCtx (RespHeaders ApiItemDetailed)
expandAPIlogItemH pid rdId timestamp _ tabM subtabM partial = withSpan_ "log-explorer.detail" [] do
  _ <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let tf = Hasql.withHasqlTimefusion authCtx.env.enableTimefusionReads
      renderItem item aptSpan full = addRespHeaders $ if partial then DetailTabExpanded pid item aptSpan (fromMaybe "tab-raw" tabM) subtabM else full
  Telemetry.otelRecordByProjectAndId authCtx.env.enableTimefusionReads pid timestamp rdId >>= \case
    Nothing -> addRespHeaders $ ItemDetailedNotFound "Record not found"
    Just record
      | record.kind == Just "log" -> renderItem record Nothing (LogItemExpanded pid record tabM)
      | otherwise -> do
          let fetchIn window match = maybe (pure Nothing) (\trId -> tf $ Telemetry.spanRecordInTrace pid trId window match) (record.context >>= (.trace_id) >>= guarded (not . T.null))
          (anchor, sdkSpan) <-
            anchorSdkSpan
              (fetchIn (addUTCTime (-300) record.timestamp, addUTCTime 1 record.timestamp) . Telemetry.SpanBySpanId)
              (fetchIn (addUTCTime (-1) record.start_time, addUTCTime 1 (spanEndOrCap record)) (Telemetry.SpanByName Telemetry.sdkSpanStoredName))
              record
          renderItem anchor sdkSpan (SpanItemExpanded pid anchor sdkSpan tabM)


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
  | DetailTabExpanded Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Telemetry.OtelLogsAndSpans) Text (Maybe Text)
  | ItemDetailedNotFound Text


-- | Dismissing the panel: every close affordance sends @closeDetailPanel@ to the
-- enclosing @.details-panel@; each shell (log-explorer, anomalies, trace view) owns
-- its handler on that container.
closeDetailAttrs :: [Attribute]
closeDetailAttrs = [[__|on click send closeDetailPanel to closest <.details-panel/>|]]


instance ToHtml ApiItemDetailed where
  toHtml (SpanItemExpanded pid spn aptSpan tabM) = toHtml $ expandedItemView pid spn aptSpan tabM
  toHtml (LogItemExpanded pid req tabM) = toHtml $ expandedItemView pid req Nothing tabM
  toHtml (DetailTabExpanded pid item aptSpan marker subtabM) =
    toHtml
      $ whenJust
        ( case subtabM of
            Just subtab | marker == "tab-req" -> find ((== subtab) . (.marker)) (snd $ httpDetailTabs item aptSpan)
            Just _ -> Nothing
            Nothing -> find ((== marker) . (.marker)) (detailTabs pid item aptSpan)
        )
        renderDetailPanel
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


-- | One detail tab. @panelClass@ carries its own literal
-- @group-has-[.MARKER:checked]/dtab:block@ class so Tailwind can see it.
data DetailTab = DetailTab
  { marker :: Text
  , cls :: Text
  , label :: Html ()
  , panelClass :: Text
  , panelId :: Text
  , content :: Html ()
  }


renderDetailPanel :: DetailTab -> Html ()
renderDetailPanel tab = tabPanel_ tab.panelClass tab.panelId tab.content


-- | A hidden panel that fetches itself the first time its radio reveals it.
-- @tabQuery@ names the tab server-side: a top-level tab is @tab=\<marker\>@, an
-- HTTP sub-tab is @tab=tab-req&subtab=\<marker\>@.
lazyPanel :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> (Text -> Text) -> DetailTab -> Html ()
lazyPanel pid item tabQuery tab =
  div_
    [ class_ $ "hidden " <> tab.panelClass
    , id_ tab.panelId
    , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer/" <> item.id <> "/" <> formatUTC item.timestamp <> "/detailed?" <> tabQuery tab.marker <> "&partial=true"
    , hxTrigger_ "intersect once"
    , hxTarget_ "this"
    , hxSwap_ "outerHTML"
    , term "hx-sync" "this:replace"
    , Aria.busy_ "true"
    ]
    $ div_ [class_ "flex justify-center py-8", role_ "status", Aria.label_ "Loading details"]
    $ loadingIndicator_ LdSM LdDots


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
      -- The selected panel renders now; hidden placeholders fetch and replace
      -- themselves when their radio makes them visible for the first time.
      div_ [class_ "mt-2 py-1 text-textWeak"]
        $ traverse_ (\tab -> if tab.marker == activeMarker then renderDetailPanel tab else lazyPanel pid item ("tab=" <>) tab) tabs
  where
    isLog = item.kind == Just "log"
    isAlert = item.kind == Just "alert"
    isHttp = not isLog && isHttpSpan item
    identityRows = Telemetry.rowIdentity (unAesonTextMaybe item.attributes)
    createdAt = formatUTC item.timestamp
    pidTxt = pid.toText
    dgrp = "dtab-" <> item.id
    -- Clamp the ?tab= deep link to a marker that actually exists — an unknown
    -- value would check no radio and leave every panel hidden.
    firstMarker = case tabs of [] -> "tab-raw"; t : _ -> t.marker
    activeMarker = maybe firstMarker (.marker) $ selectedTabM >>= \s -> find ((== s) . (.marker)) tabs
    detailTabRadio_ t = detailTab_ dgrp t.marker t.cls (t.marker == activeMarker) t.label
    tabs = detailTabs pid item aptSp

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
      -- Who the request was for, in full. The summary row above can only afford one
      -- identifier, so tenant id, user id and the rest used to be reachable only by opening
      -- the Attributes JSON tree — which is why readers concluded we only knew their email.
      -- Every field is a filter pill, so "everything from this tenant" is one click away.
      unless (null identityRows) $ div_ [class_ "flex gap-2 flex-wrap min-w-0"] $ forM_ identityRows \(k, label, v) ->
        spanBadge pid ("attributes." <> k) v label
      div_ [class_ "flex flex-wrap gap-2 items-center"] actionRow

    actionBtnBody :: Text -> Text -> Html ()
    actionBtnBody icon lbl = faSprite_ icon "regular" "w-3 h-3" >> toHtml lbl

    actionRow = do
      when isHttp do
        -- Curl text is assembled server-side (attrs + captured bodies are already
        -- in hand); the button only copies it, via the shared Copy behavior. The source
        -- is the adjacent <pre> — `next <pre/>` rather than a per-item class, so nothing
        -- has to mint a unique selector and the two stay coupled by position.
        button_ [class_ "action-btn", [__|install Copy(content: next <pre/>)|]] $ actionBtnBody "copy" "Copy as curl"
        pre_ [class_ "hidden"] $ toHtml curlCommand
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


detailTabs :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> Maybe Telemetry.OtelLogsAndSpans -> [DetailTab]
detailTabs pid item aptSp =
  catMaybes
    [ tab
        ((isLog || isAlert) && isJust item.body)
        "tab-body"
        ""
        "Body"
        "group-has-[.tab-body:checked]/dtab:block"
        "body-content"
        (whenJust item.body \b -> jsonValueToHtmlTree (AE.toJSON b) Nothing)
    , tab isHttp "tab-req" "" "Request" "group-has-[.tab-req:checked]/dtab:block" "request-content" (renderHttpDetails pid item aptSp)
    , tab (not isAlert) "tab-att" "" "Attributes" "group-has-[.tab-att:checked]/dtab:block" "att-content" attContent
    , tab
        True
        "tab-meta"
        ""
        "Process"
        "group-has-[.tab-meta:checked]/dtab:block"
        "meta-content"
        (jsonValueToHtmlTree (maybe (AE.object []) (AE.Object . KEM.fromMapText) (unAesonTextMaybe item.resource)) (Just "resource"))
    , tab
        (not isLog && not (null spanErrors))
        "tab-errors"
        "flex items-center gap-1"
        (badge "Errors" "badge badge-error badge-sm" (length spanErrors))
        "group-has-[.tab-errors:checked]/dtab:block w-full whitespace-wrap"
        "errors-content"
        (renderErrors pid (Telemetry.spanServiceName item) (\k -> atMapText k (unAesonTextMaybe item.attributes)) (\k -> atMapText k (unAesonTextMaybe item.resource)) spanErrors)
    , tab
        (not isLog)
        "tab-logs"
        "flex items-center gap-1"
        (badge "Logs" "badge badge-ghost badge-sm" (maybe 0 length (events ^? _Array)))
        "group-has-[.tab-logs:checked]/dtab:block"
        "logs-content"
        (jsonValueToHtmlTree events Nothing)
    , -- The panel is a shell, not content: "which metrics relate to this row" is a
      -- DB question and 'detailTabs' is pure. HTMX fetches the answer when the tab
      -- is first revealed, so opening a span never pays for metrics nobody asked for.
      tab
        (not isAlert)
        "tab-metrics"
        ""
        "Metrics"
        "group-has-[.tab-metrics:checked]/dtab:block"
        "metrics-content"
        ( div_
            [ hxGet_ $ "/p/" <> pid.toText <> "/log_explorer/" <> item.id <> "/" <> formatUTC item.timestamp <> "/related_metrics"
            , hxTrigger_ "intersect once"
            , hxTarget_ "this"
            , hxSwap_ "innerHTML"
            , term "hx-on::after-request" "this.removeAttribute('aria-busy')"
            , Aria.busy_ "true"
            ]
            $ div_ [class_ "flex justify-center py-8", role_ "status", Aria.label_ "Loading related metrics"]
            $ loadingIndicator_ LdSM LdDots
        )
    , tab True "tab-raw" "whitespace-nowrap" "Raw data" "group-has-[.tab-raw:checked]/dtab:block" "m-raw-content" (jsonValueToHtmlTree (AE.toJSON item) Nothing)
    ]
  where
    isLog = item.kind == Just "log"
    isAlert = item.kind == Just "alert"
    isHttp = not isLog && isHttpSpan item
    events = fromMaybe AE.Null (unAesonTextMaybe item.events)
    spanErrors = if isLog then [] else getSpanErrors events
    tab shown marker cls label panelClass panelId content = DetailTab{marker, cls, label, panelClass, panelId, content} <$ guard shown
    badge label cls count = toHtml @Text label >> div_ [class_ cls] (show count)
    attContent = case unAesonTextMaybe item.attributes of
      Just m | not (null m) -> jsonValueToHtmlTree (AE.Object $ KEM.fromMapText m) $ Just "attributes"
      _ -> emptyState_ def{size = ESCompact} "No custom attributes on this entry" ""


renderHttpDetails :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> Maybe Telemetry.OtelLogsAndSpans -> Html ()
renderHttpDetails pid item aptSp = div_ [id_ "http-content-container", class_ "group/htab flex flex-col gap-3 mt-2"] do
  let (activeMarker, tabs) = httpDetailTabs item aptSp
  div_ [class_ "bg-fillWeaker w-max rounded-lg border border-strokeWeak justify-start items-start inline-flex"]
    $ div_ [class_ "justify-start items-start flex text-sm"]
    $ forM_ tabs \tab -> httpTab_ ("htab-" <> item.id) tab.marker (tab.marker == activeMarker) tab.label
  div_ [] $ forM_ tabs \tab -> if tab.marker == activeMarker then renderDetailPanel tab else lazyPanel pid item (\m -> "tab=tab-req&subtab=" <> m) tab


httpDetailTabs :: Telemetry.OtelLogsAndSpans -> Maybe Telemetry.OtelLogsAndSpans -> (Text, [DetailTab])
httpDetailTabs item aptSp = (activeMarker, tabs)
  where
    cSp = fromMaybe item aptSp
    bodyField k = fromMaybe (AE.object []) $ unAesonTextMaybe cSp.body >>= (^? key k)
    httpAttrs = fromMaybe AE.Null $ unAesonTextMaybe cSp.attributes >>= Map.lookup "http"
    hp ks = fromMaybe AE.Null $ foldlM (\v k -> v ^? key k) httpAttrs ks
    notEmpty v = v `notElem` ([AE.Null, AE.object [], AE.Array mempty, AE.String ""] :: [AE.Value])
    tabs =
      [ DetailTab "htab-res" "" "Res Body" "group-has-[.htab-res:checked]/htab:block" "res_content" (jsonValueToHtmlTree (bodyField "response_body") $ Just "body.response_body")
      , DetailTab "htab-req" "" "Req Body" "group-has-[.htab-req:checked]/htab:block" "req_content" (jsonValueToHtmlTree (bodyField "request_body") $ Just "body.request_body")
      , DetailTab "htab-hed" "" "Headers" "group-has-[.htab-hed:checked]/htab:block" "hed_content" (jsonValueToHtmlTree (AE.object ["request_headers" AE..= hp ["request", "header"], "response_headers" AE..= hp ["response", "header"]]) Nothing)
      , DetailTab "htab-par" "" "Params" "group-has-[.htab-par:checked]/htab:block" "par_content" (jsonValueToHtmlTree (AE.object ["query_params" AE..= hp ["request", "query_params"], "path_params" AE..= hp ["request", "path_params"]]) Nothing)
      , DetailTab "htab-raw" "" "Request Details" "group-has-[.htab-raw:checked]/htab:block" "raw_content" (jsonValueToHtmlTree (AE.toJSON cSp) Nothing)
      ]
    activeMarker =
      maybe "htab-raw" fst
        $ find
          snd
          ( [ ("htab-res", notEmpty $ bodyField "response_body")
            , ("htab-req", notEmpty $ bodyField "request_body")
            , ("htab-hed", any (notEmpty . hp) [["request", "header"], ["response", "header"]])
            , ("htab-par", any (notEmpty . hp) [["request", "query_params"], ["request", "path_params"]])
            ]
              :: [(Text, Bool)]
          )


-- | @attr@ reads the span's attributes (where @code.*@ lives) and @resAttr@ its resource
-- (where @service.version@ and the @vcs.*@\/@git.*@ revision keys live). Two lookups rather
-- than one because they are two different columns: passing the span map for both is what
-- made 'StackTrace.revisionFor' answer 'Nothing' for every span it was ever given.
renderErrors :: Projects.ProjectId -> Maybe Text -> (Text -> Maybe Text) -> (Text -> Maybe Text) -> [AE.Value] -> Html ()
renderErrors pid svcM attr resAttr errs =
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
      let frames = StackTrace.framesFor attr stacktrace
      unless (null frames) $ details_ ([class_ "group/st border-t border-strokeError-strong/30"] <> [open_ "" | idx == 0]) do
        summary_ [class_ "cursor-pointer select-none flex items-center gap-1.5 px-3 py-2 text-xs font-medium text-textWeak hover:text-textStrong"] do
          faSprite_ "chevron-right" "regular" "w-3 h-3 transition-transform group-open/st:rotate-90"
          "Stack trace"
          span_ [class_ "text-2xs text-textWeak/70"] $ toHtml @Text $ "(" <> show (length frames) <> " frames)"
        div_ [class_ "px-3 pb-3 max-h-96 overflow-auto c-scroll"] $ stackTrace_ pid svcM attr resAttr stacktrace
  where
    getErrorDetails :: AE.Value -> (Text, Text, Text)
    getErrorDetails ae = (fld "type", fld "message", fld "stacktrace")
      where
        fld k = fromMaybe "" $ ae ^? key "event_attributes" . key "exception" . key k . _String
