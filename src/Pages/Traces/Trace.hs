module Pages.Traces.Trace (traceH, TraceDetailsGet (..)) where

import Data.Text
import Lucid
import Network.GRPC.HighLevel (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)

import Control.Error.Util (hush)
import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Containers.ListUtils (nubOrd)
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.Log qualified as Log
import Pages.Traces.Spans qualified as Spans
import Pkg.Parser (pSource)
import Relude
import Text.Megaparsec (parseMaybe)
import Utils (faSprite_, getDurationNSMS, listToIndexHashMap, utcTimeToNanoseconds)
import Witch.From (from)


traceH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders TraceDetailsGet)
traceH pid trId spanIdM = do
  _ <- Sessions.sessionAndProject pid
  traceItemM <- Telemetry.getTraceDetails pid trId
  case traceItemM of
    Just traceItem -> do
      spanRecords <- Telemetry.getSpandRecordsByTraceId pid trId
      let span_id = fromMaybe "" $ if isJust spanIdM then spanIdM else Just ""
      let pageProps = PageProps pid traceItem span_id spanRecords
      addRespHeaders $ TraceDetails pageProps
    Nothing -> addRespHeaders $ TraceDetailsNotFound "Trace not found"


data PageProps = PageProps
  { pid :: Projects.ProjectId
  , traceItem :: Telemetry.Trace
  , span_id :: Text
  , spanRecords :: V.Vector Telemetry.SpanRecord
  }


data TraceDetailsGet
  = TraceDetails PageProps
  | TraceDetailsNotFound Text


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails p) = toHtml $ tracePage p
  toHtml (TraceDetailsNotFound msg) = toHtml msg
  toHtmlRaw = toHtml


tracePage :: PageProps -> Html ()
tracePage p = do
  let pid = p.pid
      traceItem = p.traceItem
      sId = p.span_id
  div_ [class_ "w-full h-full"] $ do
    div_ [class_ "flex flex-col gap-4 h-full"] $ do
      div_ [class_ "flex items-end gap-6"] $ do
        h3_ [class_ "whitespace-nowrap text-3xl font-bold leading-none tracking-tight"] "Trace"
        div_ [class_ "flex items-end"] do
          span_ [class_ "text-sm text-gray-500 font-medium"] "ID"
          span_ [class_ "text-sm badge badge-ghost"] $ toHtml traceItem.traceId
      div_ [class_ "flex gap-4"] $ do
        div_ [class_ "font-medium"] do
          span_ "Timestamp"
          span_ [class_ "text-sm font-normal badge badge-ghost"] $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" traceItem.traceStartTime
        div_ [class_ "font-medium"] do
          span_ "Duration"
          span_ [class_ "text-sm font-normal badge badge-ghost"] $ toHtml $ getDurationNSMS traceItem.traceDurationNs
      div_ [class_ "mt-5 flex items-center w-full gap-10 pb-5 border-b"] do
        whenJust traceItem.serviceNames \s -> selectHead "Services" "All Services" s "services" (Just sId)
        selectHead "Status" "All Statuses" ["OK", "ERROR", "UNSET"] "statuses" (Just sId)
        selectHead "Kinds" "All Kinds" ["CLIENT", "INTERNAL", "SERVER", "PRODUCER", "CONSUMER"] "Kinds" (Just sId)
      div_ [] do
        div_ [role_ "tablist", class_ "tabs tabs-boxed"] $ do
          input_ [type_ "radio", name_ "my_tabs_2", role_ "tab", class_ "tab", term "aria-label" "Flame Graph", checked_]
          div_ [role_ "tabpanel", class_ "tab-content p-4"] do
            div_ [id_ $ "time-container-a" <> traceItem.traceId, class_ "w-full border-b border-b-gray-300 h-6 text-xs relative"] pass
            div_ [class_ "w-full h-44", id_ $ "a" <> traceItem.traceId] pass
          input_ [type_ "radio", name_ "my_tabs_2", role_ "tab", class_ "tab border-left", term "aria-label" "Span List"]
          div_ [role_ "tabpanel", class_ "tab-content bg-base-100 border-base-300 rounded-box h-48 overflow-auto"] do
            renderSpanTable p.spanRecords

      div_ [class_ "h-auto overflow-y-scroll mt-8  py-2 rounded-2xl border"] do
        h3_ [class_ "text-xl font-semibold px-4 border-b pb-2"] "Span"
        div_ [class_ "flex flex-col gap-4 px-4", id_ $ "span-" <> traceItem.traceId] do
          let tSp = fromMaybe (V.head p.spanRecords) (V.find (\s -> s.spanId == sId) p.spanRecords)
          Spans.expandedSpanItem pid tSp
      let spanJson =
            decodeUtf8 $
              AE.encode $
                p.spanRecords
                  <&> ( \sp ->
                          AE.object
                            [ "span_id" .= sp.spanId
                            , "name" .= sp.spanName
                            , "value" .= sp.spanDurationNs
                            , "start" .= utcTimeToNanoseconds sp.startTime
                            , "parent_id" .= sp.parentSpanId
                            ]
                      )
      let trId = traceItem.traceId
      script_ [text|flameGraphChart($spanJson, "a$trId")|]


selectHead :: Text -> Text -> V.Vector Text -> Text -> Maybe Text -> Html ()
selectHead title current options baseUrl swapTarget = div_ [class_ "flex flex-col gap-1"] do
  div_ [class_ "flex flex-col gap-1"] $ do
    span_ [class_ "text-sm text-gray-700 font-semibold"] $ toHtml title
  div_ [class_ "relative text-gray-600"] do
    button_ [class_ "border flex items-center justify-between border w-36 hover:bg-gray-100 rounded-lg px-2 py-1.5 text-sm", [__|on click toggle .hidden on the next <div/>|]] do
      span_ [class_ ""] $ toHtml current
      span_ [] do
        faSprite_ "chevron-down" "regular" "h-3 w-3"
    div_ [class_ "hidden min-w-36 w-max flex flex-col border shadow-sm left-0 absolute top-8 bg-base-100 z-50 bg-white text-sm rounded-lg"] do
      forM_ options $ \option -> do
        a_ [class_ "px-4 py-1 hover:bg-gray-100", href_ $ baseUrl <> option] $ toHtml option


renderSpanRecordRow :: Telemetry.SpanRecord -> Html ()
renderSpanRecordRow spanRecord = do
  let pidText = UUID.toText spanRecord.projectId
  let spanid = maybe "" UUID.toText spanRecord.uSpandId
  let tme = from @String (formatShow iso8601Format spanRecord.timestamp)
  tr_
    [ class_ "bg-white w-full overflow-x-hidden text-xs p-2 cursor-pointer hover:bg-gray-100 border-b-2 last:border-b-0"
    , hxGet_ $ "/p/" <> pidText <> "/log_explorer/" <> spanid <> "/" <> tme <> "/detailed?source=spans"
    , hxTarget_ $ "#span-" <> spanRecord.traceId
    , hxSwap_ "innerHTML"
    ]
    $ do
      td_ [class_ "px-2 py-1 whitespace-nowrap"] $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" spanRecord.timestamp
      td_ [class_ "px-2 py-1 whitespace-nowrap"] $ toHtml spanRecord.spanName
      td_ [class_ "px-2 py-1 whitespace-nowrap"] $ toHtml $ show spanRecord.kind
      td_ [class_ "px-2 py-1 whitespace-nowrap"] $ toHtml $ show spanRecord.status
      td_ [class_ "px-2 py-1 whitespace-nowrap"] $ toHtml $ getDurationNSMS spanRecord.spanDurationNs


renderSpanTable :: V.Vector Telemetry.SpanRecord -> Html ()
renderSpanTable records =
  table_ [class_ "min-w-full text-sm text-left text-gray-500"] $ do
    thead_ $
      tr_ [class_ "text-xs text-gray-600 bg-gray-200 p-2"] $ do
        th_ [scope_ "col", class_ "px-2 py- font-medium"] "timestamp"
        th_ [scope_ "col", class_ "px-2 py- font-medium"] "span name"
        th_ [scope_ "col", class_ "px-2 py- font-medium"] "kind"
        th_ [scope_ "col", class_ "px-2 py- font-medium"] "status"
        th_ [scope_ "col", class_ "px-2 py- font-medium"] "duration"
    tbody_ $
      mapM_ renderSpanRecordRow records
