module Pages.Traces.Trace (traceH, TraceDetailsGet (..)) where

import Data.Text (Text)
import Lucid
import Network.GRPC.HighLevel (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)

import Control.Error.Util (hush)
import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as KEM

import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Internal.Strict qualified as HM
import Data.List.Extra (nub)
import Data.Text qualified as T
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
import Utils (faSprite_, getDurationNSMS, getServiceColors, listToIndexHashMap, utcTimeToNanoseconds)
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


data ServiceData = ServiceData {name :: Text, duration :: Integer}


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails p) = toHtml $ tracePage p
  toHtml (TraceDetailsNotFound msg) = toHtml msg
  toHtmlRaw = toHtml


tracePage :: PageProps -> Html ()
tracePage p = do
  let pid = p.pid
      traceItem = p.traceItem
      sId = p.span_id
      serviceData = V.toList $ getServiceData <$> p.spanRecords
      serviceNames = V.fromList $ ordNub $ (.name) <$> serviceData
      serviceColors = getServiceColors serviceNames
  div_ [class_ "w-full h-full"] $ do
    div_ [class_ "flex flex-col w-full gap-4 h-full"] $ do
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
      div_ [class_ "flex gap-1 w-full mt-8"] $ do
        div_ [class_ "w-full"] do
          div_ [role_ "tablist", class_ "tabs tabs-bordered bg-white"] $ do
            input_ [type_ "radio", name_ "my_tabs_2", role_ "tab", class_ "tab after:pb-2", term "aria-label" "Flame Graph", checked_]
            div_ [role_ "tabpanel", class_ "tab-content w-full bg-white"] do
              div_ [class_ "flex gap-2 w-full pt-2"] do
                div_ [class_ "w-[65%] px-4 pt-4 border rounded-2xl overflow-x-hidden"] do
                  div_ [id_ $ "time-container-a" <> traceItem.traceId, class_ "w-full border-b border-b-gray-300 h-6 text-xs relative"] pass
                  div_ [class_ "w-full h-48 overflow-x-hidden overflow-y-auto relative", id_ $ "a" <> traceItem.traceId] pass
                div_ [class_ "border rounded-2xl w-[35%] overflow-x-hidden"] do
                  h3_ [class_ "w-full flex p-2 font-medium justify-between items-center border-b"] do
                    span_ [] "Services"
                    span_ [] "Exec Time %"
                  div_ [class_ "w-full h-[200px] overflow-x-hidden text-sm text-gray-600 overflow-y-auto", id_ $ "services-" <> traceItem.traceId] do
                    forM_ serviceNames $ \s -> do
                      let spans = filter (\x -> x.name == s) serviceData
                          duration = sum $ (.duration) <$> spans
                          allDur = sum $ (.duration) <$> serviceData
                          percent = show $ (fromIntegral duration / fromIntegral allDur) * 100
                          color = getServiceColor s serviceColors
                      div_ [class_ "flex items-center justify-between px-2 py-1"] $ do
                        div_ [class_ "flex gap-1 items-center"] $ do
                          div_ [class_ "w-3 h-3 rounded", style_ $ "background-color:" <> color] pass
                          span_ [class_ ""] $ toHtml s
                        div_ [class_ "flex gap-1 items-center"] $ do
                          span_ [class_ "text-xs max-w-52 truncate"] $ toHtml $ T.take 4 percent <> "%"
                          div_ [class_ "w-[100px] h-3 bg-gray-200 rounded overflow-hidden"] $
                            div_ [class_ "h-full pl-2 text-xs font-medium", style_ $ "width:" <> percent <> "%; background-color:" <> color] pass

            input_ [type_ "radio", name_ "my_tabs_2", role_ "tab", class_ "tab after:pb-2", term "aria-label" "Span List"]
            div_ [role_ "tabpanel", class_ "tab-content pt-2"] do
              div_ [class_ "border w-full rounded-2xl h-[230px] overflow-auto overflow-x-hidden "] do
                renderSpanTable p.spanRecords

      div_ [class_ "h-auto overflow-y-scroll mt-8  py-2 rounded-2xl border"] do
        h3_ [class_ "text-xl font-semibold px-4 border-b pb-2"] "Span"
        div_ [class_ "flex flex-col gap-4 px-4", id_ $ "span-" <> traceItem.traceId] do
          let tSp = fromMaybe (V.head p.spanRecords) (V.find (\s -> s.spanId == sId) p.spanRecords)
          Spans.expandedSpanItem pid tSp
      let spanJson = decodeUtf8 $ AE.encode $ p.spanRecords <&> getSpanJson
      let colorsJson = decodeUtf8 $ AE.encode $ AE.object [AEKey.fromText k .= v | (k, v) <- HM.toList serviceColors]
      let trId = traceItem.traceId
      script_ [text|flameGraphChart($spanJson, "a$trId", $colorsJson);|]


getSpanJson :: Telemetry.SpanRecord -> AE.Value
getSpanJson sp =
  AE.object
    [ "span_id" .= sp.spanId
    , "name" .= sp.spanName
    , "value" .= sp.spanDurationNs
    , "start" .= utcTimeToNanoseconds sp.startTime
    , "parent_id" .= sp.parentSpanId
    , "service_name" .= getServiceName sp
    ]


getServiceName :: Telemetry.SpanRecord -> Text
getServiceName sp = case sp.resource of
  AE.Object r -> maybe "Unknown" serviceNameString $ KEM.lookup "service.name" r
  _ -> "Unknown"
  where
    serviceNameString :: AE.Value -> Text
    serviceNameString (AE.String s) = s
    serviceNameString _ = "Unknown"


getServiceData :: Telemetry.SpanRecord -> ServiceData
getServiceData sp = ServiceData{name = getServiceName sp, duration = sp.spanDurationNs}


getServiceColor :: Text -> HashMap Text Text -> Text
getServiceColor s serviceColors = fromMaybe "#000000" $ HM.lookup s serviceColors


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
      td_ [class_ "px-2 py-1 max-w-48 truncate"] $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" spanRecord.timestamp
      td_ [class_ "px-2 py-1 max-w-48 truncate"] $ toHtml spanRecord.spanName
      td_ [class_ "px-2 py-1 max-w-48 truncate"] $ toHtml $ T.drop 2 $ maybe "----" show spanRecord.kind
      td_ [class_ "px-2 py-1 max-w-48 truncate"] $ toHtml $ T.drop 2 $ maybe "----" show spanRecord.status
      td_ [class_ "px-2 py-1 max-w-48 truncate"] $ toHtml $ getDurationNSMS spanRecord.spanDurationNs


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
