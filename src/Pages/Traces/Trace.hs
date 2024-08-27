module Pages.Traces.Trace (traceH, TraceDetailsGet (..)) where

import Data.Text
import Lucid
import Network.GRPC.HighLevel (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)

import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import Relude
import Utils (getDurationNSMS)


traceH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders TraceDetailsGet)
traceH pid trId spanIdM = do
  _ <- Sessions.sessionAndProject pid
  traceItemM <- Telemetry.getTraceDetails pid trId
  case traceItemM of
    Just traceItem -> do
      let span_id = fromMaybe "" $ if isJust spanIdM then spanIdM else Just ""
      addRespHeaders $ TraceDetails pid traceItem span_id
    Nothing -> addRespHeaders $ TraceDetailsNotFound "Trace not found"


data TraceDetailsGet
  = TraceDetails Projects.ProjectId Telemetry.Trace Text
  | TraceDetailsNotFound Text


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails pid traceItem span_id) = toHtml $ tracePage pid traceItem span_id
  toHtml (TraceDetailsNotFound msg) = toHtml msg
  toHtmlRaw = toHtml


tracePage :: Projects.ProjectId -> Telemetry.Trace -> Text -> Html ()
tracePage pid traceItem sId = do
  div_ [class_ "w-full"] $ do
    div_ [class_ "flex flex-col space-y-1.5 p-6 bg-gray-50 px-6 py-4"] $ do
      div_ [class_ "flex flex-col gap-4"] $ do
        div_ [class_ "flex items-end gap-6"] $ do
          h3_ [class_ "whitespace-nowrap text-2xl font-semibold leading-none tracking-tight"] "Trace"
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
