module Pages.Telemetry.Utils (getServiceName, getServiceColor, getRequestDetails, spanHasErrors, getSpanErrors, getErrorDetails, metricsTree, atMapText)
where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as KEM
import Data.HashMap.Strict hiding (null)
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Vector qualified as V
import Lucid
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Relude
import Utils (faSprite_)


getServiceName :: Maybe (Map Text AE.Value) -> Text
getServiceName rs = case Map.lookup "service" (fromMaybe Map.empty rs) of
  Just (AE.Object o) -> case KEM.lookup "name" o of
    Just (AE.String s) -> s
    _ -> "Unknown"
  _ -> "Unknown"


getServiceColor :: Text -> HashMap Text Text -> Text
getServiceColor s serviceColors = fromMaybe "bg-black" $ HM.lookup s serviceColors


atMapText :: Text -> Maybe (Map Text AE.Value) -> Maybe Text
atMapText key maybeMap = do
  m <- maybeMap
  v <- Map.lookup key m
  case v of
    AE.String t -> Just t
    AE.Number n -> Just $ show n
    _ -> Nothing


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


data MetricNode = MetricNode
  { parent :: Text
  , current :: Text
  }
  deriving (Eq, Show)


data MetricTree = MetricTree
  { spanRecord :: MetricNode
  , children :: [MetricTree]
  }
  deriving (Generic, Show)


pathToNodes :: Text -> [MetricNode]
pathToNodes path =
  let segments = T.splitOn "." path
   in zipWith toNode ("___root___" : scanl1 combine segments) segments
  where
    combine acc segment = acc <> "." <> segment
    toNode = MetricNode


buildMetricNodes :: [Text] -> [MetricNode]
buildMetricNodes = concatMap pathToNodes


buildMetricMap :: [MetricNode] -> Map (Maybe Text) [MetricNode]
buildMetricMap = Relude.foldr insertNode Map.empty
  where
    insertNode :: MetricNode -> Map (Maybe Text) [MetricNode] -> Map (Maybe Text) [MetricNode]
    insertNode sp m =
      let key = if parent sp == "___root___" then Nothing else Just (parent sp)
          newEntry = [sp]
       in Map.insertWith
            (\new old -> if sp `elem` old then old else new ++ old)
            key
            newEntry
            m


buildTree :: Map (Maybe Text) [MetricNode] -> Maybe Text -> [MetricTree]
buildTree metricMap parentId =
  case Map.lookup parentId metricMap of
    Nothing -> []
    Just metrics ->
      [ MetricTree
          MetricNode
            { parent = mt.parent
            , current = mt.current
            }
          (buildTree metricMap (if mt.parent == "___root___" then Just mt.current else Just $ mt.parent <> "." <> mt.current))
      | mt <- metrics
      ]


buildMetricTree :: [Text] -> [MetricTree]
buildMetricTree metrics =
  let metricsNodes = buildMetricNodes metrics
      metricMap = buildMetricMap metricsNodes
   in buildTree metricMap Nothing


metricsTree :: Projects.ProjectId -> V.Vector Telemetry.MetricDataPoint -> Map Text Telemetry.MetricDataPoint -> Html ()
metricsTree pid metrics dp = do
  let tr = buildMetricTree $ V.toList $ (.metricName) <$> metrics
  div_ [class_ "px-4 py-2 flex flex-col gap-2"] do
    forM_ (Relude.zip [0 ..] tr) \(i, c) -> do
      buildTree_ pid c 0 True dp


buildTree_ :: Projects.ProjectId -> MetricTree -> Int -> Bool -> Map Text Telemetry.MetricDataPoint -> Html ()
buildTree_ pid sp level isLasChild dp = do
  let hasChildren = not $ null sp.children
  let paddingLeft = show (35 * level + 46) <> "px)" -- why 35 ? I have no clue
  div_ [class_ "flex items-start w-full relative span-filterble"] do
    when (level /= 0) $ div_ [class_ "w-8 shrink-0 ml-2 h-[1px] mt-2 bg-slate-200"] pass
    unless (level == 0) $ div_ [class_ "absolute -top-3 left-2 border-l h-5 border-l-slate-200"] pass
    unless isLasChild $ div_ [class_ "absolute top-1 left-2 border-l h-full border-l-slate-200"] pass
    div_ [class_ "flex flex-col w-full grow-1 shrink-1 border-slate-200 relative"] do
      when hasChildren $ div_ [class_ "absolute top-1 left-2 border-l h-2 border-l-slate-200"] pass
      div_
        [ class_ "w-full cursor-pointer flex tree_opened justify-between max-w-full items-center h-5 hover:bg-fillWeaker"
        , [__| on click toggle .tree_opened on me|]
        ]
        do
          div_ [class_ "flex w-full justify-between items-center overflow-x-hidden"] do
            div_ [class_ "flex items-center overflow-y-hidden", style_ $ "width: calc(40vw - " <> paddingLeft] do
              when hasChildren $ faSprite_ "chevron-up" "regular" "toggler rotate-90 w-4 border border-slate-200 h-4 shadow-xs rounded-sm px-0.5 z-50 bg-slate-50 mr-1 shrink-0 text-slate-950"
              unless (sp.spanRecord.parent == "___root___") $ span_ [class_ "text-slate-400"] $ toHtml $ sp.spanRecord.parent <> "."
              span_ [class_ "text-slate-900 "] $ toHtml sp.spanRecord.current
              when hasChildren $ span_ [class_ "badge badge-ghost text-xs"] $ toHtml $ show $ length sp.children
            unless hasChildren $ do
              let fullPath = (if sp.spanRecord.parent == "___root___" then "" else sp.spanRecord.parent <> ".") <> sp.spanRecord.current
              let target = Map.lookup fullPath dp
              whenJust target $ \t -> do
                span_ [class_ "w-[10vw] truncate"] $ toHtml $ T.intercalate ", " $ V.toList t.serviceNames
                div_ [class_ "w-[8vw]"] do
                  span_ [class_ "badge badge-ghost"] $ show t.dataPointsCount
            div_ [class_ "flex w-[10vw] items-center text-xs"] do
              div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
                faSprite_ "dashboard" "regular" "w-4 h-4"
                span_ "0"
              div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
                faSprite_ "dashboard" "regular" "w-4 h-4"
                span_ "10"
              div_ [class_ "flex gap-1 items-center badge badge-ghost"] do
                faSprite_ "caution" "regular" "w-4 h-4"
                span_ "5"

      when hasChildren $ do
        div_ [class_ "flex-col hidden children_container gap-2 mt-2"] do
          forM_ (zip [0 ..] sp.children) \(i, c) -> do
            buildTree_ pid c (level + 1) (i == length sp.children - 1) dp
