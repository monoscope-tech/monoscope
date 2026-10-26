module Pages.Rum (SegmentForm (..), segmentsGetH, segmentsPostH, segmentsDeleteH, segmentToolbar_, variablesForTab) where

import Data.Effectful.Hasql qualified as Hasql
import Data.List (lookup)
import Data.Text qualified as T
import Effectful.Error.Static (throwError)
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Pkg.Rum (AudienceFilters (..), audienceFilters, audienceParams)
import Relude
import Servant (err400, errBody)
import System.Types
import Utils (faSprite_, queryStringFrom, toUriStr)
import Web.FormUrlEncoded (FromForm (..), parseUnique)


data SegmentForm = SegmentForm {name :: Text, filters :: AudienceFilters}
  deriving stock (Generic, Show)


instance FromForm SegmentForm where
  fromForm form = SegmentForm <$> parseUnique "name" form <*> fromForm form


variablesForTab :: Dashboards.Dashboard -> Maybe Text -> [Dashboards.Variable]
variablesForTab dash tab = filter (\v -> dash.file /= Just "rum.yaml" || tab == Just "audience" || tab == Just "errors" || v.key == "app") $ fold dash.variables


segmentToolbar_ :: Projects.ProjectId -> [(Text, Maybe Text)] -> Html ()
segmentToolbar_ pid params =
  div_
    [ id_ "rum-segments"
    , hxGet_ $ "/p/" <> pid.toText <> "/rum/segments" <> queryStringFrom params
    , hxTrigger_ "load, update-query from:window"
    , hxVals_ "js:{...Object.fromEntries(new URLSearchParams(location.search))}"
    , hxSwap_ "innerHTML"
    , class_ "px-4 py-2 border-b border-strokeWeak"
    ]
    mempty


segmentsGetH :: Projects.ProjectId -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (Html ()))
segmentsGetH pid params = do
  _ <- Projects.sessionAndProject pid
  segments <- Hasql.interp @[(Text, HI.AsJsonb AudienceFilters)] [HI.sql|SELECT name, filters FROM projects.rum_segments WHERE project_id = #{pid} ORDER BY name|]
  let current = audienceFilters params
      root = "/p/" <> pid.toText <> "/rum"
      window = filter (\(key, _) -> key `elem` ["from", "to", "since", "environment"]) params
      tab = [("tab", Just $ fromMaybe "audience" $ join (lookup "activeTabSlug" params) <|> join (lookup "tab" params))]
  addRespHeaders $ div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-2 text-sm"] do
    nav_ [Aria.label_ "Saved audience segments", class_ "flex flex-wrap items-center gap-2"] do
      span_ [class_ "text-textWeak"] "Segments"
      forM_ segments \(name, HI.AsJsonb filters) -> do
        a_ [href_ $ root <> queryStringFrom (tab <> audienceParams filters <> window), class_ $ "rounded px-2 py-1 hover:bg-fillWeak " <> if filters == current then "bg-fillBrand-weak text-textBrand font-semibold" else "text-textStrong"] $ toHtml name
        button_ [type_ "button", class_ "btn btn-ghost btn-xs", Aria.label_ $ "Delete segment " <> name, hxDelete_ $ root <> "/segments/" <> toUriStr name <> queryStringFrom params, hxTarget_ "#rum-segments", hxSwap_ "innerHTML"] $ faSprite_ "xmark" "regular" "h-3 w-3"
    details_ [class_ "relative"] do
      summary_ [class_ "cursor-pointer text-textBrand"] "Save segment"
      form_ [class_ "absolute z-20 mt-2 p-3 w-64 rounded-lg border border-strokeWeak bg-bgRaised shadow-lg flex flex-col gap-3", hxPost_ $ root <> "/segments" <> queryStringFrom params, hxTarget_ "#rum-segments", hxSwap_ "innerHTML"] do
        label_ [class_ "flex flex-col gap-1"] do
          "Segment name"
          input_ [name_ "name", required_ "", maxlength_ "100", class_ "input input-sm w-full", placeholder_ "Mobile Safari"]
        forM_ ([("app", current.app), ("browser", current.browser), ("os", current.os), ("device", current.device), ("errored", Just $ if current.errored then "true" else "false")] :: [(Text, Maybe Text)]) \(key, value) -> input_ [type_ "hidden", name_ key, value_ $ fromMaybe "" value]
        button_ [type_ "submit", class_ "btn btn-primary btn-sm"] "Save segment"
    a_ [class_ "text-textBrand hover:underline", href_ $ root <> "/sessions" <> queryStringFrom (audienceParams current <> window)] "View sessions"
    a_ [class_ "text-textBrand hover:underline", href_ $ root <> queryStringFrom ([("tab", Just "errors")] <> audienceParams current <> window)] "View errors"
    when (any isJust [current.app, current.browser, current.os, current.device] || current.errored)
      $ a_ [class_ "text-textWeak hover:underline", href_ $ root <> queryStringFrom (tab <> window)] "Clear filters"


segmentsPostH :: Projects.ProjectId -> [(Text, Maybe Text)] -> SegmentForm -> ATAuthCtx (RespHeaders (Html ()))
segmentsPostH pid params form = do
  _ <- Projects.sessionAndProject pid
  let name = T.strip form.name
      filters = audienceFilters $ audienceParams form.filters
  when (T.null name || T.length name > 100) $ throwError err400{errBody = "Segment name must contain 1–100 characters."}
  Hasql.interpExecute_ [HI.sql|INSERT INTO projects.rum_segments (project_id, name, filters) VALUES (#{pid}, #{name}, #{HI.AsJsonb filters}) ON CONFLICT (project_id, name) DO UPDATE SET filters = EXCLUDED.filters|]
  segmentsGetH pid params


segmentsDeleteH :: Projects.ProjectId -> Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (Html ()))
segmentsDeleteH pid name params = do
  _ <- Projects.sessionAndProject pid
  Hasql.interpExecute_ [HI.sql|DELETE FROM projects.rum_segments WHERE project_id = #{pid} AND name = #{name}|]
  segmentsGetH pid params
