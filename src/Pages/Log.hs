module Pages.Log (apiLog) where

import Config
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Sequence (mapWithIndex)
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Relude

-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson

apiLog :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
apiLog sess pid = do
  pool <- asks pool
  (project, requests) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      requests <- RequestDumps.selectRequestDumpByProject pid
      pure (project, requests)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Logs"
          }
  pure $ bodyWrapper bwconf $ apiLogsPage pid requests

apiLogsPage :: Projects.ProjectId -> Vector RequestDumps.RequestDumpLogItem -> Html ()
apiLogsPage pid requests =
  section_ [class_ "container mx-auto  px-3 py-10 flex flex-col h-full overflow-hidden"] $ do
    div_ [class_ "card-round bg-white overflow-hidden grow divide-y flex flex-col mb-8 text-sm"] $ do
      div_ [class_ "pl-3 py-2 space-x-5"] $ do
        strong_ "Query results"
        span_ "330 log entries"

      table_ [class_ "table-fixed grow min-w-full h-full divide-y flex flex-col monospace"] $ do
        thead_ [class_ "text-xs bg-gray-100 gray-400"] $ do
          tr_ [class_ "flex flex-row text-left space-x-4"] $ do
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-8"] ""
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-32"] "TIMESTAMP"
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 grow"] "SUMMARY"
        tbody_ [class_ " grow overflow-y-scroll h-full whitespace-nowrap text-sm"] $ do
          requests & traverse_ \req -> do
            tr_
              [ class_ "border-t divide-x space-x-4 flex hover:bg-blue-50 cursor-pointer",
                [__| |]
              ]
              $ do
                td_ [class_ "inline-block p-1 px-2 w-8"] ">"
                td_ [class_ "inline-block p-1 px-2 w-32 overflow-hidden"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" (req ^. #createdAt)
                td_ [class_ "inline-block p-1 px-2  grow overflow-hidden"] $ do
                  span_ [class_ "inline-block bg-green-100 green-800 px-3 rounded-xl monospace"] $ toHtml $ req ^. #method
                  span_ [class_ "inline-block bg-stone-100 stone-800 px-3 rounded-xl monospace"] $ toHtml $ req ^. #urlPath
                  let rawUrl = req ^. #rawUrl
                  let referer = req ^. #referer
                  p_ [class_ "inline-block"] $ toHtml $ [text| raw_url=$rawUrl referer=$referer |]
          tr_ $
            td_ $ do
              "test line"

-- valueToFields :: AE.Value -> [(Text, [AE.Value])]
-- valueToFields value = snd $ valueToFields' value ("", [])
--   where
--     valueToFields' :: AE.Value -> (Text, [(Text, AE.Value)]) -> (Text, [(Text, AE.Value)])
--     valueToFields' (AE.Object v) akk = HM.toList v & foldl' (\(akkT, akkL) (key, val) -> (akkT, snd $ valueToFields' val (akkT <> "." <> key, akkL))) akk
--     valueToFields' (AE.Array v) akk = foldl' (\(akkT, akkL) val -> (akkT, snd $ valueToFields' val (akkT <> ".[]", akkL))) akk v
--     valueToFields' v (akk, l) = (akk, (akk, v) : l)

-- | jsonValueToHtmlTree takes an aeson object and converts it into a html tree
-- each primitive value in the json and the values.
--
-- Regular nested text fields:
-- >>> valueToFields [aesonQQ|{"menu":{"id":"text"}}|]
-- [(".menu.id",[String "text"])]
--
-- Integer nested field within an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":[{"int_field":22}]}}|]
-- WAS [(".menu.id.[].int_field",[Number 22.0])]
-- NOW Not in scope: ‘aesonQQ’
--
-- Deeper nested field with an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":{"menuitems":[{"key":"value"}]}}}|]
-- WAS [(".menu.id.menuitems.[].key",[String "value"])]
-- NOW Not in scope: ‘aesonQQ’
--
-- Flat array value:
-- >>> valueToFields [aesonQQ|{"menu":["abc", "xyz"]}|]
-- WAS [(".menu.[]",[String "abc",String "xyz"])]
-- NOW Not in scope: ‘aesonQQ’
--
-- Float values and Null
-- >>> valueToFields [aesonQQ|{"fl":1.234, "nl": null}|]
-- WAS [(".fl",[Number 1.234]),(".nl",[Null])]
-- NOW Not in scope: ‘aesonQQ’
--
-- Multiple fields with same key via array:
-- >>> valueToFields [aesonQQ|{"menu":[{"id":"text"},{"id":123}]}|]
-- WAS [(".menu.[].id",[String "text",Number 123.0])]
-- NOW Not in scope: ‘aesonQQ’
jsonValueToHtmlTree :: AE.Value -> Html ()
jsonValueToHtmlTree val = div_ $ jsonValueToHtmlTree' ("", val)
  where
    jsonValueToHtmlTree' :: (Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (key, (AE.Object v)) = div_ $ toHtml $ (HM.toList v & mapM_ jsonValueToHtmlTree')
    -- jsonValueToHtmlTree' (key, (AE.Array v)) = div_ $ toHtml $ (Vector.toList v & mapM_ (\i item -> jsonValueToHtmlTree' ((toText $ show i), item)))
    jsonValueToHtmlTree' (key, value) = div_ $ toHtml $ show value
