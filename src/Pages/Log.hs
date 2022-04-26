module Pages.Log (apiLog, apiLogItem) where

import Config
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Sequence (mapWithIndex)
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector, imapM_)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX (hxGet_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Lucid.Hyperscript.QuasiQuoter (_hs)
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

apiLogItem :: Sessions.PersistentSession -> Projects.ProjectId -> UUID.UUID -> DashboardM (Html ())
apiLogItem sess pid rdId = do
  pool <- asks pool
  logItemM <- liftIO $ withPool pool $ RequestDumps.selectRequestDumpByProjectAndId pid rdId
  case logItemM of
    Just logItem -> pure $ apiLogItemView logItem
    Nothing -> pure $ div_ "invalid log request ID"

apiLogsPage :: Projects.ProjectId -> Vector RequestDumps.RequestDumpLogItem -> Html ()
apiLogsPage pid requests =
  section_ [class_ "container mx-auto  px-3 py-10 flex flex-col h-full overflow-hidden"] $
    div_ [class_ "card-round bg-white overflow-hidden grow divide-y flex flex-col mb-8 text-sm"] $ do
      div_ [class_ "pl-3 py-2 space-x-5"] $ do
        strong_ "Query results"
        span_ "330 log entries"

      jsonTreeAuxillaryCode
      table_ [class_ "table-fixed grow min-w-full h-full divide-y flex flex-col monospace"] $ do
        thead_ [class_ "text-xs bg-gray-100 gray-400"] $ do
          tr_ [class_ "flex flex-row text-left space-x-4"] $ do
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-8"] ""
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-32"] "TIMESTAMP"
            th_ [class_ "font-normal inline-block py-1.5 p-1 px-2 grow"] "SUMMARY"
        tbody_ [class_ " grow overflow-y-scroll h-full whitespace-nowrap text-sm divide-y"] $ do
          requests & traverse_ \req -> do
            let logItemPath = RequestDumps.requestDumpLogItemUrlPath pid (req ^. #id)
            tr_
              [ class_ "border-l-4 border-l-transparent divide-x space-x-4 flex hover:bg-blue-50 cursor-pointer",
                term
                  "_"
                  [text|
                  on click 
                    if I match <.expanded-log/> then 
                      log "matched" then
                      remove next <.log-item-info/> then 
                      remove .expanded-log from me
                    else
                      fetch $logItemPath as html then put it after me then
                      add .expanded-log to me
                    end 
                  |]
              ]
              $ do
                td_ [class_ "inline-block p-1 px-2 w-8"] ">"
                td_ [class_ "inline-block p-1 px-2 w-32 overflow-hidden"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" (req ^. #createdAt)
                td_ [class_ "inline-block p-1 px-2  grow overflow-hidden"] $ do
                  span_ [class_ "inline-block bg-green-100 green-800 px-3 rounded-xl monospace"] $ toHtml $ req ^. #method
                  span_ [class_ "inline-block bg-stone-100 stone-800 px-3 rounded-xl monospace"] $ toHtml $ req ^. #urlPath
                  let rawUrl = req ^. #rawUrl
                  let referer = req ^. #referer
                  p_ [class_ "inline-block"] $ toHtml [text| raw_url=$rawUrl referer=$referer |]

apiLogItemView :: RequestDumps.RequestDumpLogItem -> Html ()
apiLogItemView req =
  tr_ [class_ "log-item-info border-l-blue-200 border-l-4"] $
    td_ [class_ "pl-8 py-3"] $ do
      jsonValueToHtmlTree $ AE.toJSON req

jsonValueToHtmlTree :: AE.Value -> Html ()
jsonValueToHtmlTree val = div_ $ jsonValueToHtmlTree' ("", val)
  where
    jsonValueToHtmlTree' :: (Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (key, AE.Object v) = div_ $ do
      a_
        [ class_ "-ml-2 cursor-pointer",
          [__|on click toggle .collapsed on next <div/> from me|]
        ]
        $ do
          span_ [class_ ""] "▸"
          span_ $ toHtml $ if key == "" then "{" else key <> ": {"
      div_ [class_ $ "pl-10 " <> (if key == "" then "" else "collapsed")] $ do
        div_ [class_ "tree-children-count"] $ show $ length $ HM.toList v
        div_ [class_ "tree-children"] $ do
          toHtml (HM.toList v & mapM_ jsonValueToHtmlTree')
      span_ "}"
    jsonValueToHtmlTree' (key, AE.Array v) = div_ $ do
      a_
        [ class_ "-ml-2",
          [__|on click toggle .collapsed on next <div/> from me|]
        ]
        $ do
          span_ [class_ ""] "▸"
          span_ $ toHtml (key <> ": [")
      div_ [class_ "pl-10 "] $ do
        div_ [class_ "tree-children-count"] $ show $ Vector.length v
        div_ [class_ "tree-children"] $ do
          v & imapM_ \i item -> jsonValueToHtmlTree' (toText @String $ show i, item)
      span_ "]"
    jsonValueToHtmlTree' (key, value) = do
      div_ $ do
        span_ [class_ "inline-block"] $ toHtml key
        span_ [class_ "inline-block text-blue-800"] ":"
        a_ [class_ "inline-block hover:bg-blue-50 text-blue-800 ml-2.5 cursor-pointer"] $toHtml $ unwrapJsonPrimValue value

jsonTreeAuxillaryCode :: Html ()
jsonTreeAuxillaryCode = div_ $ do
  script_ [text||]
  style_
    [text|
    .tree-children {
      display: block;
    }
    .tree-children-count { display: none; }
    .collapsed {display: inline-block;padding-left:0; }
    .collapsed .tree-children {
      display: none !important; 
    }
    .collapsed .tree-children-count {display: block !important;}
  |]

unwrapJsonPrimValue :: AE.Value -> Text
unwrapJsonPrimValue (AE.Bool True) = "true"
unwrapJsonPrimValue (AE.Bool False) = "true"
unwrapJsonPrimValue (AE.String v) = "\"" <> toText v <> "\""
unwrapJsonPrimValue (AE.Number v) = toText @String $ show v
unwrapJsonPrimValue AE.Null = "null"
unwrapJsonPrimValue (AE.Object _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only" -- should never be reached
unwrapJsonPrimValue (AE.Array _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only" -- should never be reached
