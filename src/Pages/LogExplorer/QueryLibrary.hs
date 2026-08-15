module Pages.LogExplorer.QueryLibrary (
  QueryLibraryView (..),
  SaveQueryForm (..),
  queryLibraryH,
  saveQueryH,
  deleteQueryH,
) where

import Data.List qualified as L
import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Vector qualified as V
import Lucid
import Models.Projects.Projects qualified as Projects
import Pkg.Components.LogQueryBox (queryLibraryContent_)
import Pkg.Parser (parseQueryToAST, toQText)
import Relude
import System.Types
import Utils (nonEmptyT)
import Web.FormUrlEncoded (FromForm)


data SaveQueryForm = SaveQueryForm {query :: Maybe Text, queryLibId :: Maybe Text, queryTitle :: Maybe Text}
  deriving stock (Generic)
  deriving anyclass (FromForm)


newtype QueryLibraryView = QueryLibraryView (V.Vector Projects.QueryLibItem, V.Vector Projects.QueryLibItem)


instance ToHtml QueryLibraryView where
  toHtml (QueryLibraryView (saved, recent)) = toHtml do
    queryLibraryContent_ saved recent
    let json = T.replace "<" "\\u003c" $ decodeUtf8 $ AE.encode $ recent <> saved
    script_ $ "window.queryLibraryData=" <> json <> ";document.getElementById('filterElement')?.setQueryLibrary?.(window.queryLibraryData);"
  toHtmlRaw = toHtml


queryLibraryH :: Projects.ProjectId -> ATAuthCtx (RespHeaders QueryLibraryView)
queryLibraryH pid = do
  (sess, _) <- Projects.sessionAndProject pid
  queryLibraryFragment pid sess.persistentSession.userId


saveQueryH :: Projects.ProjectId -> SaveQueryForm -> ATAuthCtx (RespHeaders QueryLibraryView)
saveQueryH pid form = do
  (sess, _) <- Projects.sessionAndProject pid
  let uid = sess.persistentSession.userId
      queryAST = fromRight [] $ parseQueryToAST (maybeToMonoid form.query)
  case (,) <$> nonEmptyT form.queryLibId <*> nonEmptyT form.queryTitle of
    Just (qId, title) -> Projects.queryLibTitleEdit pid uid qId title >> addSuccessToast "Edited Query title successfully" Nothing
    Nothing -> Projects.queryLibInsert Projects.QLTSaved pid uid (toQText queryAST) queryAST form.queryTitle >> addSuccessToast "Saved to Query Library successfully" Nothing
  addTriggerEvent "closeModal" ""
  queryLibraryFragment pid uid


deleteQueryH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders QueryLibraryView)
deleteQueryH pid qId = do
  (sess, _) <- Projects.sessionAndProject pid
  let uid = sess.persistentSession.userId
  Projects.queryLibItemDelete pid uid qId
  addSuccessToast "Deleted from Query Library successfully" Nothing
  queryLibraryFragment pid uid


queryLibraryFragment :: Projects.ProjectId -> Projects.UserId -> ATAuthCtx (RespHeaders QueryLibraryView)
queryLibraryFragment pid uid = do
  queryLib <- Projects.queryLibHistoryForUser pid uid
  let (recent, saved) = bimap V.fromList V.fromList $ L.partition ((== Projects.QLTHistory) . (.queryType)) queryLib
  addRespHeaders $ QueryLibraryView (saved, recent)
