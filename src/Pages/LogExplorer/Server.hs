module Pages.LogExplorer.Server (server) where

import Models.Projects.Projects qualified as Projects
import Pages.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.LogExplorer.Routes (Routes, Routes' (..))
import Servant qualified
import System.Types (ATAuthCtx)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { logExplorerGet = Log.apiLogH pid
    , logExplorerItemGet = LogItem.apiLogItemH pid
    , logExplorerItemDetailedGet = LogItem.expandAPIlogItemH pid
    }
