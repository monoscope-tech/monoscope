module Pages.Specification.Server (server) where

import Models.Projects.Projects qualified as Projects
import Pages.Specification.Documentation qualified as Documentation
import Pages.Specification.Routes (Routes, Routes' (..))
import Servant qualified
import System.Types (ATAuthCtx)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { documentationPut = Documentation.documentationPutH pid
    , documentationPost = Documentation.documentationPostH pid
    , documentationGet = Documentation.documentationGetH pid
    }
