module Start (
  startApp,
) where

import Prelude
import System.Server qualified as Server


startApp :: IO ()
startApp = Server.runAPItoolkit
