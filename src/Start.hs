module Start (
  startApp,
) where

import Foreign.C.String
import Relude
import RustInterop
import System.Server qualified as Server


-- startApp :: IO ()
-- startApp =  withCString "Rust 🦀" hello

startApp :: IO ()
startApp = Server.runAPItoolkit
