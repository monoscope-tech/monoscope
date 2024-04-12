module Start (
  startApp,
) where

import Relude
import System.Server qualified as Server
import RustInterop
import Foreign.C.String

-- startApp :: IO ()
-- startApp =  withCString "Rust 🦀" hello

startApp :: IO ()
startApp = Server.runAPItoolkit
