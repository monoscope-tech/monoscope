{-# LANGUAGE OverloadedLabels #-}

module Start (
  startApp,
)
where

import Relude
import System.Server qualified as Server


startApp :: IO ()
startApp = Server.runAPItoolkit
