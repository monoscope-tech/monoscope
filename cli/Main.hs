module Main (main) where

import CLI.Main (cliMain)
import Paths_monoscope qualified as Paths
import Relude


main :: IO ()
main = cliMain Paths.version
