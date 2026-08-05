module Main (main) where

import CLI.Main (cliMain)
import Paths_monoscope_cli qualified as Paths
import Relude


main :: IO ()
main = cliMain Paths.version
