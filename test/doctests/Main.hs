module Main (main) where

import Relude hiding (getArgs)
import Test.DocTest (doctest)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let extensions = 
        [ "-XGHC2021"
        , "-XBlockArguments"
        , "-XDataKinds"
        , "-XDerivingVia"
        , "-XDeriveAnyClass"
        , "-XDerivingStrategies"
        , "-XDuplicateRecordFields"
        , "-XExplicitNamespaces"
        , "-XExtendedDefaultRules"
        , "-XLambdaCase"
        , "-XMultiWayIf"
        , "-XNoImplicitPrelude"
        , "-XOverloadedLabels"
        , "-XOverloadedLists"
        , "-XOverloadedRecordDot"
        , "-XOverloadedStrings"
        , "-XPatternSynonyms"
        , "-XQuasiQuotes"
        , "-XRoleAnnotations"
        , "-XUndecidableInstances"
        , "-XTypeFamilies"
        , "-XRecordWildCards"
        , "-XTemplateHaskell"
        , "-XAllowAmbiguousTypes"
        , "-XTupleSections"
        , "-XViewPatterns"
        , "-XDeriveGeneric"
        , "-XBangPatterns"
        , "-XPackageImports"
        ]
  doctest $ args ++ 
    [ "-isrc"
    , "--fast"
    ] ++ extensions ++ ["src"]
