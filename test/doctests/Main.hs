module Main (main) where

import Relude
import Test.DocTest (doctest)

main :: IO ()
main = do
  args <- getArgs
  let extensions =
        [ "-XGHC2021"
        , "-XBlockArguments"
        , "-XDataKinds"
        , "-XMultilineStrings"
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
  -- Filter out test-runner flags not supported by doctest (e.g. --color, --jobs)
  let filteredArgs = filter (\a -> not ("--color" `isPrefixOf` a || "--jobs" `isPrefixOf` a || "--match" `isPrefixOf` a)) args
  doctest $ filteredArgs ++
    [ "-isrc"
    , "-ishared/src"
    , "--fast"
    , "-package monoscope"
    , "-package monoscope-shared"
    , "-hide-package base64-bytestring"
    ] ++ extensions ++ ["src", "shared/src"]
