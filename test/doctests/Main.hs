module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Data.List (sort)
import Data.Text qualified as T
import Relude
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStr)
import System.Process (readProcessWithExitCode)
import Test.DocTest (doctest)


-- | Recursively collect `.hs` files that contain doctest prompts (`>>>`).
findDoctestFiles :: FilePath -> IO [FilePath]
findDoctestFiles root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure []
    else do
      entries <- sort <$> listDirectory root
      concat <$> forM entries \entry -> do
        let path = root </> entry
        subDir <- doesDirectoryExist path
        if subDir
          then findDoctestFiles path
          else
            if takeExtension path == ".hs"
              then do
                content <- decodeUtf8 @Text @ByteString <$> readFileBS path
                pure [path | any (T.isInfixOf ">>>") (T.lines content)]
              else pure []


-- | Per-package doctest configuration: each package is loaded from source only
-- for its own tree while its upstream local packages come precompiled via
-- `-package`, avoiding GHCi re-interpretation and duplicate doctest runs.
data PkgSuite = PkgSuite
  { pkgName :: String
  , pkgRoot :: FilePath
  , pkgFlags :: [String]
  }


pkgSuites :: [PkgSuite]
pkgSuites =
  [ PkgSuite "shared" "shared/src" ["-ishared/src", "-package monoscope-proto"]
  , PkgSuite "cli" "cli/CLI" ["-icli", "-package monoscope-shared", "-package monoscope-proto"]
  , PkgSuite "src" "src" ["-isrc", "-package monoscope-shared", "-package monoscope-cli", "-package monoscope-proto"]
  ]


parsePkg :: [String] -> (Maybe String, [String])
parsePkg [] = (Nothing, [])
parsePkg ("--pkg" : p : rest) = (Just p, rest)
parsePkg (x : xs) = second (x :) (parsePkg xs)


main :: IO ()
main = do
  args <- getArgs
  let (pkgM, restArgs) = parsePkg args
      filteredArgs = filter (\a -> not ("--color" `isPrefixOf` a || "--jobs" `isPrefixOf` a || "--match" `isPrefixOf` a)) restArgs
      commonFlags =
        [ "--fast"
        , "-hide-package base64-bytestring"
        , "-XGHC2021"
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
  case pkgM of
    Just name ->
      case find ((== name) . pkgName) pkgSuites of
        Nothing -> die ("unknown doctest package: " <> name)
        Just suite -> do
          files <- findDoctestFiles suite.pkgRoot
          unless (null files) $ doctest (filteredArgs ++ suite.pkgFlags ++ commonFlags ++ files)
    Nothing -> do
      exe <- getExecutablePath
      results <- mapConcurrently (\s -> (s.pkgName,) <$> readProcessWithExitCode exe (["--pkg", s.pkgName] ++ filteredArgs) "") pkgSuites
      forM_ results \(name, (rc, out, err)) -> do
        unless (null out) $ putStr out
        when (rc /= ExitSuccess && not (null err)) $ hPutStr stderr ("--- doctest " <> name <> " ---\n" <> err)
      when (any (\(_, (rc, _, _)) -> rc /= ExitSuccess) results) exitFailure
