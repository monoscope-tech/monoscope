module Pkg.GitCommand (Access (..), command) where

import Data.Base64.Types (extractBase64)
import Relude
import System.Process.Typed (ProcessConfig, nullStream, proc, setEnv, setStderr, setWorkingDir)
import "base64" Data.ByteString.Base64 qualified as Base64


data Access = Local | GitHub (Maybe Text)


command :: [(String, String)] -> Access -> FilePath -> [String] -> ProcessConfig () () ()
command environment access directory args =
  setWorkingDir directory $ setStderr nullStream $ setEnv clean $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "core.fsmonitor=false"] <> args
  where
    protocol = case access of Local -> "file"; GitHub _ -> "https"
    settings =
      [("credential.helper", ""), ("http.followRedirects", "false")] <> case access of
        Local -> []
        GitHub token -> foldMap (\value -> [("http.https://github.com/.extraheader", "AUTHORIZATION: basic " <> toString (extractBase64 $ Base64.encodeBase64 $ encodeUtf8 $ "x-access-token:" <> value))]) token
    clean =
      filter ((== "PATH") . fst) environment
        <> [("LC_ALL", "C"), ("GIT_CONFIG_NOSYSTEM", "1"), ("GIT_CONFIG_GLOBAL", "/dev/null"), ("GIT_TERMINAL_PROMPT", "0"), ("GIT_ALLOW_PROTOCOL", protocol), ("GIT_CONFIG_COUNT", show $ length settings)]
        <> concat [[("GIT_CONFIG_KEY_" <> show index, key), ("GIT_CONFIG_VALUE_" <> show index, value)] | (index, (key, value)) <- zip [0 :: Int ..] settings]
