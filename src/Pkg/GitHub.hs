{-# LANGUAGE PackageImports #-}

module Pkg.GitHub (
  generateAppJWT,
  getInstallationToken,
  listInstallationRepos,
  GitHubRepo (..),
  InstallationToken (..),
) where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Effectful.Wreq (HTTP, defaults, getWith, header, postWith, responseBody)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Deriving.Aeson qualified as DAE
import Effectful (Eff, IOE, type (:>))
import Jose.Jwa (JwsAlg (RS256))
import Jose.Jws qualified as Jws
import Jose.Jwt (Jwt (..))
import Relude
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import "crypton-x509" Data.X509 (PrivKey (..))
import "crypton-x509-store" Data.X509.File (readKeyFile)


data GitHubRepo = GitHubRepo
  { id :: Int64
  , name :: Text
  , fullName :: Text
  , private :: Bool
  , defaultBranch :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] GitHubRepo


data InstallationToken = InstallationToken
  { token :: Text
  , expiresAt :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] InstallationToken


data ReposResponse = ReposResponse
  { totalCount :: Int
  , repositories :: [GitHubRepo]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] ReposResponse


-- | Generate a JWT for GitHub App authentication
-- The private key should be base64-encoded PEM format
generateAppJWT :: Text -> Text -> IO (Either Text Text)
generateAppJWT appId privateKeyB64 = do
  now <- round <$> getPOSIXTime :: IO Int64
  let iat = now - 60 -- 1 minute in the past to account for clock drift
      expTime = now + 300 -- 5 minutes from now (GitHub max is 10, using 5 for safety)
      payload =
        AE.encode
          $ AE.object
            [ "iat" AE..= iat
            , "exp" AE..= expTime
            , "iss" AE..= appId
            ]

  case B64.decodeBase64Untyped (encodeUtf8 privateKeyB64) of
    Left err -> pure $ Left $ "Failed to decode base64: " <> err
    Right pemBytes ->
      withSystemTempFile "github-key.pem" $ \tmpPath h -> do
        BS.hPut h pemBytes
        hClose h
        keys <- readKeyFile tmpPath
        case keys of
          [] -> pure $ Left "No private key found in PEM"
          (PrivKeyRSA rsaKey : _) -> do
            result <- Jws.rsaEncode RS256 rsaKey (toStrict payload)
            pure $ case result of
              Left err -> Left $ "Failed to sign JWT: " <> toText (show err)
              Right (Jwt jwtBytes) -> Right $ decodeUtf8 jwtBytes
          _ -> pure $ Left "Unsupported key type (expected RSA)"


-- | Get an installation access token using the App JWT
getInstallationToken :: (HTTP :> es, IOE :> es) => Text -> Text -> Int64 -> Eff es (Either Text InstallationToken)
getInstallationToken appId privateKeyB64 installationId = do
  jwtResult <- liftIO $ generateAppJWT appId privateKeyB64
  case jwtResult of
    Left err -> pure $ Left err
    Right jwt -> do
      let url = "https://api.github.com/app/installations/" <> show installationId <> "/access_tokens"
          opts =
            defaults
              & header "Authorization"
              .~ ["Bearer " <> encodeUtf8 jwt]
                & header "Accept"
              .~ ["application/vnd.github+json"]
                & header "X-GitHub-Api-Version"
              .~ ["2022-11-28"]
                & header "User-Agent"
              .~ ["Monoscope-App"]
      response <- postWith opts (toString url) ("" :: ByteString)
      let body = response ^. responseBody
      case AE.eitherDecode body of
        Left err -> pure $ Left $ "Failed to parse token response: " <> toText err
        Right token -> pure $ Right token


-- | List repositories accessible to the installation
listInstallationRepos :: HTTP :> es => Text -> Eff es (Either Text [GitHubRepo])
listInstallationRepos accessToken = do
  let opts =
        defaults
          & header "Authorization"
          .~ ["Bearer " <> encodeUtf8 accessToken]
            & header "Accept"
          .~ ["application/vnd.github+json"]
            & header "X-GitHub-Api-Version"
          .~ ["2022-11-28"]
            & header "User-Agent"
          .~ ["Monoscope-App"]
  response <- getWith opts "https://api.github.com/installation/repositories?per_page=100"
  let body = response ^. responseBody
  case AE.eitherDecode body of
    Left err -> pure $ Left $ "Failed to parse repos response: " <> toText err
    Right (repos :: ReposResponse) -> pure $ Right repos.repositories
