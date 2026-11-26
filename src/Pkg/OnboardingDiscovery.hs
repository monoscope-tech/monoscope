-- | Smart endpoint discovery for onboarding uptime checks.
-- Normalizes a user-supplied domain or URL, safely probes common health/app
-- paths in parallel, and ranks the best endpoints with LLM assistance
-- (falling back deterministically when no LLM is configured).
module Pkg.OnboardingDiscovery (
  ProbeResult (..),
  inferWorkDomain,
  normalizeTargetUrl,
  candidateUrls,
  probeCandidateUrls,
  discoverEndpoints,
  rankEndpointsDeterministic,
  extractUrlsFromText,
) where

import Control.Lens ((.~), (?~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum)
import Data.Effectful.LLM qualified as LLM
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import GHC.Clock (getMonotonicTime)
import Models.Projects.OnboardingInstrument (SuggestedUrl (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types.Header (hContentType)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Network.Wreq qualified as Wreq
import Relude
import UnliftIO.Async (forConcurrently)
import UnliftIO.Exception (tryAny)
import UnliftIO.Timeout (timeout)
import Utils (safeScrapeUrl)


-- $setup
-- >>> import Relude
-- >>> import Models.Projects.OnboardingInstrument (SuggestedUrl (..))


data ProbeResult = ProbeResult
  { url :: Text
  , path :: Text
  , status :: Maybe Int
  , latencyMs :: Maybe Int
  , contentType :: Text
  , bodySnippet :: Text
  , err :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Extract a work/company domain from a user's email address, ignoring consumer providers.
--
-- >>> inferWorkDomain "jane@acme.io"
-- Just "acme.io"
-- >>> inferWorkDomain "tony@gmail.com"
-- Nothing
-- >>> inferWorkDomain "dev@app.acme.com"
-- Just "app.acme.com"
-- >>> inferWorkDomain "not-an-email"
-- Nothing
inferWorkDomain :: Text -> Maybe Text
inferWorkDomain email = case T.splitOn "@" (T.toLower $ T.strip email) of
  [_, dom]
    | not (T.null dom)
    , "." `T.isInfixOf` dom
    , dom `notElem` consumerDomains
    , safeScrapeUrl ("https://" <> dom) ->
        Just dom
  _ -> Nothing
  where
    consumerDomains :: [Text]
    consumerDomains =
      [ "gmail.com"
      , "googlemail.com"
      , "yahoo.com"
      , "yahoo.co.uk"
      , "outlook.com"
      , "hotmail.com"
      , "live.com"
      , "msn.com"
      , "icloud.com"
      , "me.com"
      , "mac.com"
      , "proton.me"
      , "protonmail.com"
      , "pm.me"
      , "aol.com"
      , "fastmail.com"
      , "hey.com"
      , "mail.com"
      , "zoho.com"
      , "yandex.com"
      , "gmx.com"
      , "example.com"
      ]


-- | Normalize a user-entered domain or URL into an https/http URL and verify SSRF safety.
--
-- >>> normalizeTargetUrl "acme.com"
-- Just "https://acme.com"
-- >>> normalizeTargetUrl "  https://api.acme.com/health  "
-- Just "https://api.acme.com/health"
-- >>> normalizeTargetUrl "http://127.0.0.1:8080/health"
-- Nothing
-- >>> normalizeTargetUrl ""
-- Nothing
normalizeTargetUrl :: Text -> Maybe Text
normalizeTargetUrl raw =
  let trimmed = T.strip raw
      withScheme
        | T.null trimmed = ""
        | "http://" `T.isPrefixOf` trimmed || "https://" `T.isPrefixOf` trimmed = trimmed
        | otherwise = "https://" <> trimmed
   in guarded safeScrapeUrl withScheme


-- | Build a bounded, deduplicated list of candidate endpoints on the same origin.
--
-- >>> take 4 (candidateUrls "https://api.acme.com/v1")
-- ["https://api.acme.com/v1","https://api.acme.com/","https://api.acme.com/health","https://api.acme.com/healthz"]
-- >>> take 3 (candidateUrls "https://acme.com")
-- ["https://acme.com/","https://acme.com/health","https://acme.com/healthz"]
candidateUrls :: Text -> [Text]
candidateUrls baseUrl = case parseURI (toString baseUrl) of
  Nothing -> [baseUrl]
  Just uri -> case uriAuthority uri of
    Nothing -> [baseUrl]
    Just auth ->
      let host = toText (uriRegName auth)
          scheme = toText (uriScheme uri)
          port = toText (uriPort auth)
          origin = scheme <> "//" <> host <> port
          canonicalBase = if null (uriPath uri) then origin <> "/" else baseUrl
          paths = ["/", "/health", "/healthz", "/ready", "/status", "/api/health", "/ping"]
          -- When given a bare root domain (e.g. acme.com), also probe common app/api/status subdomains
          isBareRoot = null (uriPort auth) && (null (uriPath uri) || uriPath uri == "/") && length (T.splitOn "." host) == 2
          subdomainProbes =
            if isBareRoot
              then
                filter
                  safeScrapeUrl
                  [ "https://app." <> host <> "/"
                  , "https://api." <> host <> "/"
                  , "https://api." <> host <> "/health"
                  , "https://status." <> host <> "/"
                  ]
              else []
       in ordNub $ canonicalBase : map (origin <>) paths <> subdomainProbes


-- | Probe candidate URLs concurrently with a tight 4s timeout per request.
probeCandidateUrls :: IOE :> es => Text -> Eff es [ProbeResult]
probeCandidateUrls normalizedUrl = liftIO do
  let urls = candidateUrls normalizedUrl
  forConcurrently urls probeOne


probeOne :: Text -> IO ProbeResult
probeOne targetUrl = do
  let path = maybe "/" (toText . (\p -> if null p then "/" else p) . uriPath) $ parseURI (toString targetUrl)
      opts = Wreq.defaults & Wreq.checkResponse ?~ (\_ _ -> pass) & Wreq.redirects .~ 3
  t0 <- getMonotonicTime
  res <- timeout 4_000_000 $ tryAny $ Wreq.getWith opts (toString targetUrl)
  t1 <- getMonotonicTime
  let ms = max 1 $ round ((t1 - t0) * 1000)
  pure case res of
    Nothing -> ProbeResult targetUrl path Nothing Nothing "" "" (Just "Request timed out after 4s")
    Just (Left err) -> ProbeResult targetUrl path Nothing Nothing "" "" (Just $ friendlyHttpError err)
    Just (Right resp) ->
      let code = resp ^. Wreq.responseStatus . Wreq.statusCode
          ct = decodeUtf8 $ fromMaybe "" $ resp ^? Wreq.responseHeader hContentType
          body = T.take 260 $ decodeUtf8 (LBS.take 512 $ resp ^. Wreq.responseBody)
       in ProbeResult targetUrl path (Just code) (Just ms) ct body Nothing


friendlyHttpError :: SomeException -> Text
friendlyHttpError ex = case fromException ex of
  Just (HC.HttpExceptionRequest _ content) -> case content of
    HC.ResponseTimeout -> "Timed out waiting for response"
    HC.ConnectionTimeout -> "Connection timed out"
    HC.ConnectionFailure _ -> "Could not connect (DNS or host unreachable)"
    HC.InternalException _ -> "TLS or connection error"
    _ -> "Request failed"
  _ -> "Could not reach endpoint"


-- | Probe candidate endpoints and rank them via LLM (with deterministic fallback).
discoverEndpoints :: (IOE :> es, LLM.LLM :> es) => Text -> Eff es ([ProbeResult], [SuggestedUrl])
discoverEndpoints normalizedUrl = do
  probes <- probeCandidateUrls normalizedUrl
  let reachable = filter (isJust . (.status)) probes
  suggestions <-
    if null reachable
      then pure []
      else do
        llmRes <- classifyWithLLM normalizedUrl reachable
        pure $ fromMaybe (rankEndpointsDeterministic normalizedUrl reachable) llmRes
  pure (probes, suggestions)


classifyWithLLM :: LLM.LLM :> es => Text -> [ProbeResult] -> Eff es (Maybe [SuggestedUrl])
classifyWithLLM target probes = do
  let systemPrompt =
        "You are an observability onboarding assistant. Given HTTP probe results for a service, "
          <> "pick the best 1 to 4 endpoints to monitor for uptime. Distinguish real JSON/text healthcheck "
          <> "endpoints from SPA catch-all HTML pages that return 200 on every path, and skip 404s. "
          <> "Respond ONLY with a JSON array of objects: "
          <> "[{\"url\": string, \"name\": string, \"reason\": string, \"expectedStatus\": int, \"recommended\": bool}]. "
          <> "Keep name <= 32 chars and reason <= 48 chars."
      userPrompt = "Target entered: " <> target <> "\nProbe results JSON:\n" <> decodeUtf8 (AE.encode probes)
  LLM.callLLM systemPrompt userPrompt "" <&> \case
    Left _ -> Nothing
    Right raw ->
      let cleaned = LLM.stripCodeBlock raw
       in case AE.eitherDecodeStrict @[LLMEndpoint] (encodeUtf8 cleaned) of
            Left _ -> Nothing
            Right [] -> Nothing
            Right xs ->
              Just
                [ SuggestedUrl
                    { url = e.url
                    , name = e.name
                    , reason = e.reason
                    , expectedStatus = e.expectedStatus
                    , statusCode = matched >>= (.status)
                    , latencyMs = matched >>= (.latencyMs)
                    , recommended = e.recommended
                    }
                | e <- take 5 xs
                , let matched = find ((== e.url) . (.url)) probes
                , isJust matched
                ]


data LLMEndpoint = LLMEndpoint
  { url :: Text
  , name :: Text
  , reason :: Text
  , expectedStatus :: Int
  , recommended :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | Deterministic ranking when LLM is unavailable: prefers real JSON/text health paths
-- over SPA catch-all duplicates, and always keeps the user's primary URL if reachable.
--
-- >>> let p u pt st ct body = ProbeResult u pt (Just st) (Just 45) ct body Nothing
-- >>> map (.url) $ rankEndpointsDeterministic "https://api.acme.com" [p "https://api.acme.com" "/" 200 "text/html" "<html>app</html>", p "https://api.acme.com/health" "/health" 200 "application/json" "{\"ok\":true}", p "https://api.acme.com/ping" "/ping" 404 "text/plain" "not found"]
-- ["https://api.acme.com/health","https://api.acme.com"]
rankEndpointsDeterministic :: Text -> [ProbeResult] -> [SuggestedUrl]
rankEndpointsDeterministic entered probes =
  let okProbes = filter (\p -> maybe False (\c -> c >= 200 && c < 400) p.status) probes
      rootHtml = find (\p -> p.path == "/" && "text/html" `T.isInfixOf` T.toLower p.contentType) okProbes
      isSpaDup p =
        p.path
          /= "/"
          && "text/html"
          `T.isInfixOf` T.toLower p.contentType
          && maybe False (\r -> not (T.null r.bodySnippet) && T.take 80 r.bodySnippet == T.take 80 p.bodySnippet) rootHtml
      distinct = filter (not . isSpaDup) okProbes
      score p =
        ( if isHealthPath p.path && isStructured p
            then 0 :: Int
            else
              if isHealthPath p.path
                then 1
                else
                  if p.url == entered
                    then 2
                    else
                      if p.path == "/"
                        then 3
                        else 4
        , fromMaybe 9999 p.latencyMs
        )
      ordered = take 4 $ sortOn score distinct
      host = maybe "Service" (toText . uriRegName) (parseURI (toString entered) >>= uriAuthority)
   in zipWith (toSuggestion host) [0 :: Int ..] ordered
  where
    isHealthPath pt = any (`T.isInfixOf` T.toLower pt) ["health", "ready", "status", "ping", "live"]
    isStructured p = any (`T.isInfixOf` T.toLower p.contentType) ["json", "text/plain"]
    toSuggestion fallbackHost idx p =
      let pHost = maybe fallbackHost (toText . uriRegName) (parseURI (toString p.url) >>= uriAuthority)
          (label, reason)
            | isHealthPath p.path = (pHost <> " health", "Health check endpoint")
            | p.path == "/" = (pHost, if "app." `T.isPrefixOf` pHost then "Web app" else if "api." `T.isPrefixOf` pHost then "API endpoint" else if "status." `T.isPrefixOf` pHost then "Status page" else "Primary site")
            | otherwise = (pHost <> " " <> p.path, "Reachable endpoint")
       in SuggestedUrl
            { url = p.url
            , name = label
            , reason = reason
            , expectedStatus = fromMaybe 200 p.status
            , statusCode = p.status
            , latencyMs = p.latencyMs
            , recommended = idx < 2
            }


-- | Extract public https:// URLs from repository docs/configs for suggested uptime checks.
--
-- >>> extractUrlsFromText "See https://api.acme.com/health and http://localhost:3000/test or https://github.com/acme/repo"
-- ["https://api.acme.com/health"]
extractUrlsFromText :: Text -> [Text]
extractUrlsFromText txt =
  ordNub
    $ filter (\u -> safeScrapeUrl u && not (isNoiseHost u))
    $ mapMaybe cleanToken (words txt)
  where
    cleanToken w =
      let stripped = T.dropAround (`elem` ("\"'()[]<>`,;*" :: String)) w
       in if "https://" `T.isPrefixOf` stripped
            then Just $ T.dropWhileEnd (\c -> not (isAlphaNum c) && c /= '/') stripped
            else Nothing
    isNoiseHost u =
      any
        (`T.isInfixOf` T.toLower u)
        [ "github.com"
        , "githubusercontent.com"
        , "npmjs.com"
        , "pypi.org"
        , "shields.io"
        , "badge."
        , "example.com"
        , "w3.org"
        , "schema.org"
        , "mozilla.org"
        , "opentelemetry.io"
        , "monoscopehq.com"
        , "localhost"
        ]
