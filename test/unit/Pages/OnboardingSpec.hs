module Pages.OnboardingSpec (spec) where

import Data.Text qualified as T
import Models.Projects.GitSync (generateAppJWT)
import Models.Projects.OnboardingInstrument qualified as OI
import Pkg.OnboardingDiscovery qualified as Discovery
import Pkg.OnboardingInstrument qualified as Instrument
import Relude
import Test.Hspec


-- | Tailwind cannot see the language guide's visibility class: 'Pages.Onboarding'
-- builds it by concatenation as
-- @"group-has-[#check-" <> l.slug <> ":checked]/pg:block"@, and the scanner only
-- reads literal text. Those rules exist in the built CSS solely because a comment
-- block in the same file spells every one out in full, marked "DO NOT DELETE".
spec :: Spec
spec = do
  describe "Onboarding language guides" do
    it "everyLanguageSlug_hasAScannerVisibleTailwindClass" do
      src <- decodeUtf8 @Text <$> readFileBS "src/Pages/Onboarding.hs"
      let occurrencesAfter tok = drop 1 $ T.splitOn tok src
          slugs = ordNub [T.takeWhile (/= '"') s | s <- occurrencesAfter "Language \""]
          spelledOut = ordNub [T.takeWhile (/= ':') s | s <- occurrencesAfter "group-has-[#check-"]
      length slugs `shouldSatisfy` (> 5)
      length spelledOut `shouldSatisfy` (> 5)
      sort (filter (`notElem` spelledOut) slugs) `shouldBe` []

  describe "Onboarding smart URL discovery" do
    it "normalizes bare domains and rejects private/loopback URLs" do
      Discovery.normalizeTargetUrl "acme.com" `shouldBe` Just "https://acme.com"
      Discovery.normalizeTargetUrl "  https://api.acme.com/health  " `shouldBe` Just "https://api.acme.com/health"
      Discovery.normalizeTargetUrl "http://127.0.0.1:8080/health" `shouldBe` Nothing
      Discovery.normalizeTargetUrl "" `shouldBe` Nothing

    it "deduplicates root slash in candidate URLs" do
      take 3 (Discovery.candidateUrls "https://acme.com")
        `shouldBe` ["https://acme.com/", "https://acme.com/health", "https://acme.com/healthz"]

    it "ranks JSON health endpoints above root HTML and filters SPA catch-all duplicates" do
      let p u path st ct body = Discovery.ProbeResult u path (Just st) (Just 40) ct body Nothing
          spaHtml = "<!doctype html><html><body><div id='root'></div></body></html>"
          probes =
            [ p "https://app.acme.com/" "/" 200 "text/html" spaHtml
            , p "https://app.acme.com/status" "/status" 200 "text/html" spaHtml
            , p "https://app.acme.com/health" "/health" 200 "application/json" "{\"status\":\"ok\"}"
            , p "https://app.acme.com/ping" "/ping" 404 "text/plain" "Not Found"
            ]
          ranked = Discovery.rankEndpointsDeterministic "https://app.acme.com/" probes
      map (.url) ranked `shouldBe` ["https://app.acme.com/health", "https://app.acme.com/"]
      map (.recommended) ranked `shouldBe` [True, True]

    it "extracts public app URLs while ignoring doc/code hosting noise" do
      Discovery.extractUrlsFromText "API: https://api.acme.com/health Docs: https://github.com/acme/repo Local: http://localhost:3000"
        `shouldBe` ["https://api.acme.com/health"]

  describe "Onboarding stack detection" do
    it "detects backend, frontend RUM, and Kubernetes stacks from repo paths" do
      Instrument.detectStackFromPaths ["package.json", "next.config.ts", "src/app/page.tsx"] `shouldBe` "Next.js (TypeScript / Frontend + API)"
      Instrument.detectStackFromPaths ["vite.config.ts", "index.html"] `shouldBe` "Vite (Frontend Web / RUM)"
      Instrument.detectStackFromPaths ["Chart.yaml", "templates/deployment.yaml"] `shouldBe` "Kubernetes (Helm)"
      Instrument.detectStackFromPaths ["go.mod", "main.go"] `shouldBe` "Go"

  describe "GitHub App authentication" do
    it "generateAppJWT_acceptsRawPEM" do
      pem <- decodeUtf8 @Text <$> readFileBS "test/fixtures/github-app-test.pem"
      generateAppJWT "123" pem >>= \case
        Left err -> expectationFailure $ "raw PEM should be accepted: " <> toString err
        Right _ -> pure ()
