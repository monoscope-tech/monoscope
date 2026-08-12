module Pages.CodeContextSpec (spec) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Lucid qualified
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Pkg.Git qualified as Git
import Pages.CodeContext qualified as PageCodeContext
import Pages.Components (stackTrace_)
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec


render :: Lucid.Html () -> Text
render = LT.toStrict . Lucid.renderText


-- | Regression: the revision a span reports lives in its /resource/, not its attributes.
--
-- 'revisionFor' was handed the span-attribute lookup, which structurally cannot hold
-- @service.version@ (ProcessMessage writes it to @resource@, and every other reader —
-- 'Telemetry.spanServiceName', the @resource___service___version@ column, the
-- @resource.service.version@ facet — reads it there). So the snippet URL never carried a
-- revision and every frame rendered at the mapping's branch: today's source, silently, which
-- is wrong exactly when someone is debugging an old release. The doctests could not catch it
-- because they pass 'revisionFor' a hand-built lookup; only the wiring was wrong.
revisionWiringSpec :: Spec
revisionWiringSpec = describe "stack frame source URL" do
  let pyStack = "Traceback (most recent call last):\n  File \"/srv/app/checkout.py\", line 88, in charge\n    raise ValueError(x)"
      lookupOf kvs k = listToMaybe [v | (k', v) <- kvs, k' == k]
      urlFor spanAttrs resourceAttrs = render $ stackTrace_ testPid (Just "checkout") (lookupOf spanAttrs) (lookupOf resourceAttrs) pyStack

  -- The needle omits the leading @&@ on purpose: Lucid escapes it to @&amp;@ inside an
  -- attribute, so matching on "&revision=" never fires — which would have made the two
  -- negative cases below pass whether or not the bug was fixed.
  it "carries the revision the span's resource reports" do
    urlFor [] [("service.version", "1c0ffee")] `shouldSatisfy` T.isInfixOf "revision=1c0ffee"

  it "does not mistake a span attribute for the resource's revision" do
    -- The whole bug: a sha in the wrong map must not be read, or the fix is untested.
    urlFor [("service.version", "1c0ffee")] [] `shouldNotSatisfy` T.isInfixOf "revision="

  it "omits the revision when the resource reports a version that is not a sha" do
    urlFor [] [("service.version", "v2.3.1")] `shouldNotSatisfy` T.isInfixOf "revision="


spec :: Spec
spec = do
  revisionWiringSpec
  around withTestResources do
    describe "Source code settings (code mappings)" do
      -- Without a credential there is nothing to read repos through, so the page must point at
      -- the control that fixes that rather than offer a form whose every submission is discarded.
      it "asks for a GitHub connection before it asks for a mapping" \tr -> do
        (_, html) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid Nothing
        let out = render html
        out `shouldSatisfy` T.isInfixOf "Connect GitHub"
        -- The install has to come back here, not to dashboard sync: they are one grant but
        -- two destinations, and landing on the wrong one is how the old flow made source
        -- context look like it required configuring YAML sync.
        out `shouldSatisfy` T.isInfixOf "git-sync/install?to=code"
        out `shouldNotSatisfy` T.isInfixOf "Link repository"

      it "maps several services onto several repos, and resolves each frame to its own" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        -- The config-sync repo. It is the project's monoscope YAML, NOT where the services live —
        -- the whole point of 0124 is that these are different repositories.
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "monoscope-config" "main" "ghp_test" Nothing ""

        let add repo svc prefix root =
              testServant tr $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm (Just repo) (Just "main") svc Nothing (Just prefix) (Just root))
        _ <- add "checkout-service" (Just "checkout") "/srv/app/" "src"
        (_, added) <- add "billing-service" (Just "billing") "/opt/billing/" ""

        -- The account came from the installation already granted for config sync; the repos did
        -- not, and neither is the config repo.
        let out = render added
        out `shouldSatisfy` T.isInfixOf "acme/checkout-service"
        out `shouldSatisfy` T.isInfixOf "acme/billing-service"
        out `shouldNotSatisfy` T.isInfixOf "monoscope-config"
        -- The row has to say what the mapping covers in frame-path terms, not print the four
        -- columns it is stored as.
        out `shouldSatisfy` T.isInfixOf "frames under /srv/app/"

        mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
        sort (map (.repo) mappings) `shouldBe` ["billing-service", "checkout-service"]

        -- The rewrite is the whole feature, and it must send each service's frame to its own
        -- repo rather than to whichever mapping sorted first.
        let repoFor svc path = (\(cm, p) -> (cm.repo, p)) <$> CodeContext.resolveRepoPath mappings (Just svc) path
        repoFor "checkout" "/srv/app/services/checkout.py" `shouldBe` Just ("checkout-service", "src/services/checkout.py")
        repoFor "billing" "/opt/billing/invoice.rb" `shouldBe` Just ("billing-service", "invoice.rb")
        -- A service whose frames no mapping claims resolves to nothing rather than to the other
        -- service's repo — a snippet from the wrong repo is worse than no snippet.
        repoFor "search" "/srv/search/index.go" `shouldBe` Nothing

        case mappings of
          (cm : _) -> do
            (_, afterDelete) <- testServant tr $ PageCodeContext.codeMappingsDeleteH testPid cm.id
            render afterDelete `shouldNotSatisfy` T.isInfixOf (cm.owner <> "/" <> cm.repo)
            runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \ms -> length ms `shouldBe` 1
          [] -> expectationFailure "expected two mappings, got none"

      -- A frame that no mapping covers is exactly when someone needs the mapping form, and
      -- the one thing they cannot be expected to retype is the path they were just looking at.
      it "carries the unmapped frame's path into the form" \tr -> do
        let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
        _ <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid Git.GitHub Nothing "acme" "monoscope-config" "main" "ghp_test" Nothing ""

        (_, unmapped) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        render unmapped `shouldSatisfy` T.isInfixOf "code-mappings?sample=/srv/app/checkout.py"

        (_, form) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid (Just "/srv/app/checkout.py")
        render form `shouldSatisfy` T.isInfixOf "value=\"/srv/app/checkout.py\""

    describe "Frame source endpoint (codeContextH)" do
      -- A project that never configured a mapping opens an error panel like anyone else. It
      -- must see a sentence, not a failure it did not cause — and the sentence has to say
      -- which of the two possible problems it is.
      it "explains an unresolvable frame instead of failing the panel" \tr -> do
        (_, unmapped) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing Nothing
        render unmapped `shouldSatisfy` T.isInfixOf "No linked repository covers /srv/app/checkout.py"

        (_, noLine) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") Nothing Nothing Nothing
        render noLine `shouldSatisfy` T.isInfixOf "no file and line"
