module Pages.CodeContextSpec (spec) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Lucid qualified
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Pages.CodeContext qualified as PageCodeContext
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec


render :: Lucid.Html () -> Text
render = LT.toStrict . Lucid.renderText


spec :: Spec
spec = around withTestResources do
  describe "Source code settings (code mappings)" do
    -- Without a linked repo there is nothing to map onto, so the page must point at the
    -- control that fixes that rather than offer a form whose every submission is discarded.
    it "asks for a repository before it asks for a mapping" \tr -> do
      (_, html) <- testServant tr $ PageCodeContext.codeMappingsGetH testPid
      let out = render html
      out `shouldSatisfy` T.isInfixOf "Link a GitHub repository first"
      out `shouldNotSatisfy` T.isInfixOf "Add mapping"

    it "round-trips a mapping and resolves a frame path through it" \tr -> do
      let encKey = encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey
      syncM <- runQueryEffect tr $ GitSync.insertGitHubSync encKey testPid "acme" "monolith" "main" "ghp_test" Nothing ""
      sync <- maybe (error "failed to create github_sync fixture") pure syncM

      (_, added) <- testServant tr $ PageCodeContext.codeMappingsPostH testPid (PageCodeContext.CodeMappingForm Nothing (Just "/srv/app/") (Just "src"))
      let addedHtml = render added
      addedHtml `shouldSatisfy` T.isInfixOf "/srv/app/"
      addedHtml `shouldSatisfy` T.isInfixOf "acme/monolith@main"

      mappings <- runQueryEffect tr $ CodeContext.getCodeMappings testPid
      map (.githubSyncId) mappings `shouldBe` [sync.id]
      -- The mapping is what turns a build path into a repo path; that rewrite is the whole
      -- feature, so assert it rather than just that a row exists.
      snd <$> CodeContext.resolveRepoPath mappings Nothing "/srv/app/services/checkout.py" `shouldBe` Just "src/services/checkout.py"

      case mappings of
        [cm] -> do
          (_, afterDelete) <- testServant tr $ PageCodeContext.codeMappingsDeleteH testPid cm.id
          render afterDelete `shouldSatisfy` T.isInfixOf "No mappings yet"
          runQueryEffect tr (CodeContext.getCodeMappings testPid) >>= \ms -> length ms `shouldBe` 0
        _ -> expectationFailure $ "expected exactly one mapping, got " <> show (length mappings)

  describe "Frame source endpoint (codeContextH)" do
    -- A project that never configured a mapping opens an error panel like anyone else. It
    -- must see a sentence, not a failure it did not cause — and the sentence has to say
    -- which of the two possible problems it is.
    it "explains an unresolvable frame instead of failing the panel" \tr -> do
      (_, unmapped) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") (Just 88) Nothing
      render unmapped `shouldSatisfy` T.isInfixOf "No code mapping covers this path"

      (_, noLine) <- testServant tr $ PageCodeContext.codeContextH testPid (Just "/srv/app/checkout.py") Nothing Nothing
      render noLine `shouldSatisfy` T.isInfixOf "no file and line"
