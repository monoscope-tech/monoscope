module Pages.BodyWrapperSpec (spec) where

import Data.Char (isSpace)
import Data.Default (def)
import Data.Text qualified as T
import Lucid (renderText)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude
import System.Config (DeploymentEnv (..), EnvConfig (..))
import Test.Hspec


-- | The browser SDK is loaded with @defer@, so it executes after the parser finishes —
-- after any inline script in the document. Constructing the client inline therefore threw
-- @Uncaught ReferenceError: Monoscope is not defined@ on every dashboard page load, and the
-- product collected no browser telemetry about itself at all.
--
-- The loader and the constructor also had different render conditions (the constructor
-- omitted @isProd@), so a non-prod deploy holding a telemetry key emitted a constructor with
-- nothing having loaded the name it calls.
spec :: Spec
spec = describe "Browser SDK bootstrap" do
  let shell key monitoring env =
        toStrict
          $ renderText
          $ flip bodyWrapper mempty (def :: BWConfig){config = (def :: EnvConfig){environment = env, enableBrowserMonitoring = monitoring, telemetryApiKey = key, telemetryServiceName = "monoscope-ui"}}

  it "constructsTheClientOnlyAfterTheDeferredBundleHasRun" do
    let html = shell "test-key" True Prod
    html `shouldSatisfy` T.isInfixOf "deps/monoscope/monoscope-"
    html `shouldSatisfy` T.isInfixOf "new Monoscope("
    -- Whitespace-stripped and read in a tight window, because the handler has to be the one
    -- \*enclosing* this call. Searching the whole prefix instead would find the theme script's
    -- own DOMContentLoaded — 457 characters earlier in the real page — and pass on the very
    -- inline-call shape that threw "Monoscope is not defined".
    let compact = T.filter (not . isSpace)
        beforeCall = fst $ T.breakOn "window.monoscope=newMonoscope(" (compact html)
    T.takeEnd 60 beforeCall `shouldSatisfy` T.isInfixOf "addEventListener('DOMContentLoaded',()=>{"

  it "neverEmitsTheConstructorWithoutTheBundleThatDefinesIt" do
    -- No key, monitoring off, and non-prod: each must drop the pair together.
    for_ [shell "" True Prod, shell "test-key" False Prod, shell "test-key" True Dev] \html -> do
      html `shouldNotSatisfy` T.isInfixOf "new Monoscope("
      html `shouldNotSatisfy` T.isInfixOf "deps/monoscope/monoscope-"
