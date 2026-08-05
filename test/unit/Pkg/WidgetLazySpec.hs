-- | Regression guard for the 2026-08-02 dashboard outage.
--
-- Dashboard page render prefilled widgets by querying TimeFusion inline, with no
-- timeout. When TF wedged, every render thread parked on it, the monoscope
-- replicas grew to their 24GB cap and Swarm OOM-killed them in a loop.
--
-- 'lazyWidget' is the escape hatch: past the render budget we drop the prefill and
-- let the browser fetch the data. That only works if a prefill-less widget renders
-- a shell that actually re-fetches, so these tests pin that contract from both
-- sides — the stripping, and the renderer's reaction to it.
module Pkg.WidgetLazySpec (spec) where

import Control.Lens ((&), (.~), (?~))
import Data.Default (def)
import Data.Generics.Labels ()
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lucid (renderText, toHtml)
import Pages.Dashboards (lazyWidget)
import Pkg.Components.Widget qualified as Widget
import Relude
import Test.Hspec


-- | A minimal stat widget. Stat is the interesting case: its renderer decides
-- whether to self-fetch from the widget's own fields rather than emitting a
-- viewport trigger like the table shell does.
statWidget :: Widget.Widget
statWidget = def & #wType .~ Widget.WTStat & #id ?~ "w1" & #title ?~ "Requests"


render :: Widget.Widget -> Text
render = toText . TL.toStrict . renderText . toHtml


-- | Deferred widgets fetch only after entering the viewport.
selfFetches :: Widget.Widget -> Bool
selfFetches = T.isInfixOf "\"intersect once, update-query from:window\"" . render


spec :: Spec
spec = describe "lazyWidget (dashboard render-budget fallback)" do
  it "strips every field that marks a widget as prefilled" do
    let prefilled =
          statWidget
            & #eager ?~ True
            & #html ?~ "<b>cached</b>"
            & #dataset ?~ (def & #value ?~ 42)
        stripped = lazyWidget prefilled
    stripped.eager `shouldBe` Nothing
    stripped.html `shouldBe` Nothing
    -- WidgetDataset has no Eq instance, so assert on the constructor.
    isNothing stripped.dataset `shouldBe` True

  it "defers stat and table fetches until visible" do
    -- The payoff: a degraded widget must come back with a spinner that resolves
    -- once it is visible, without competing with above-fold TimeFusion queries.
    selfFetches (lazyWidget (statWidget & #eager ?~ True)) `shouldBe` True
    selfFetches (def & #wType .~ Widget.WTTable & #id ?~ "w2") `shouldBe` True

  -- The trap this guards. renderStatContent reads `eager` as "data is present" and
  -- drops `load` from the trigger, so clearing html/dataset but LEAVING the flag
  -- renders a spinner that nothing ever resolves. A blown budget would then show a
  -- permanently-loading dashboard instead of a slow one.
  it "would hang if the eager flag were left set" do
    selfFetches (statWidget & #eager ?~ True & #html .~ Nothing & #dataset .~ Nothing) `shouldBe` False
