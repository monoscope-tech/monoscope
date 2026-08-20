module Pkg.WidgetTooltipSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Pkg.Components.Widget qualified as Widget
import Relude
import Test.Hspec


tooltipField :: Text -> Maybe AE.Value
tooltipField key = do
  AE.Object root <- pure $ Widget.widgetToECharts def
  AE.Object tooltip <- KM.lookup "tooltip" root
  KM.lookup (K.fromText key) tooltip


spec :: Spec
spec = describe "widget tooltip viewport containment" do
  it "renders tooltips inside the chart and confines them to its visible bounds" do
    tooltipField "appendToBody" `shouldBe` Just (AE.Bool False)
    tooltipField "confine" `shouldBe` Just (AE.Bool True)

  it "keeps tall multi-series tooltips scrollable and enterable" do
    tooltipField "enterable" `shouldBe` Just (AE.Bool True)
    tooltipField "extraCssText"
      `shouldBe` Just (AE.String "box-sizing: border-box; max-height: calc(100% - 16px); overflow-y: auto; overscroll-behavior: contain;")
