module DataSeedingSpec (spec) where

import DataSeeding
import NeatInterpolation (text)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Process SeedingConfig" $ do
    it "should parse simple config to obj" $ do
      -- let timestamp = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      let input =
            [text|
        - from: 2019-08-31 05:14:37.537084021 UTC
          to: 2019-08-31 05:14:37.537084021 UTC
          intervals: 5min
          count: 2
          path: /test/path
        |]
      let generated = parseConfigToJson (encodeUtf8 input)
      traceShowM generated
      pending
