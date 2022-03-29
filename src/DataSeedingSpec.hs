module DataSeedingSpec (spec) where

import Data.UUID qualified as UUID
import DataSeeding
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "SeedingConfig" $ do
    it "should parse simple config to obj" $ do
      -- let timestamp = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      let input =
            [text|
        - from: 2022-03-01 01:00 +0000
          to: 2022-03-09 01:00 +0000
          count: 2
          method: GET
          duration_to: 500000000
          duration_from: 1000000
          status_codes_oneof: [200,404,203]
          path: /test/path
          path_params: []
          query_params:
            - name: key
              field_type: "string"
              type_gen_format: "address"
              children: []
          request_headers:             
            - name: key
              field_type: "string"
              type_gen_format: "address"
              children: []
          response_headers: 
            - name: key
              field_type: "string"
              type_gen_format: "address"
              children: []
          request_body:
            - name: key
              field_type: "string"
              type_gen_format: "address"
              children: []
          response_body:
            - name: key
              field_type: "string"
              type_gen_format: "address"
              children: []
        |]
      generated <- liftIO $ parseConfigToJson (Projects.ProjectId UUID.nil) (encodeUtf8 input)
      pending
