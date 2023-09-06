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
- from: 2023-06-01 01:00 +0000
  to: 2023-06-20 01:00 +0000
  count: 1000
  method: GET
  duration_to: 100000000
  duration_from: 1000000
  status_codes_oneof: [200,404,203]
  path: /vehicles/query
  path_params: []
  query_params:
    - name: action 
      field_type: "string"
      type_gen_format: "verbs"
      children: []
  request_headers:             
    - name: X-Vehicle-Type 
      field_type: "string"
      type_gen_format: "vehicle"
      children: []
  response_headers: 
    - name: X-Action 
      field_type: "string"
      type_gen_format: "verb"
      children: []
  request_body:
    - name: first_name
      field_type: "string"
      type_gen_format: "first_name"
      children: []
    - name: last_name
      field_type: "string"
      type_gen_format: "last_name"
      children: []
  response_body:
    - name: first_name
      field_type: "string"
      type_gen_format: "first_name"
      children: []
    - name: last_name
      field_type: "string"
      type_gen_format: "last_name"
      children: []
    - name: car 
      field_type: "string"
      type_gen_format: "vehicle"
      children: []
              |]
      generated <- liftIO $ parseConfigToJson (Projects.ProjectId UUID.nil) (encodeUtf8 input)
      -- pTraceShowM generated
      pending
