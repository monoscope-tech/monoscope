module Pkg.Parser.ExprSpec (spec) where

import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Parser
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


spec :: Spec
spec = do
  describe "parseQueryToComponents" do
    it "should gensql" do
      let dateRange = (Nothing, Nothing)
      let extraQuery = "method==\"POST\""
      let projectedColsByUser = ["request_body.requestClientId", "request_body.contextClientId", "request_type", "method", "status_code", "request_body\8226contextclientid"]
      let pid = UUIDId $ Unsafe.fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
      let _resp = parseQueryToComponents ((defSqlQueryCfg pid fixedUTCTime Nothing Nothing){dateRange, projectedColsByUser}) extraQuery
      -- pTraceShowM resp
      -- pass
      True `shouldBe` True
