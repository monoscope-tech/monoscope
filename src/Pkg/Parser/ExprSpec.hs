module Pkg.Parser.ExprSpec (spec) where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.UUID qualified as UUID
import DataSeeding
import Debug.Pretty.Simple (pTraceShowM)
import Models.Projects.Projects qualified as Projects
import Pkg.Parser
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


fixedUTCTime :: UTCTime
fixedUTCTime = UTCTime (fromGregorian 2020 1 1) (secondsToDiffTime 0)


spec :: Spec
spec = do
  describe "parseQueryToComponents" do
    it "should gensql" do
      let cursorM = Nothing
      let dateRange = (Nothing, Nothing)
      let extraQuery = "method==\"POST\""
      let projectedColsByUser = ["request_body.requestClientId", "request_body.contextClientId", "request_type", "method", "status_code", "request_body\8226contextclientid"]
      let pid = Projects.ProjectId $ Unsafe.fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
      let resp = parseQueryToComponents ((defSqlQueryCfg pid fixedUTCTime){cursorM, dateRange, projectedColsByUser}) extraQuery
      pTraceShowM resp
      -- pass
      True `shouldBe` True
