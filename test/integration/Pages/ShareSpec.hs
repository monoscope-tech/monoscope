module Pages.ShareSpec (spec) where

import Control.Exception (try)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid (renderText, toHtml)
import Pages.Share qualified as Share
import Pkg.TestUtils
import Relude
import Test.Hspec
import Web.ApiHandlers qualified as ApiH


spec :: Spec
spec = around withTestResources do
  describe "Shared Event Lifecycle" do
    it "creates a public view from an ingested event and expires it after 48 hours" \tr -> do
      apiKey <- createTestAPIKey tr testPid "shared-event"
      ingestTrace tr apiKey "shared-checkout" frozenTime
      (eventId, eventTime) <- withResource tr.trPool \conn -> do
        rows <- PGS.query conn
          [sql|SELECT id, timestamp FROM otel_logs_and_spans
               WHERE project_id = ? AND name = 'shared-checkout'
               ORDER BY timestamp DESC LIMIT 1|]
          (PGS.Only testPid)
        maybe (fail "the shared event was not ingested") pure $ listToMaybe rows

      missingEventId <- nextRandom
      beforeInvalid <- countShares tr
      void $ try @SomeException $ testServant tr $ Share.shareLinkPostH testPid missingEventId eventTime (Just "request")
      afterInvalid <- countShares tr
      afterInvalid `shouldBe` beforeInvalid

      runAsBase tr (ApiH.apiShareLinkCreate testPid ApiH.ShareLinkCreate{ApiH.eventId = missingEventId, ApiH.eventCreatedAt = eventTime, ApiH.eventType = Just "log"})
        `shouldThrow` anyException
      afterInvalidApi <- countShares tr
      afterInvalidApi `shouldBe` beforeInvalid

      (_, Share.ShareLinkPost shareIdText) <- testServant tr $ Share.shareLinkPostH testPid eventId eventTime (Just "request")
      shareId <- maybe (fail "the share ID was invalid") pure $ UUID.fromText shareIdText
      live <- runAsBase tr $ Share.shareLinkGetH shareId
      let liveHtml = TL.toStrict $ renderText $ toHtml live
      liveHtml `shouldSatisfy` T.isInfixOf "shared-checkout"
      liveHtml `shouldSatisfy` T.isInfixOf "Expires in"

      apiShare <- runAsBase tr $ ApiH.apiShareLinkCreate testPid ApiH.ShareLinkCreate{ApiH.eventId = eventId, ApiH.eventCreatedAt = eventTime, ApiH.eventType = Just "log"}
      apiLive <- runAsBase tr $ Share.shareLinkGetH apiShare.id
      TL.toStrict (renderText $ toHtml apiLive) `shouldSatisfy` T.isInfixOf "shared-checkout"

      void $ withResource tr.trPool \conn ->
        PGS.execute conn [sql|UPDATE apis.share_events SET created_at = ? WHERE id = ?|] (frozenTime, shareId)
      advanceDays tr 3
      expired <- runAsBase tr $ Share.shareLinkGetH shareId
      let expiredHtml = TL.toStrict $ renderText $ toHtml expired
      expiredHtml `shouldSatisfy` T.isInfixOf "Link expired"
      expiredHtml `shouldSatisfy` not . T.isInfixOf "shared-checkout"


countShares :: TestResources -> IO Int
countShares tr = withResource tr.trPool \conn -> do
  [PGS.Only n] <- PGS.query conn [sql|SELECT count(*) FROM apis.share_events WHERE project_id = ?|] (PGS.Only testPid)
  pure n
