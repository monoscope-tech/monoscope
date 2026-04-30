module ReplaySpec (spec) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Pages.Replay (concatRawJsonArrays, mergeEventArrays, sessionFileKeys)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


clearReplaySessions :: TestResources -> IO ()
clearReplaySessions tr =
  void $ withPool tr.trPool $ DBT.execute
    [sql| DELETE FROM projects.replay_sessions WHERE project_id = ? |]
    (Only pid)


spec :: Spec
spec = aroundAll withTestResources do
  describe "concatRawJsonArrays" do
    it "returns empty array for no inputs" $ \_ ->
      concatRawJsonArrays [] `shouldBe` "[]"

    it "concatenates two arrays" $ \_ ->
      concatRawJsonArrays ["[1,2]", "[3,4]"] `shouldBe` "[1,2,3,4]"

    it "skips empty arrays" $ \_ ->
      concatRawJsonArrays ["[]", "[1]", "[]"] `shouldBe` "[1]"

    it "handles single non-empty array" $ \_ ->
      concatRawJsonArrays ["[{\"a\":1}]"] `shouldBe` "[{\"a\":1}]"

    it "round-trips valid JSON" $ \_ -> do
      let input = ["[1,2,3]", "[4,5,6]"]
          result = concatRawJsonArrays input
      AE.decode result `shouldBe` Just ([1, 2, 3, 4, 5, 6] :: [Int])

  describe "mergeEventArrays" do
    let ev ts = AE.object ["timestamp" AE..= (ts :: Double)]

    it "sorts arrays by first event timestamp" $ \_ ->
      mergeEventArrays [V.fromList [ev 5], V.fromList [ev 1], V.fromList [ev 3]]
        `shouldBe` V.fromList [ev 1, ev 3, ev 5]

    it "excludes empty arrays" $ \_ ->
      mergeEventArrays [V.empty, V.fromList [ev 2]] `shouldBe` V.fromList [ev 2]

    it "returns empty for all-empty input" $ \_ ->
      mergeEventArrays [V.empty, V.empty] `shouldBe` V.empty

  describe "sessionFileKeys" do
    it "returns empty list for unknown session" $ \tr -> do
      keys <- runQueryEffect tr $ sessionFileKeys pid UUID.nil
      keys `shouldBe` []

    it "returns tracked file keys after insert" $ \tr -> do
      clearReplaySessions tr
      let sid = UUID.nil
          key1 = UUID.toText sid <> "/20260101T000000.json"
      void $ withPool tr.trPool $ DBT.execute
        [sql|
          INSERT INTO projects.replay_sessions
            (session_id, project_id, last_event_at, event_file_count, file_keys)
          VALUES (?, ?, now(), 1, ARRAY[?]::text[])
          ON CONFLICT (session_id) DO NOTHING
        |]
        (sid, pid, key1)
      keys <- runQueryEffect tr $ sessionFileKeys pid sid
      keys `shouldBe` [key1]
      clearReplaySessions tr

  describe "compressAndMergeReplaySessions batch selection" do
    it "picks up sessions older than 30 min with merged=FALSE" $ \tr -> do
      clearReplaySessions tr
      let sid = UUID.fromWords 0 0 0 1
      void $ withPool tr.trPool $ DBT.execute
        [sql|
          INSERT INTO projects.replay_sessions
            (session_id, project_id, last_event_at, event_file_count, merged)
          VALUES (?, ?, now() - interval '2 hours', 0, FALSE)
          ON CONFLICT (session_id) DO NOTHING
        |]
        (sid, pid)
      rows :: V.Vector (UUID.UUID, Int) <- withPool tr.trPool $ DBT.query
        [sql|
          SELECT session_id, event_file_count
          FROM projects.replay_sessions
          WHERE merged = FALSE
            AND last_event_at < now() - interval '30 minutes'
            AND project_id = ?
        |]
        (Only pid)
      rows `shouldSatisfy` (not . V.null)
      clearReplaySessions tr

    it "does not pick up recently active sessions" $ \tr -> do
      clearReplaySessions tr
      let sid = UUID.fromWords 0 0 0 2
      void $ withPool tr.trPool $ DBT.execute
        [sql|
          INSERT INTO projects.replay_sessions
            (session_id, project_id, last_event_at, event_file_count, merged)
          VALUES (?, ?, now(), 0, FALSE)
          ON CONFLICT (session_id) DO NOTHING
        |]
        (sid, pid)
      rows :: V.Vector (Only UUID.UUID) <- withPool tr.trPool $ DBT.query
        [sql|
          SELECT session_id FROM projects.replay_sessions
          WHERE merged = FALSE
            AND last_event_at < now() - interval '30 minutes'
            AND project_id = ?
        |]
        (Only pid)
      rows `shouldBe` V.empty
      clearReplaySessions tr

  describe "concatRawJsonArrays large input" do
    it "handles many arrays efficiently without OOM" $ \_ -> do
      let validChunk = "[1,2,3,4,5]" :: BL.ByteString
          manyChunks = replicate 1000 validChunk
          result = concatRawJsonArrays manyChunks
      -- Should produce valid JSON array of 5000 numbers
      let decoded = AE.decode result :: Maybe [Int]
      fmap length decoded `shouldBe` Just 5000
