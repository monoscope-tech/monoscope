module Pkg.EmbeddingFairnessSpec (spec) where

import BackgroundJobs qualified
import Data.Effectful.LLM qualified as LLM
import Data.Pool (withResource)
import Data.Text qualified as T
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Dispatch.Dynamic (interpose)
import Langchain.DocumentLoader.Core qualified as Doc
import Pkg.TestUtils
import Relude
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (threadDelay)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Pattern embedding stage fairness" do
    it "saves log embeddings while error embedding is blocked, and preserves them on cancellation" \tr -> do
      withResource tr.trPool \conn -> do
        void $ PG.execute_ conn [sql|UPDATE apis.error_patterns SET merge_override = TRUE|]
        void $ PG.execute_ conn [sql|UPDATE apis.log_patterns SET merge_override = TRUE|]
        void
          $ PG.execute_
            conn
            [sql|
          INSERT INTO apis.error_patterns (project_id, error_type, message, stacktrace, hash)
          VALUES ('00000000-0000-0000-0000-000000000000', 'EmbeddingStageBlockedError', 'blocked', '', 'embedding-stage-blocked')|]
        void
          $ PG.execute_
            conn
            [sql|
          INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash)
          VALUES ('00000000-0000-0000-0000-000000000000', 'ready log stage', 'embedding-stage-ready')|]
      errorStarted <- newEmptyMVar
      releaseError <- newEmptyMVar
      let provider = interpose @LLM.LLM \_ -> \case
            LLM.EmbedDocuments _ docs
              | any (T.isInfixOf "EmbeddingStageBlockedError" . toStrict . Doc.pageContent) docs ->
                  liftIO $ putMVar errorStarted () >> takeMVar releaseError
              | otherwise -> do
                  readMVar errorStarted
                  pure $ Right $ map (const [1, 0]) docs
            LLM.CallLLM{} -> pure $ Left "No merge judge is needed for this fixture"
            LLM.CallAgenticChat{} -> pure $ Left "No agent is needed for this fixture"
          saved = withResource tr.trPool \conn -> do
            [PG.Only ready] <-
              PG.query_
                conn
                [sql|
              SELECT embedding IS NOT NULL FROM apis.log_patterns WHERE pattern_hash = 'embedding-stage-ready'|]
            pure ready
          waitSaved = saved >>= \ready -> unless ready (threadDelay 10000 >> waitSaved)
      withAsync (runTestBg frozenTime tr $ provider $ BackgroundJobs.patternEmbeddingAndMerge testPid) \_ ->
        timeout 3000000 waitSaved `shouldReturn` Just ()
      saved `shouldReturn` True
      withResource tr.trPool \conn -> do
        [PG.Only untouched] <-
          PG.query_
            conn
            [sql|
          SELECT embedding IS NULL FROM apis.error_patterns WHERE hash = 'embedding-stage-blocked'|]
        untouched `shouldBe` True
