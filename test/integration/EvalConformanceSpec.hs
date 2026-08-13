-- | Conformance between the two interpreters of 'Pkg.Parser.Expr'.
--
-- Live Tail matches rows in memory with 'Pkg.Parser.Eval.evalExpr'; Events matches the same
-- rows in the database with the SQL that @Display Expr@ emits. The product promise is that one
-- query means one thing in both tabs — and nothing in the types enforces it. Two interpreters
-- over one AST can drift on any single operator, silently, and the symptom a user sees is the
-- same filter returning different rows in two places.
--
-- So each case runs /both ways against the same row/: the evaluator over the decoded record,
-- and the generated @WHERE@ clause over that row as ingest actually stores it. Disagreement is
-- a failure, and the report names the query so a drift points at the operator.
--
-- Deliberately a small table rather than a generator: one case per semantic decision documented
-- in the "Pkg.Parser.Eval" header. Those decisions are what drift; a fuzzer over KQL would
-- mostly re-test the parser.
--
-- The row is written to the flattened columns the SQL reads /and/ the JSON blobs the evaluator
-- resolves through, because that is the duality ingest creates. A row that populated only one
-- of the two would make every case agree for the wrong reason.
module EvalConformanceSpec (spec) where

import Data.Aeson qualified as AE
import Data.Map.Strict qualified as Map
import Data.Text.Display (display)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..), UUIDId (..))
import Pkg.Parser.Eval (evalExpr, filterExpr, resolveIn)
import Pkg.Parser.Stats (parseQueryToAST)
import Pkg.TestUtils (TestResources (..), frozenTime, withTestResources)
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


rowId :: Text
rowId = "11111111-1111-1111-1111-111111111111"


-- | The record the evaluator sees. Nested, as the ingest path decodes it.
record :: Telemetry.OtelLogsAndSpans
record =
  Telemetry.OtelLogsAndSpans
    { id = rowId
    , project_id = pid.toText
    , timestamp = frozenTime
    , observed_timestamp = Nothing
    , context = Nothing
    , level = Just "error"
    , severity = Nothing
    , body = Just (AesonText (AE.String "boom"))
    , attributes =
        Just
          ( AesonText
              ( Map.fromList
                  [
                    ( "http"
                    , AE.object
                        [ "request" AE..= AE.object ["method" AE..= ("GET" :: Text)]
                        , "response" AE..= AE.object ["status_code" AE..= (503 :: Int)]
                        ]
                    )
                  ]
              )
          )
    , resource = Just (AesonText (Map.fromList [("service", AE.object ["name" AE..= ("checkout" :: Text)])]))
    , hashes = Just V.empty
    , kind = Just "log"
    , status_code = Nothing
    , status_message = Nothing -- the IS NULL / IS NOT NULL cases hinge on this
    , start_time = frozenTime
    , end_time = Nothing
    , events = Nothing
    , links = Nothing
    , duration = Just 1
    , name = Just "GET /pay"
    , parent_id = Nothing
    , summary = V.empty
    , date = frozenTime
    , errors = Nothing
    , message_size_bytes = 0
    }


-- | The same row as ingest stores it: JSON blobs plus the flattened projections the SQL side
-- reads for aliased and @attributes.*@ fields.
seedRow :: TestResources -> IO ()
seedRow tr = withPool tr.trPool $ do
  void $ DBT.execute [sql| DELETE FROM otel_logs_and_spans WHERE id = ? |] (Only rowId)
  -- The flattened columns carry literals rather than bound parameters so the whole row lands
  -- in one statement (postgresql-simple's ToRow tuples stop at ten). They are the constants
  -- above, mirrored: the SQL side reads these, the evaluator reads the JSON blobs, and a row
  -- that populated only one of the two would make every case agree for the wrong reason.
  void
    $ DBT.execute
      [sql|
        INSERT INTO otel_logs_and_spans
          (id, project_id, timestamp, start_time, date, summary, message_size_bytes,
           name, kind, level, status_message, attributes, resource,
           resource___service___name,
           attributes___http___request___method,
           attributes___http___response___status_code)
        VALUES (?, ?, ?, ?, ?, '{}', 0, ?, ?, ?, NULL, ?, ?, 'checkout', 'GET', 503)
      |]
      ( rowId
      , pid.toText
      , frozenTime
      , frozenTime
      , frozenTime
      , record.name
      , record.kind
      , record.level
      , AesonText (maybe mempty (\(AesonText m) -> m) record.attributes)
      , AesonText (maybe mempty (\(AesonText m) -> m) record.resource)
      )


-- | One case per documented semantic decision.
cases :: [Text]
cases =
  [ "level == \"error\"" -- exact match…
  , "level == \"ERROR\"" -- …case-sensitive, so this must not match
  , "name contains \"/PAY\"" -- text operators are case-insensitive
  , "name startswith \"get\""
  , "name !endswith \"/checkout\""
  , "name !contains \"nope\""
  , "attributes.http.response.status_code >= 500" -- flattened numeric column
  , "attributes.http.request.method == \"GET\"" -- flattened text column
  , "service == \"checkout\"" -- output alias → resource___service___name
  , "status_message == null" -- IS NULL
  , "status_message != null" -- IS NOT NULL
  , "kind != \"span\"" -- present field, negated
  , "level in (\"error\", \"warn\")"
  , "level !in (\"warn\")"
  , "level has \"err\"" -- has is a substring match here, matching the SQL
  ]


-- | The in-memory verdict. A query that fails to compile is reported as such rather than
-- silently counting as False, which would make a parser regression look like agreement.
evalVerdict :: Text -> Either Text Bool
evalVerdict q = do
  sections <- parseQueryToAST q
  e <- maybeToRight "not a filter" (filterExpr sections)
  first show (evalExpr (resolveIn (AE.toJSON record)) e)


-- | The database's verdict on the same row: does the generated WHERE clause select it?
sqlVerdict :: TestResources -> Text -> IO (Either Text Bool)
sqlVerdict tr q = case clauseFor q of
  Left err -> pure (Left err)
  Right clause -> do
    -- The clause is interpolated, not bound: it *is* SQL, generated from the same AST the
    -- evaluator ran. Test-only, and the inputs are the literals in `cases` below.
    rows <-
      withPool tr.trPool
        $ DBT.query_
          (fromString (toString ("SELECT count(*) FROM otel_logs_and_spans WHERE id = '" <> rowId <> "' AND (" <> clause <> ")")))
    pure $ Right $ case rows of
      [Only (n :: Int64)] -> n > 0
      _ -> False
  where
    clauseFor s = display <$> (maybeToRight "not a filter" . filterExpr =<< parseQueryToAST s)



spec :: Spec
spec = around withTestResources $ describe "EvalSqlConformance" do
  it "agrees with the generated SQL on every documented semantic decision" \tr -> do
    seedRow tr
    results <- forM cases \q -> do
      sqlV <- sqlVerdict tr q
      pure (q, evalVerdict q, sqlV)
    -- Reported as a list of (query, in-memory, sql) so a drift names the operator that broke
    -- rather than just failing.
    [(q, e, s) | (q, e, s) <- results, e /= s] `shouldBe` []

    -- Agreement is only evidence if both sides actually decided something. Without this, a
    -- table where every case failed to parse would agree on `Left` for all fifteen and pass
    -- while testing nothing — the exact way a conformance test rots into decoration.
    let verdicts = [v | (_, v, _) <- results]
    (Right True `elem` verdicts, Right False `elem` verdicts) `shouldBe` (True, True)
    filter isLeft verdicts `shouldBe` []
