module Models.Telemetry.DeterministicIdSpec (spec) where

import Data.Aeson qualified as AE
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Vector qualified as V
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), mintOtelLogIds)
import Pkg.DeriveUtils (AesonText (..))
import Relude
import Test.Hspec

-- Deterministic, content-derived row ids (Telemetry.deterministicOtelId): the
-- guard against the 06-19 duplicate pile-up — reprocessing a dead-letter message
-- must re-derive the SAME id so TF's (id, timestamp) dedup collapses it instead
-- of appending a fresh random row (the old v4 behaviour).
idOf :: OtelLogsAndSpans -> Text
idOf r = (V.head (mintOtelLogIds (V.singleton r))).id

t0, t1 :: UTCTime
t0 = UTCTime (fromGregorian 2026 6 19) (secondsToDiffTime 100)
t1 = UTCTime (fromGregorian 2026 6 19) (secondsToDiffTime 200)

-- Minimal record; only `id` (always overwritten) and identity-bearing fields matter.
base :: OtelLogsAndSpans
base =
  OtelLogsAndSpans
    { project_id = "proj-1", id = "", timestamp = t0, observed_timestamp = Nothing, context = Nothing, level = Nothing
    , severity = Nothing, body = Nothing, attributes = Nothing, resource = Nothing, hashes = Nothing, kind = Nothing
    , status_code = Nothing, status_message = Nothing, start_time = t0, end_time = Nothing, events = Nothing, links = Nothing
    , duration = Nothing, name = Nothing, parent_id = Nothing, summary = V.empty, date = t0, errors = Nothing, message_size_bytes = 0
    }

ctx :: Text -> Text -> Maybe Context
ctx tr sp = Just (Context (Just tr) (Just sp) Nothing Nothing Nothing)

jsonBody :: Text -> Maybe (AesonText AE.Value)
jsonBody s = Just (AesonText (AE.String s))

jsonMap :: [(Text, AE.Value)] -> Maybe (AesonText (Map Text AE.Value))
jsonMap kvs = Just (AesonText (Map.fromList kvs))

spec :: Spec
spec = describe "deterministic OTel row id" do
  it "is deterministic: two independently-built identical records get the same id" do
    idOf base{context = ctx "tr" "sp"} `shouldBe` idOf base{context = ctx "tr" "sp"}

  it "spans key on (project, trace_id, span_id): same span id collapses despite attribute drift" do
    let a = base{context = ctx "tr" "sp", attributes = jsonMap [("k", AE.Number 1)]}
        b = base{context = ctx "tr" "sp", attributes = jsonMap [("k", AE.Number 2)]}
    idOf a `shouldBe` idOf b

  it "different span ids get different ids" do
    idOf base{context = ctx "tr" "sp1"} `shouldNotBe` idOf base{context = ctx "tr" "sp2"}

  it "excludes timestamp: same identity at a different time keeps the same id (timestamp is the *other* dedup key)" do
    idOf base{context = ctx "tr" "sp", timestamp = t0} `shouldBe` idOf base{context = ctx "tr" "sp", timestamp = t1}

  it "logs (no span_id) key on content: different body ⇒ different id" do
    idOf base{body = jsonBody "hello"} `shouldNotBe` idOf base{body = jsonBody "world"}

  it "logs: identical content ⇒ same id" do
    idOf base{body = jsonBody "hello", name = Just "log.event"} `shouldBe` idOf base{body = jsonBody "hello", name = Just "log.event"}

  it "logs: same body but different resource ⇒ different id (no over-collapse across sources)" do
    let a = base{body = jsonBody "oom", resource = jsonMap [("host", AE.String "pod-a")]}
        b = base{body = jsonBody "oom", resource = jsonMap [("host", AE.String "pod-b")]}
    idOf a `shouldNotBe` idOf b
