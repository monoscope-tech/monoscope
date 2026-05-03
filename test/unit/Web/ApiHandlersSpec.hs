module Web.ApiHandlersSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Vector qualified as V
import Models.Telemetry.Telemetry qualified as Telemetry
import Pkg.DeriveUtils (AesonText (..))
import Relude
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Web.ApiHandlers (synthStackFromSpans)


-- | Build a minimal OtelLogsAndSpans for the formatter tests. Only the fields
-- the formatter touches (name, status_code, context.span_id,
-- resource."service.name", start_time) are interesting; everything else gets a
-- benign default.
mkSpan
  :: Maybe Text  -- ^ name
  -> Maybe Text  -- ^ span_id
  -> Maybe Text  -- ^ service.name
  -> Maybe Text  -- ^ status_code
  -> UTCTime     -- ^ start_time
  -> Telemetry.OtelLogsAndSpans
mkSpan name spanId service statusCode startTime =
  Telemetry.OtelLogsAndSpans
    { id = ""
    , project_id = ""
    , timestamp = startTime
    , parent_id = Nothing
    , observed_timestamp = Nothing
    , hashes = V.empty
    , name = name
    , kind = Nothing
    , status_code = statusCode
    , status_message = Nothing
    , level = Nothing
    , severity = Nothing
    , body = Nothing
    , duration = Nothing
    , start_time = startTime
    , end_time = Nothing
    , context = (\sid -> Telemetry.Context Nothing (Just sid) Nothing Nothing Nothing) <$> spanId
    , events = Nothing
    , links = Nothing
    , attributes = Nothing
    , resource = (\svc -> AesonText (Map.singleton "service" (AE.Object (AEKM.singleton "name" (AE.String svc))))) <$> service
    , summary = V.empty
    , date = startTime
    , errors = Nothing
    }


t :: Integer -> UTCTime
t s = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime s)


spec :: Spec
spec = describe "synthStackFromSpans" do
  it "returns empty for an empty span list" do
    synthStackFromSpans "trace-1" [] `shouldBe` ""

  it "includes the trace id and a header line" do
    let s = mkSpan (Just "GET /") (Just "abc") (Just "api") Nothing (t 0)
    let out = synthStackFromSpans "trace-xyz" [s]
    out `shouldSatisfy` T.isInfixOf "trace-xyz"
    out `shouldSatisfy` T.isInfixOf "synthesized from trace"

  it "marks errored spans with !! and non-errored with three spaces" do
    let ok = mkSpan (Just "ok") (Just "1") (Just "svc") (Just "OK") (t 0)
    let err = mkSpan (Just "boom") (Just "2") (Just "svc") (Just "ERROR") (t 1)
    let out = synthStackFromSpans "tr" [ok, err]
    out `shouldSatisfy` T.isInfixOf "!! at boom"
    out `shouldSatisfy` T.isInfixOf "   at ok"

  it "orders spans by start_time regardless of input order" do
    let later = mkSpan (Just "later") (Just "L") Nothing Nothing (t 100)
    let earlier = mkSpan (Just "earlier") (Just "E") Nothing Nothing (t 1)
    let out = synthStackFromSpans "tr" [later, earlier]
    -- earlier should appear *before* later in the output: its remaining tail is longer.
    let earlierTail = T.length (snd (T.breakOn "earlier" out))
    let laterTail = T.length (snd (T.breakOn "later" out))
    earlierTail `shouldSatisfy` (> laterTail)

  it "omits the bracketed service suffix when service is missing" do
    let s = mkSpan (Just "n") (Just "s") Nothing Nothing (t 0)
    synthStackFromSpans "tr" [s] `shouldSatisfy` T.isInfixOf "at n (span=s)"

  it "renders <unnamed> when the span has no name" do
    let s = mkSpan Nothing (Just "s") Nothing Nothing (t 0)
    synthStackFromSpans "tr" [s] `shouldSatisfy` T.isInfixOf "at <unnamed>"

  it "renders ? when the span has no context/span_id" do
    let s = mkSpan (Just "n") Nothing Nothing Nothing (t 0)
    synthStackFromSpans "tr" [s] `shouldSatisfy` T.isInfixOf "(span=?)"
