module Models.Telemetry.TimestampMicrosSpec (spec) where

-- Regression guard for the TS/TF end_time sub-microsecond divergence (parity
-- plan): monoscope built UTCTime from nanoseconds via a lossy Double /1e9 and
-- sent full-precision ISO-8601 text to both engines. Postgres/TimescaleDB
-- *rounds* excess fractional digits on ::timestamptz; DataFusion/TimeFusion
-- *truncates* — so ~39% of rows landed exactly 1µs apart. Fix: exact integer
-- ns→UTC (nanosecondsToUTC) + µs quantization on the wire (roundUTCToMicros).

import Data.Text qualified as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Models.Telemetry.Telemetry (roundUTCToMicros)
import Opentelemetry.OtlpServer (nanosecondsToUTC)
import Relude
import Test.Hspec

-- Exactly the text tsColumn puts on the wire for both engines.
wireText :: Word64 -> Text
wireText = toText . iso8601Show . roundUTCToMicros . nanosecondsToUTC

-- 2026-01-14T21:02:54 + fractional nanoseconds.
base :: Word64
base = 1768424574000000000

spec :: Spec
spec = describe "end_time microsecond parity (TS rounds, TF truncates ISO text)" do
  it "rounds a ≥500ns residue up (the 1µs prod divergence case)" do
    -- .427542900 → residue 900ns rounds UP to .427543 (Postgres behavior)
    wireText (base + 427542900) `shouldBe` "2026-01-14T21:02:54.427543Z"

  it "rounds a <500ns residue down to the same µs" do
    wireText (base + 427542400) `shouldBe` "2026-01-14T21:02:54.427542Z"

  it "never emits sub-microsecond digits, so both engines parse identically" do
    -- fractional digits between '.' and trailing 'Z' (0 when iso8601Show omits them)
    let fracDigits t = T.length (T.drop 1 (snd (T.breakOn "." (T.dropEnd 1 t))))
    forM_ [427542900, 427542400, 1, 999999999, 500] \r ->
      fracDigits (wireText (base + r)) `shouldSatisfy` (<= 6)
