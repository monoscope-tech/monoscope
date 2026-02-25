module UtilsBenchSpec (spec) where

import Data.Text qualified as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Relude
import Test.Hspec
import Utils (replaceAllFormats)

-- Realistic log lines covering all pattern types the scanner should handle
sampleLines :: [Text]
sampleLines =
  -- UUIDs, IPs, ports, hashes
  [ "User 550e8400-e29b-41d4-a716-446655440000 connected from 192.168.1.50:9876"
  , "GET /api/v2/users/123/orders/456 returned 200"
  , "Error at 192.168.0.1:8080 with status 404"
  , "Hash: a94a8fe5ccb19ba61c4c0873d391e987982fbbd3, Status: 403, Port: :8443"
  , "Connected to 10.0.0.1:443 and 192.168.1.100:22"
  , "Server started on :8080 with PID 12345"
  -- Timestamps (ISO, MySQL, standalone time)
  , "2024-01-15T14:30:00.123Z INFO Processing request 123456"
  , "2023-10-14 10:29:38 ERROR: Connection refused to 10.0.0.5:3306"
  , "[2024-03-01T08:15:22+05:30] WARN Rate limit exceeded client=172.16.0.1"
  , "SELECT * FROM users WHERE id = 42 AND created_at > 2024-01-15 10:30:00"
  -- JWT, email, URL
  , "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.signature sent to user@example.com"
  , "Webhook delivery to https://example.com/hook/abc123 failed with 502"
  , "Authentication failed for admin@company.org from IP 10.0.0.5"
  -- JSON-like structures (colon handling)
  , "{\"status_code\":200,\"duration\":45.67,\"user_id\":\"550e8400-e29b-41d4-a716-446655440000\"}"
  , "{\"http\":{\"request\":{\"method\":\"GET\"},\"response\":{\"status_code\":200}},\"url\":{\"full\":\"http://frontend-proxy:8080/\"}}"
  , "{\"rpc\":{\"grpc\":{\"status_code\":0},\"method\":\"GetCart\",\"service\":\"oteldemo.CartService\"}}"
  -- gRPC/OTEL-style structured logs
  , "outgoing 200 GET GET {\"http\":{\"request\":{\"method\":\"GET\"},\"response\":{\"status_code\":200}},\"url\":{\"full\":\"http://frontend-proxy:8080/\"}}"
  , "INFO \"Product Found\" {\"app\":{\"product\":{\"id\":\"1YMWWN1N1O\",\"name\":\"Eclipsmart Travel Refractor Telescope\"}}}"
  , "internal user_flood_home {\"flood\":{\"count\":5}}"
  -- Kafka, database, k8s
  , "Kafka offset 987654321 partition 3 topic events-production"
  , "Database query took 123ms for table users_sessions with 5000 rows"
  , "pod/api-server-7b8f6c9d4-x2k9m restarted 3 times in namespace production"
  -- MAC, hex, floats, mixed
  , "MAC 00:1A:2B:3C:4D:5E on VLAN 100 with latency 12.5ms"
  , "Memory usage 1048576 bytes, CPU 95.2%, uptime 86400 seconds"
  , "Transaction 0xDEADBEEF failed with hash a94a8fe5ccb19ba61c4c0873d391e987982fbbd3"
  -- Longer log lines (~200+ chars)
  , "[2024-01-15T14:30:00.123Z] [ERROR] Connection to db-master.region.rds.amazonaws.com:5432 failed: timeout after 30000ms (attempt 3/5) trace_id=c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
  , "Request from 172.16.0.1 forwarded to 10.0.0.5:3000 via proxy at 192.168.1.1:8080 with X-Request-ID: 507f1f77bcf86cd799439011 in 45.67ms"
  -- Short lines
  , "Responses: 200, 301, 404, 500"
  , "OK"
  , ""
  ]

spec :: Spec
spec = describe "replaceAllFormats benchmark" $ do
  let rows2k = take 2000 $ cycle sampleLines
      rows20k = take 20000 $ cycle sampleLines
      avgLen = sum (map T.length rows2k) `div` length rows2k

  it ("processes 2,000 rows (avg " <> show avgLen <> " chars) in under 1 second") $ do
    elapsed <- bench rows2k
    putStrLn $ "\n  2k rows: " <> showMs elapsed <> "  (" <> showPerRow elapsed 2000 <> "/row)"
    elapsed `shouldSatisfy` (< 1.0)

  it ("processes 20,000 rows (avg " <> show avgLen <> " chars) in under 5 seconds") $ do
    elapsed <- bench rows20k
    putStrLn $ "\n  20k rows: " <> showMs elapsed <> "  (" <> showPerRow elapsed 20000 <> "/row)"
    elapsed `shouldSatisfy` (< 5.0)

bench :: [Text] -> IO Double
bench rows = do
  start <- getCurrentTime
  let results = map replaceAllFormats rows
  void $ evaluateNF $ foldl' (\acc t -> acc + T.length t) 0 results
  end <- getCurrentTime
  pure $ realToFrac (diffUTCTime end start)

showMs :: Double -> String
showMs s = show (round (s * 1000) :: Int) <> "ms"

showPerRow :: Double -> Int -> String
showPerRow s n = show (round (s / fromIntegral n * 1_000_000) :: Int) <> "μs"
