module Models.Telemetry.DeterministicId (EventIdDomain (..), eventUuid) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Scientific qualified as Scientific
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Data.UUID.V5 qualified as UUIDv5
import Data.Vector qualified as V
import Relude


data EventIdDomain = LegacyRequestSpanId | LegacyRequestTraceId
  deriving stock (Eq, Show)


-- | Derive a UUID from semantic JSON, independent of object-key insertion
-- order. The closed domain selects a distinct compile-time UUID namespace.
eventUuid :: EventIdDomain -> AE.Value -> UUID.UUID
eventUuid domain = UUIDv5.generateNamed (namespace domain) . BSL.unpack . B.toLazyByteString . canonical


namespace :: EventIdDomain -> UUID.UUID
namespace = \case
  LegacyRequestSpanId -> [uuid|c9da4547-5fe7-588a-a87b-61ca5363dcb8|]
  LegacyRequestTraceId -> [uuid|1d33295f-c1de-5e90-b0d5-4d5e80e32998|]


canonical :: AE.Value -> B.Builder
canonical = \case
  AE.Null -> B.word8 0
  AE.Bool False -> B.word8 1
  AE.Bool True -> B.word8 2
  AE.Number n -> B.word8 3 <> bytes (T.encodeUtf8 $ toText $ Scientific.formatScientific Scientific.Generic Nothing $ Scientific.normalize n)
  AE.String s -> B.word8 4 <> bytes (T.encodeUtf8 s)
  AE.Array xs -> B.word8 5 <> count (V.length xs) <> foldMap canonical xs
  AE.Object object ->
    let fields = sortOn (AEK.toText . fst) $ AEKM.toList object
     in B.word8 6 <> count (length fields) <> foldMap (\(key, value) -> bytes (T.encodeUtf8 $ AEK.toText key) <> canonical value) fields
  where
    bytes :: ByteString -> B.Builder
    bytes bs = B.word64BE (fromIntegral $ BS.length bs) <> B.byteString bs
    count :: Int -> B.Builder
    count = B.word64BE . fromIntegral
