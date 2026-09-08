module Pkg.UUID (generateNamedV5) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.UUID qualified as UUID
import Relude
import "cryptonite" Crypto.Hash (Digest, SHA1, hash)


-- | UUID v5 with a byte-string name. The uuid package's list API unpacks and
-- repacks the entire name; metric series names can contain large JSON values.
-- SHA1 supplies 20 bytes; UUID v5 uses the first 16, with its version and variant.
--
-- >>> import Data.UUID.V5 qualified as V5
-- >>> import Data.ByteString qualified as BS
-- >>> map (\s -> generateNamedV5 V5.namespaceDNS s == V5.generateNamed V5.namespaceDNS (BS.unpack s)) ["", "www.widgets.com", "a\NULb"]
-- [True,True,True]
generateNamedV5 :: UUID.UUID -> ByteString -> UUID.UUID
generateNamedV5 namespace name =
  UUID.fromWords
    (wordAt 0)
    ((wordAt 4 .&. 0xffff0fff) .|. 0x5000)
    ((wordAt 8 .&. 0x3fffffff) .|. 0x80000000)
    (wordAt 12)
  where
    digest = BA.convert (hash (toStrict (UUID.toByteString namespace) <> name) :: Digest SHA1)
    wordAt offset = BS.foldl' (\word byte -> shiftL word 8 .|. fromIntegral byte) 0 (BS.take 4 $ BS.drop offset digest)
