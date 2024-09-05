module Pages.HashAssets (hashAssetFile) where

import Crypto.Hash (SHA256, hashlazy)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Relude (FilePath, ($), (.), (<>))
import Data.ByteString.Base16 qualified as B16
import Data.Digest.XXHash (xxHash)
import Numeric (showHex)
import Relude

-- adds a version hash to file paths, to force cache invalidation when a new version appears
hashAssetFile :: FilePath -> TH.Q TH.Exp
hashAssetFile path = do
  content <- TH.runIO $ BL.readFile ("static" <> path)
  let hash = fromString $ showHex (xxHash content) ""
  [|$(TH.lift path) <> "?v=" <> $(TH.lift (T.unpack hash))|]
