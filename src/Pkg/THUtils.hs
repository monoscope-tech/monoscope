module Pkg.THUtils (hashAssetFile, hashFile) where

import Data.Digest.XXHash (xxHash)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Numeric (showHex)
import Relude


-- adds a version hash to file paths, to force cache invalidation when a new version appears
hashAssetFile :: FilePath -> TH.Q TH.Exp
hashAssetFile path = do
  content <- TH.runIO $ readFileLBS ("static" <> path)
  let hash = fromString $ showHex (xxHash content) ""
  [|$(TH.lift path) <> "?v=" <> $(TH.lift (toString hash))|]


hashFile :: FilePath -> TH.Q TH.Exp
hashFile path = do
  content <- TH.runIO $ readFileLBS ("static" <> path)
  let hash = fromString $ showHex (xxHash content) ""
  [|$(TH.lift (toString hash))|]
