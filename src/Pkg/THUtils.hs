module Pkg.THUtils (hashAssetFile, hashFile, markdown) where

import Data.Digest.XXHash (xxHash)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Lucid
import Numeric (showHex)
import Relude
import Text.MMark qualified as MMark
import Text.Megaparsec qualified as M


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


markdown :: Text -> TH.Q TH.Exp
markdown txt = do
  case MMark.parse "" txt of
    Left bundle -> error (fromString $ M.errorBundlePretty bundle)
    Right r -> do
      let html = renderText $ MMark.render r -- Render MMark to HTML
      [|toHtmlRaw $(TH.lift html)|]
