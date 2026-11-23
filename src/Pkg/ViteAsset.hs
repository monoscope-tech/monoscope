module Pkg.ViteAsset (viteAssetFile) where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Pkg.AssetManifestFingerprint (assetManifestFingerprint)
import Relude


-- | URL of a Vite build output, looked up by manifest key (e.g. @"index.html"@ for the
-- entry). The emitted filename already carries a content hash, so this must NOT get a
-- @?v=@ query on top: chunks import the entry back as a bare @./index.<hash>.js@, and a
-- queried URL is a second module identity — the browser would evaluate the whole graph
-- twice and every @customElements.define@ in it would throw on the second pass.
viteAssetFile :: FilePath -> TH.Q TH.Exp
viteAssetFile key =
  assetManifestFingerprint `seq` do
    let dir = "/public/assets/web-components/dist/"
        manifest = "static" <> dir <> "manifest.json"
    TH.qAddDependentFile manifest
    chunks <- TH.runIO $ AE.eitherDecodeFileStrict' @AE.Object manifest
    either (fail . (("viteAssetFile " <> manifest <> ": ") <>)) (TH.lift . (dir <>) . toString @Text)
      $ chunks
      >>= AET.parseEither (\o -> o AE..: fromString key >>= (AE..: "file"))
