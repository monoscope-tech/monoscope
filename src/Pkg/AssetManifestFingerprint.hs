module Pkg.AssetManifestFingerprint (assetManifestFingerprint) where


-- This development fallback is replaced with the Vite entry filename while
-- building the production image. DeriveUtils references it so a changed
-- manifest invalidates GHC's persistent object cache.
assetManifestFingerprint :: String
assetManifestFingerprint = "development"
