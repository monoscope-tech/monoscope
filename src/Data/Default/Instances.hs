module Data.Default.Instances () where

import Data.Aeson.Types qualified as AET
import Data.Default
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Relude.Unsafe qualified as Unsafe
import Data.CaseInsensitive (CI, FoldCase)
import qualified Data.CaseInsensitive as CI (mk)

import Data.Monoid (Monoid(mempty))

import qualified Data.Text as Strict (Text)
import qualified Data.Text as Strict.Text (empty)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text.Lazy as Lazy.Text (empty)
import Data.Vector (Vector, empty)



instance Default ZonedTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  {-# INLINE def #-}


instance Default UUID.UUID where
  def = UUID.nil
  {-# INLINE def #-}


instance Default AET.Value where
  def = AET.emptyObject
  {-# INLINE def #-}


-- Another option would be to use:
--
-- instance (IsString s, FoldCase s) => Default (CI s) where
--     def = CI.mk (fromString "")
--
-- Problem with the above definition is if someone defines his/her
-- newtype with IsString instance that should have different default
-- value then "". In example hostname for connecting to, etc.

instance (Default s, FoldCase s) => Default (CI s) where
    def = CI.mk def
    {-# INLINE def #-}

-- $providedInstances
--
-- @
-- instance ('Default' s, 'FoldCase' s) => 'Default' ('CI' s) where
--     'def' = 'CI.mk' 'def'
-- @


----- TEXT

-- | @'def' = 'Strict.Text.empty'@
instance Default Strict.Text where
    def = Strict.Text.empty
    {-# INLINE def #-}

-- | @'def' = 'Lazy.Text.empty'@
instance Default Lazy.Text where
    def = Lazy.Text.empty
    {-# INLINE def #-}


----- Vector
instance Default (Vector a) where
    def = empty
    {-# INLINE def #-}
    -- Function empty is also inlined and we need to preserve low-level
    -- optimizations that are done in vector package.
