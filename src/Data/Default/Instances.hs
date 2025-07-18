{-# LANGUAGE CPP #-}

module Data.Default.Instances () where

import Data.Aeson.Types qualified as AET
import Data.CaseInsensitive (CI, FoldCase)
import Data.CaseInsensitive qualified as CI (mk)
import Data.Default
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Relude.Unsafe qualified as Unsafe


#if __GLASGOW_HASKELL__ < 910
import Data.Bool (Bool (False))

instance Default Bool where
  def = False 
  {-# INLINE def #-}
#endif


instance Default ZonedTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
  {-# INLINE def #-}


instance Default UTCTime where
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
instance Default T.Text where
  def = T.empty
  {-# INLINE def #-}


-- | @'def' = 'Lazy.Text.empty'@
instance Default TL.Text where
  def = TL.empty
  {-# INLINE def #-}


----- Vector
instance Default (V.Vector a) where
  def = V.empty
  {-# INLINE def #-}

-- Function empty is also inlined and we need to preserve low-level
-- optimizations that are done in vector package.
