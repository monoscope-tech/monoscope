module Data.Default.Instances where

import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import qualified Data.UUID as UUID
import qualified Relude.Unsafe as Unsafe

instance Default ZonedTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"

instance Default UUID.UUID where
  def = UUID.nil

instance Default AET.Value where
  def = AET.emptyObject
