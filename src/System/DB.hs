module System.DB (DB) where

import Effectful (IOE, type (:>))
import Effectful.PostgreSQL (WithConnection)


-- | Database effect constraint alias combining WithConnection and IOE
type DB es = (WithConnection :> es, IOE :> es)
