module Data.Effectful.Cache where

import Effectful
import Effectful.Dispatch.Dynamic
import Relude
import qualified Data.Cache as C

-- type role Cache phantom nominal

data Cache k v :: Effect where
  FetchWithCache :: (Eq k, Hashable k) => k -> (k -> m v) -> Cache k v m v


type instance DispatchOf (Cache k v) = 'Dynamic


fetchWithCache
  :: forall (k :: Type) (v :: Type) (es :: [Effect])
   . (Hashable k, Cache k v :> es)
  => k
  -> (k -> Eff es v)
  -> Eff es v
fetchWithCache key action = send $ FetchWithCache key action

-- runCacheIO :: IOE :> es => C.Cache k v -> Eff (Cache ': es ) a -> Eff es a 
-- runCacheIO cache= interpret $ \_ -> case 
--   FetchWithCache key action -> liftIO $ C.fetchWithCache cache key action
