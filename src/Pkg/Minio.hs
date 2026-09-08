module Pkg.Minio (runMinio) where

import Network.HTTP.Client (Manager)
import Network.Minio qualified as Minio
import Relude


-- | Reuse the application transport while keeping credentials, endpoint,
-- region and bucket-location cache local to this operation.
runMinio :: Manager -> Minio.ConnectInfo -> Minio.Minio a -> IO (Either Minio.MinioErr a)
runMinio manager config action = do
  connection <- Minio.mkMinioConn config manager
  Minio.runMinioWith connection action
