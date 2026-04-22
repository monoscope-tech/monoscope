module Models.Apis.ShareEvents (
  createShareLink,
  getShareLink,
)
where

import Data.Effectful.Hasql qualified as Hasql
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Effectful (Eff, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB)
import Relude


createShareLink
  :: DB es
  => UUID.UUID
  -> Projects.ProjectId
  -> UUID.UUID
  -> Text
  -> UTCTime
  -> Eff es ()
createShareLink sid pid eventId eventType eventCreatedAt =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.share_events (id, project_id, event_id, event_type, event_created_at)
             VALUES (#{sid},#{pid},#{eventId},#{eventType},#{eventCreatedAt}) |]


-- | Resolve a share id to (project_id, event_id, event_type, event_created_at) if still within the 48h TTL.
getShareLink :: (DB es, Time :> es) => UUID.UUID -> Eff es (Maybe (Projects.ProjectId, UUID.UUID, Text, UTCTime))
getShareLink sid = do
  now <- Time.currentTime
  Hasql.interpOne
    [HI.sql|
      SELECT project_id, event_id, event_type, event_created_at
        FROM apis.share_events
       WHERE id = #{sid}
         AND created_at > #{now}::timestamptz - interval '48 hours'
       LIMIT 1
    |]
