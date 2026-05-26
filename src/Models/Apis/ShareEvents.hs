module Models.Apis.ShareEvents (
  createShareLink,
)
where

import Data.Effectful.Hasql qualified as Hasql
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Effectful (Eff)
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
