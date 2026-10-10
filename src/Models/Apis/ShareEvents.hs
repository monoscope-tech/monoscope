module Models.Apis.ShareEvents (
  ShareKind (..),
  createShareLink,
)
where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (DB, WrappedEnumSC (..))
import Relude
import Servant (FromHttpApiData)


-- | What a share link points at; only 'ShareLog' renders without a trace breakdown.
data ShareKind = ShareRequest | ShareLog | ShareSpan
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON, FromHttpApiData, HI.DecodeValue, HI.EncodeValue, ToSchema) via WrappedEnumSC 'Nothing "Share" ShareKind


createShareLink
  :: DB es
  => UUID.UUID
  -> Projects.ProjectId
  -> UUID.UUID
  -> ShareKind
  -> UTCTime
  -> Eff es ()
createShareLink sid pid eventId eventType eventCreatedAt =
  Hasql.interpExecute_
    [HI.sql| INSERT INTO apis.share_events (id, project_id, event_id, event_type, event_created_at)
             VALUES (#{sid},#{pid},#{eventId},#{eventType},#{eventCreatedAt}) |]
