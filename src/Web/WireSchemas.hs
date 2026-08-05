-- | OpenApi schemas for the wire types that live in @monoscope-shared@.
--
-- They can't be declared alongside those types: the shared package is compiled
-- into the CLI, which must not link openapi3. Standalone @deriving via@ puts the
-- instances back on the server side, where the generated spec needs them, using
-- exactly the same wrappers the rest of the API types use.
--
-- Importing this module is what brings the instances into scope for
-- 'Web.Routes.apiV1OpenApi'.
module Web.WireSchemas () where

import Data.OpenApi (ToParamSchema, ToSchema)
import Models.Telemetry.Schema qualified as Schema
import Pages.Charts.Types qualified as Charts
import Pkg.DeriveUtils (JsonValueSchema (..), SnakeSchema (..), WrappedEnumSC (..))
import Relude
import Web.Wire (Paged)


deriving via SnakeSchema Charts.MetricsStats instance ToSchema Charts.MetricsStats


deriving via SnakeSchema Charts.MetricsData instance ToSchema Charts.MetricsData


deriving via WrappedEnumSC 'Nothing "DT" Charts.DataType instance ToSchema Charts.DataType


deriving via WrappedEnumSC 'Nothing "DT" Charts.DataType instance ToParamSchema Charts.DataType


deriving via SnakeSchema Schema.FieldInfo instance ToSchema Schema.FieldInfo


deriving via SnakeSchema Schema.Schema instance ToSchema Schema.Schema


deriving via JsonValueSchema (Paged a) instance Typeable a => ToSchema (Paged a)
