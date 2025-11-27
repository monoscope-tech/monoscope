{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Models.Apis.Fields.Query (
  bulkInsertFields,
)
where

import Data.Vector qualified as V
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (executeMany)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Models.Apis.Fields.Types (Field (..), FieldCategoryEnum, FieldTypes)
import Models.Projects.Projects qualified as Projects
import Relude


instance FromRow Text where
  fromRow = field


bulkInsertFields :: DB :> es => V.Vector Field -> Eff es ()
bulkInsertFields fields = void $ dbtToEff $ executeMany q (V.toList rowsToInsert)
  where
    q =
      [sql| INSERT into apis.fields (project_id, endpoint_hash, key, field_type, format, description, key_path, field_category, hash)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING; |]
    rowsToInsert :: V.Vector (Projects.ProjectId, Text, Text, FieldTypes, Text, Text, Text, FieldCategoryEnum, Text)
    rowsToInsert =
      V.map
        ( \field ->
            ( field.projectId
            , field.endpointHash
            , field.key
            , field.fieldType
            , field.format
            , field.description
            , field.keyPath
            , field.fieldCategory
            , field.hash
            )
        )
        fields
