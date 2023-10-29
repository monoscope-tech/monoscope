{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Models.Apis.Fields.Query (
  fieldById,
  selectFields,
  insertFieldQueryAndParams,
  fieldsByEndpointHashes,
  insertFields,
  updateFieldByHash,
  deleteFieldByHash,
  selectFieldsByHashes,
  getFieldsByEndpointKeyPathAndCategory,
  autoCompleteFields,
) where

import Data.Time (ZonedTime)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Models.Apis.Fields.Types (Field, FieldCategoryEnum, FieldId, FieldTypes, SwField)
import Models.Apis.Fields.Types qualified as FT
import Models.Projects.Projects qualified as Projects
import Optics.Core ((^.))
import Relude
import Utils (DBField (MkDBField))


instance FromRow Text where
  fromRow = field


insertFieldQueryAndParams :: Field -> (Query, [DBField])
insertFieldQueryAndParams field = (q, params)
  where
    q =
      [sql| insert into apis.fields (project_id, endpoint_hash, key, field_type, format, description, key_path, field_category, hash) 
                    VALUES (?,?,?,?,?,?,?,?,?) ON CONFLICT DO NOTHING; |]
    params =
      [ MkDBField $ field ^. #projectId
      , MkDBField $ field ^. #endpointHash
      , MkDBField $ field ^. #key
      , MkDBField $ field ^. #fieldType
      , MkDBField $ field ^. #format
      , MkDBField $ field ^. #description
      , MkDBField $ field ^. #keyPath
      , MkDBField $ field ^. #fieldCategory
      , MkDBField $ field ^. #hash
      ]


insertFields :: [Field] -> DBT IO Int64
insertFields fields = do
  let q =
        [sql| 
        INSERT INTO apis.fields
        (project_id, endpoint_hash, key, field_type, field_type_override, format, format_override, description, key_path, field_category, hash)
        VALUES (?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT (hash)
        DO UPDATE SET field_type= EXCLUDED.field_type, description = EXCLUDED.description, format = EXCLUDED.format;
      |]
  let params = map getFieldParams fields
  executeMany q params


getFieldParams :: FT.Field -> (Projects.ProjectId, Text, Text, FieldTypes, Maybe Text, Text, Maybe Text, Text, Text, FieldCategoryEnum, Text)
getFieldParams field =
  ( field.projectId
  , field.endpointHash
  , field.key
  , field.fieldType
  , field.fieldTypeOverride
  , field.format
  , field.formatOverride
  , field.description
  , field.keyPath
  , field.fieldCategory
  , field.hash
  )


fieldById :: FieldId -> DBT IO (Maybe Field)
fieldById fid = selectById @Field (Only fid)


selectFields :: Projects.ProjectId -> Text -> DBT IO (Vector Field)
selectFields pid endpointHash = query Select q (pid, endpointHash)
  where
    q =
      [sql| select id,created_at,updated_at,project_id,endpoint_hash,key,field_type,
                field_type_override,format,format_override,description,key_path,field_category, hash
                from apis.fields where project_id=? AND endpoint_hash=? order by field_category, key |]
selectFieldsByHashes :: Projects.ProjectId -> Vector Text -> DBT IO (Vector Field)
selectFieldsByHashes pid fieldHashes = query Select q (pid, fieldHashes)
  where
    q =
      [sql| SELECT id,created_at,updated_at,project_id,endpoint_hash,key,field_type,
              field_type_override,format,format_override,description,key_path,field_category, hash
              FROM apis.fields WHERE project_id=? AND hash= ANY(?)  ORDER BY field_category, key 
          |]


getFieldsByEndpointKeyPathAndCategory :: Projects.ProjectId -> Text -> Text -> FieldCategoryEnum -> DBT IO (Vector Text)
getFieldsByEndpointKeyPathAndCategory pid endpointId keyPath fieldCategory = query Select q (pid, endpointId, keyPath, fieldCategory)
  where
    q = [sql|SELECT f.format from apis.endpoints enp JOIN apis.fields f on enp.hash = f.endpoint_hash where f.project_id=? AND enp.id=? AND f.key_path=? AND f.field_category=?  limit 4;|]


updateFieldByHash :: Text -> Text -> Text -> DBT IO Int64
updateFieldByHash endpointHash fieldHash description = do
  let q =
        [sql| UPDATE apis.fields SET  description=? WHERE endpoint_hash=? AND hash=? |]
  execute Update q (description, endpointHash, fieldHash)


deleteFieldByHash :: Text -> ZonedTime -> DBT IO Int64
deleteFieldByHash fieldHash dTime = do
  let q =
        [sql| UPDATE apis.fields SET  deleted_at=? WHERE hash=? |]
  execute Update q (dTime, fieldHash)


fieldsByEndpointHashes :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector SwField)
fieldsByEndpointHashes pid hashes = query Select q (pid, hashes)
  where
    q =
      [sql|
      SELECT  endpoint_hash f_endpoint_hash, key f_key, field_type f_field_type, format f_format,
             description f_description, key_path f_key_path, field_category f_field_category, hash f_hash
      FROM apis.fields
      WHERE project_id = ? AND endpoint_hash = ANY(?)
    |]


autoCompleteFields :: Projects.ProjectId -> FieldCategoryEnum -> Text -> DBT IO (Vector Text)
autoCompleteFields pid fieldCategory pathPrefix = query Select q (pid, fieldCategory, pathPrefix <> "%")
  where
    q = [sql|SELECT DISTINCT key_path from apis.fields WHERE project_id = ? AND field_category = ? AND key_path <> ''  AND key_path LIKE ?|]
