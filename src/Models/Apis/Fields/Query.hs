module Models.Apis.Fields.Query (fieldById, selectFields, insertFieldQueryAndParams, fieldsByEndpointHashes, insertFields, updateFieldByHash, deleteFieldByHash) where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Data.Vector qualified as V
import Database.PostgreSQL.Transact (DBT, executeMany)
import Database.PostgreSQL.Transact qualified as PgT
import Models.Projects.Projects qualified as Projects

import Data.Time (ZonedTime)
import Models.Apis.Fields.Types (Field, FieldId, SwField)
import Optics.Core ((^.))

import Relude
import Utils (DBField (MkDBField))

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

insertFields :: Vector Field -> DBT IO Int64
insertFields fields = do
  let q =
        [sql| 
        INSERT INTO apis.fields
        (id, created_at, updated_at, project_id, endpoint_hash, key, field_type, field_type_override, format, format_override, description, key_path, field_category, hash)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT (hash)
        DO UPDATE SET field_type= EXCLUDED.field_type, description = EXCLUDED.description, format = EXCLUDED.format;
      |]
  executeMany q (V.toList fields)

fieldById :: FieldId -> DBT IO (Maybe Field)
fieldById fid = selectById @Field (Only fid)

selectFields :: Text -> DBT IO (Vector Field)
selectFields endpointHash = query Select q (Only endpointHash)
 where
  q =
    [sql| select id,created_at,updated_at,project_id,endpoint_hash,key,field_type,
                field_type_override,format,format_override,description,key_path,field_category, hash
                from apis.fields where endpoint_hash=? order by field_category, key |]

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
