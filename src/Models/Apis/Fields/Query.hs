module Models.Apis.Fields.Query (fieldById, selectFields, insertFieldQueryAndParams, fieldsByEndpointHashes) where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Database.PostgreSQL.Transact qualified as PgT
import Models.Projects.Projects qualified as Projects

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

fieldById :: FieldId -> DBT IO (Maybe Field)
fieldById fid = selectById @Field (Only fid)

selectFields :: Text -> DBT IO (Vector Field)
selectFields endpointHash = query Select q (Only endpointHash)
 where
  q =
    [sql| select id,created_at,updated_at,project_id,endpoint_hash,key,field_type,
                field_type_override,format,format_override,description,key_path,field_category, hash
                from apis.fields where endpoint_hash=? order by field_category, key |]

fieldsByEndpointHashes :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector SwField)
fieldsByEndpointHashes pid hashes = query Select q (pid, hashes)
 where
  q =
    [sql|
      SELECT field_type f_field_type, field_type_override f_field_type_override, format f_format,
             description f_description, key_path f_key_path, field_category f_field_category,
             hash f_hash, format_override f_format_override, endpoint_hash f_endpoint_hash
      FROM apis.fields
      WHERE project_id = ? AND endpoint_hash = ANY(?)
    |]
