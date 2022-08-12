module Models.Apis.Fields.Query (fieldById, selectFields, insertFieldQueryAndParams) where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Models.Apis.Fields.Types (Field, FieldId)
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
      [ MkDBField $ field ^. #projectId,
        MkDBField $ field ^. #endpointHash,
        MkDBField $ field ^. #key,
        MkDBField $ field ^. #fieldType,
        MkDBField $ field ^. #format,
        MkDBField $ field ^. #description,
        MkDBField $ field ^. #keyPath,
        MkDBField $ field ^. #fieldCategory,
        MkDBField $ field ^. #hash
      ]

fieldById :: FieldId -> DBT IO (Maybe Field)
fieldById fid = selectById @Field (Only fid)

selectFields :: Text -> DBT IO (Vector Field)
selectFields endpointHash = query Select q (Only endpointHash)
  where
    q =
      [sql| select id,created_at,updated_at,project_id,endpoint_hash,key,field_type,
                    field_type_override,format,format_override,description,key_path,key_path_str,field_category
                    from apis.fields where endpoint_hash=? order by field_category, key |]
