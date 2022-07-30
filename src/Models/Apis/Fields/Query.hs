module Models.Apis.Fields.Query (fieldById, selectFields, insertFieldQueryAndParams) where

import Data.Vector (Vector)
import Database.PostgreSQL.Entity (selectById)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types (Field, FieldId)
import Optics.Core ((^.))
import Relude
import Utils (DBField (MkDBField))

insertFieldQueryAndParams :: Field -> (Query, [DBField])
insertFieldQueryAndParams field = (q, params)
  where
    q = [sql| insert into api.fields (project_id, endpoint_hash, key, field_type, format, description, key_path, field_category, hash) VALUES (?,?,?,?,?,?,?,?,?); |]
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

-- | upsertFields
-- FIXME: This is no longer needed since we insert fields and formats directly
-- upsertFields :: Endpoints.EndpointId -> [(Field, [AE.Value])] -> DBT IO [(Formats.FormatId, FieldId)]
-- upsertFields endpointID fields = returning q options
--   where
--     q =
--       [sql|
--         select (apis.create_field_and_formats(
--           pid::uuid, eid::uuid, key, field_type::apis.field_type, field_type_override, fmt, fmt_override,
--           dsc, key_path, key_path_str, field_cat::apis.field_category, examples::text[], max_examples::int)).*
--         FROM (
--           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
--         ) as v(pid,eid,key,field_type,field_type_override,fmt,fmt_override,dsc,key_path,key_path_str,field_cat,examples,max_examples)
--       |]
--     options =
--       fields & map \(field, examples) ->
--         ( field ^. #projectId,
--           endpointID,
--           field ^. #key,
--           field ^. #fieldType,
--           field ^. #fieldTypeOverride,
--           field ^. #format,
--           field ^. #formatOverride,
--           field ^. #description,
--           field ^. #keyPath,
--           field ^. #keyPathStr,
--           field ^. #fieldCategory,
--           Vector.fromList examples,
--           10 :: Int64 -- max examples size
--         )
fieldById :: FieldId -> DBT IO (Maybe Field)
fieldById fid = selectById @Field (Only fid)

selectFields :: Endpoints.EndpointId -> DBT IO (Vector Field)
selectFields = query Select q
  where
    q =
      [sql| select id,created_at,updated_at,project_id,endpoint_id,key,field_type,
                    field_type_override,format,format_override,description,key_path,key_path_str,field_category
                    from apis.fields where endpoint_id=? order by field_category, key |]
