{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.RequestDumps
  ( RequestDump (..),
    RequestDumpLogItem,
    insertRequestDump,
    requestDumpLogUrlPath,
    selectRequestsByStatusCodesStatByMin,
    labelRequestLatency,
    LabelValue,
    selectReqLatenciesRolledBySteps,
    selectRequestsByEndpointsStatByMin,
    selectReqLatenciesRolledByStepsForProject,
    selectRequestDumpByProject,
    selectRequestDumpByProjectAndId,
    requestDumpLogItemUrlPath,
    parseQueryStringToWhereClause,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON, object)
import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Text.Display
import Data.Time (CalendarDiffTime, ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Void
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert, Select), execute, query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT)
import Deriving.Aeson qualified as DAE
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Optics.TH
import Relude hiding (many, some)
import Relude.Unsafe qualified as Unsafe
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

data RequestDump = RequestDump
  { id :: UUID.UUID,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: UUID.UUID,
    host :: Text,
    urlPath :: Text,
    rawUrl :: Text,
    pathParams :: AE.Value,
    method :: Text,
    referer :: Text,
    protoMajor :: Int,
    protoMinor :: Int,
    duration :: CalendarDiffTime,
    statusCode :: Int,
    --
    queryParams :: AE.Value,
    requestBody :: AE.Value,
    responseBody :: AE.Value,
    requestHeaders :: AE.Value,
    responseHeaders :: AE.Value,
    --
    queryParamsKeypaths :: Vector Text,
    requestBodyKeypaths :: Vector Text,
    responseBodyKeypaths :: Vector Text,
    requestHeadersKeypaths :: Vector Text,
    responseHeadersKeypaths :: Vector Text,
    --
    shapeId :: Shapes.ShapeId,
    --
    formatIds :: Vector Formats.FormatId,
    fieldIds :: Vector Fields.FieldId
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving
    (Entity)
    via (GenericEntity '[Schema "apis", TableName "request_dumps", PrimaryKey "id", FieldModifiers '[CamelToSnake]] RequestDump)

makeFieldLabelsNoPrefix ''RequestDump

data RequestDumpLogItem = RequestDumpLogItem
  { id :: UUID.UUID,
    createdAt :: ZonedTime,
    host :: Text,
    urlPath :: Text,
    method :: Text,
    rawUrl :: Text,
    referer :: Text,
    --
    pathParams :: AE.Value,
    -- duration :: CalendarDiffTime,
    statusCode :: Int,
    --
    queryParams :: AE.Value,
    requestBody :: AE.Value,
    responseBody :: AE.Value,
    requestHeaders :: AE.Value,
    responseHeaders :: AE.Value
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToRow, FromRow)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestDumpLogItem

makeFieldLabelsNoPrefix ''RequestDumpLogItem

requestDumpLogItemUrlPath :: Projects.ProjectId -> UUID.UUID -> Text
requestDumpLogItemUrlPath pid rdId = "/p/" <> Projects.projectIdText pid <> "/log_explorer/" <> UUID.toText rdId

requestDumpLogUrlPath :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Text
requestDumpLogUrlPath pid q cols = "/p/" <> Projects.projectIdText pid <> "/log_explorer/?query=" <> fromMaybe "" q <> "&cols=" <> fromMaybe "" cols

selectRequestDumpByProject :: Projects.ProjectId -> Text -> DBT IO (Vector RequestDumpLogItem)
selectRequestDumpByProject pid extraQuery = do
  query Select (Query $ encodeUtf8 q) (Only pid)
  where
    extraQueryParsed = either error (\v -> if v == "" then "" else " AND " <> v) $ parseQueryStringToWhereClause extraQuery
    q =
      [text| SELECT  id,created_at,host,url_path,method,raw_url,referer,
                      path_params,status_code,query_params,
                      request_body,response_body,request_headers,response_headers
             FROM apis.request_dumps where project_id=? |]
        <> extraQueryParsed

selectRequestDumpByProjectAndId :: Projects.ProjectId -> UUID.UUID -> DBT IO (Maybe RequestDumpLogItem)
selectRequestDumpByProjectAndId pid rdId = queryOne Select q (pid, rdId)
  where
    q =
      [sql|SELECT   id,created_at,host,url_path,method,raw_url,referer,
                      path_params,status_code,query_params,
                      request_body,response_body,request_headers,response_headers
             FROM apis.request_dumps where project_id=? and id=?|]

insertRequestDump :: RequestDump -> DBT IO ()
insertRequestDump = void <$> execute Insert q
  where
    q = [sql| INSERT INTO apis.request_dumps VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?::uuid[],?::uuid[])|]

selectRequestsByStatusCodesStatByMin :: Projects.ProjectId -> Text -> Text -> DBT IO (Vector (ZonedTime, Text, Int))
selectRequestsByStatusCodesStatByMin pid urlPath method = query Select q (pid, urlPath, method)
  where
    q =
      [sql|
       SELECT time_bucket('1 minute', created_at) as timeB,
             status_code::text,
             count(id)
        FROM apis.request_dumps
        where created_at > NOW() - interval '14' day
        and project_id=? and url_path=? and method=?
        GROUP BY timeB, status_code;
    |]

selectRequestsByEndpointsStatByMin :: Projects.ProjectId -> DBT IO Text
selectRequestsByEndpointsStatByMin pid = do
  let q = [sql| SELECT ts_text FROM apis.project_requests_by_endpoint_per_min WHERE project_id=?; |]
  (Only val) <- fromMaybe (Only "[]") <$> queryOne Select q (Only pid)
  pure val

selectReqLatenciesRolledBySteps :: Int -> Int -> Projects.ProjectId -> Text -> Text -> DBT IO (Vector (Int, Int))
selectReqLatenciesRolledBySteps maxv steps pid urlPath method = query Select q (maxv, steps, steps, steps, pid, urlPath, method)
  where
    q =
      [sql| 
select duration_steps, count(id)
	FROM generate_series(0, ?, ?) AS duration_steps
	LEFT OUTER JOIN apis.request_dumps on (duration_steps = round((EXTRACT(epoch FROM duration)/1000000)/?)*? 
    AND created_at > NOW() - interval '14' day
    AND project_id=? and url_path=? and method=?)
	GROUP BY duration_steps 
	ORDER BY duration_steps;
      |]

-- TODO: expand this into a view
selectReqLatenciesRolledByStepsForProject :: Int -> Int -> Projects.ProjectId -> DBT IO (Vector (Int, Int))
selectReqLatenciesRolledByStepsForProject maxv steps pid = query Select q (maxv, steps, steps, steps, pid)
  where
    q =
      [sql| 
select duration_steps, count(id)
	FROM generate_series(0, ?, ?) AS duration_steps
	LEFT OUTER JOIN apis.request_dumps on (duration_steps = round((EXTRACT(epoch FROM duration)/1000000)/?)*? AND created_at > NOW() - interval '14' day
    AND project_id=?)
	GROUP BY duration_steps 
	ORDER BY duration_steps;
      |]

-- Useful for charting latency histogram on the dashbaord and endpoint details pages
newtype LabelValue = LabelValue (Int, Int, Maybe Text)
  deriving stock (Show)

instance ToJSON LabelValue where
  toJSON (LabelValue (x, y, Nothing)) = object ["label" .= (show x :: Text), "value" .= (y :: Int)]
  toJSON (LabelValue (x, y, Just z)) =
    object
      [ "label" .= (z :: Text),
        "lineposition" .= (show x :: Text),
        "labelposition" .= (show x :: Text),
        "vline" .= ("true" :: Text),
        "labelhalign" .= ("center" :: Text),
        "dashed" .= ("1" :: Text)
      ]

labelRequestLatency :: (Int, Int, Int, Int) -> (Int, Int) -> [LabelValue]
labelRequestLatency (pMax, p90, p75, p50) (x, y)
  | x == pMax = [LabelValue (x, y, Just "max"), LabelValue (x, y, Nothing)]
  | x == p90 = [LabelValue (x, y, Just "p90"), LabelValue (x, y, Nothing)]
  | x == p75 = [LabelValue (x, y, Just "p75"), LabelValue (x, y, Nothing)]
  | x == p50 = [LabelValue (x, y, Just "p50"), LabelValue (x, y, Nothing)]
  | otherwise = [LabelValue (x, y, Nothing)]

type Parser = Parsec Void Text

data Operator
  = Eq
  | NotEq
  deriving stock (Eq, Show)

-- FOR postgres sql gen
instance Display Operator where
  displayPrec prec Eq = "="
  displayPrec prec NotEq = "!="

pOperator :: Parser Operator
pOperator =
  choice @[]
    [ Eq <$ string "=",
      NotEq <$ string "!="
    ]

data Subject = Subject Text [Text]
  deriving stock (Eq, Show)

instance Display Subject where
  displayPrec prec (Subject x []) = displayPrec prec x
  displayPrec prec (Subject x [y]) = displayPrec prec $ x <> "->>" <> "'" <> y <> "'"
  displayPrec prec (Subject x z) = do
    let z' = reverse $ map (\a -> "'" <> a <> "'") z
    let y = Unsafe.head z'
    let ys = reverse $ Unsafe.tail z'
    let valIntermediate = x <> "->" <> T.intercalate "->" ys
    let val = valIntermediate <> "->>" <> y
    displayPrec prec val

data UnitExpr = UnitExpr Operator Subject Val
  deriving stock (Eq, Show)

instance Display UnitExpr where
  displayPrec prec (UnitExpr opp key val) = displayParen (prec > 0) $ displayPrec prec key <> displayPrec prec opp <> displayBuilder val

-- pUnitExpr would parse the smallest unit of a query expression, which has a subject, operation, and object
-- >>> parseTest pUnitExpr "abc==xyz"
-- >>> parseTest pUnitExpr "abc.mm==null"
-- UnitExpr Eq "abc.mm" Null
pUnitExpr :: Parser UnitExpr
pUnitExpr = do
  subjectT <- toText <$> some (alphaNumChar <|> oneOf @[] ['.', '-', '_'])
  hspace
  operator <- pOperator
  hspace
  val <- pVal
  case T.splitOn "." subjectT of
    (x : xs) -> pure $ UnitExpr operator (Subject x xs) val
    [] -> error "unreachable step, empty subject in query unit expr parsing."

data Val = Num Text | Str Text | Boolean Bool | Null
  deriving stock (Eq, Show)

instance Display Val where
  displayPrec prec (Num a) = displayBuilder a
  displayPrec prec (Str a) = displayBuilder $ "'" <> a <> "'"
  displayPrec prec (Boolean True) = "t"
  displayPrec prec (Boolean False) = "f"
  displayPrec prec Null = "null"

pBool :: Parser Bool
pBool = (True <$ string "true") <|> True <$ string "false"

pString :: Parser String
pString = do
  -- TODO: support single quotes as well. Maybe?
  _ <- char '"'
  val <- some (anySingleBut '"')
  _ <- char '"'
  pure val

pVal :: Parser Val
pVal =
  choice @[]
    [ Null <$ string "null",
      Boolean <$> pBool,
      Num . toText <$> some (digitChar <|> char '.'),
      Str . toText <$> pString
    ]

-- TODO: actually parse this section to remove possibility of malicious input
parseQueryStringToWhereClause :: Text -> Either Text Text
parseQueryStringToWhereClause q = if q == "" then Right "" else bimap (toText . errorBundlePretty) display (parse pUnitExpr "" q)
