module Models.Projects.ProjectMemory (PageId, MemoryText, mkMemoryText, memoryText, Document (..), Author (..), Page (..), Edit (..), Request (..), Summary (..), summarize, pageLimit, latestPages, pageHistory, writePage) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Pkg.DeriveUtils (DB, UUIDId)
import Relude


-- $setup
-- >>> :module Prelude
-- >>> import Data.Coerce qualified as Coerce
-- >>> import Data.Aeson qualified as Aeson
-- >>> import Data.UUID qualified as UUID
-- >>> import Pkg.DeriveUtils (UUIDId (..))
-- >>> import Models.Projects.ProjectMemory qualified as Memory
-- >>> import Relude


type PageId = UUIDId "memory_page"


type role MemoryText nominal


newtype MemoryText (limit :: Nat) = MemoryText Text
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON)


-- | Parse once into a bounded value; callers cannot coerce between limits.
--
-- >>> Memory.memoryText <$> Memory.mkMemoryText @3 "abc"
-- Right "abc"
-- >>> map (isLeft . Memory.mkMemoryText @3) ["", " ", "abcd", "a\NULb"]
-- [True,True,True,True]
-- >>> Memory.memoryText <$> Aeson.eitherDecode @(Memory.MemoryText 3) "\"abc\""
-- Right "abc"
-- >>> map (isLeft . Aeson.eitherDecode @(Memory.MemoryText 3)) ["\"\"", "\"abcd\"", "\"a\\u0000b\""]
-- [True,True,True]
-- >>> (Coerce.coerce <$> Memory.mkMemoryText @16000 "abc") :: Either Text (Memory.MemoryText 120)
-- ...
-- ...Couldn't match type...
-- ...
mkMemoryText :: forall limit. KnownNat limit => Text -> Either Text (MemoryText limit)
mkMemoryText value
  | T.null (T.strip value) || T.any (== '\NUL') value || fromIntegral (T.length value) > natVal (Proxy @limit) = Left $ "Enter between 1 and " <> show (natVal $ Proxy @limit) <> " characters without null bytes"
  | otherwise = Right $ MemoryText value


memoryText :: MemoryText limit -> Text
memoryText (MemoryText value) = value


instance KnownNat limit => AE.FromJSON (MemoryText limit) where
  parseJSON = AE.withText "Wiki text" $ either (fail . toString) pure . mkMemoryText


data Document = Document {title :: MemoryText 120, body :: MemoryText 16000}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Document


-- | The tagged JSON shape is also checked by the append-only storage schema.
--
-- >>> Aeson.toJSON (Memory.Member $ UUIDId UUID.nil) == Aeson.object ["tag" Aeson..= ("Member" :: Text), "contents" Aeson..= ("00000000-0000-0000-0000-000000000000" :: Text)]
-- True
-- >>> Aeson.toJSON (Memory.Agent $ UUIDId UUID.nil) == Aeson.object ["tag" Aeson..= ("Agent" :: Text), "contents" Aeson..= ("00000000-0000-0000-0000-000000000000" :: Text)]
-- True
data Author = Member (UUIDId "user") | Agent (UUIDId "agent_attempt")
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Author


data Page = Page {id :: PageId, version :: Int64, document :: Document, author :: Author, updatedAt :: UTCTime}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


data Edit = Edit {pageId :: PageId, version :: Word32, document :: Document}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Request = List | Read PageId | Write Edit
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Summary = Summary {id :: PageId, version :: Int64, title :: MemoryText 120, author :: Author, updatedAt :: UTCTime}
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


summarize :: Page -> Summary
summarize page = Summary page.id page.version page.document.title page.author page.updatedAt


pageLimit :: Int
pageLimit = 100


latestPages :: DB es => UUIDId "project" -> Eff es [Page]
latestPages pid =
  Hasql.interp
    [HI.sql|
  SELECT page_id, version, document, author, created_at
  FROM (SELECT DISTINCT ON (page_id) * FROM projects.memory_revisions
        WHERE project_id = #{pid} ORDER BY page_id, version DESC) pages
  ORDER BY created_at DESC, page_id LIMIT #{pageLimit}
|]


pageHistory :: DB es => UUIDId "project" -> PageId -> Eff es [Page]
pageHistory pid page =
  Hasql.interp
    [HI.sql|
  SELECT page_id, version, document, author, created_at FROM projects.memory_revisions
  WHERE project_id = #{pid} AND page_id = #{page} ORDER BY version DESC LIMIT #{pageLimit}
|]


writePage :: DB es => UUIDId "project" -> PageId -> Word32 -> Document -> Author -> Eff es (Maybe Page)
writePage pid page expected document author = do
  let version = fromIntegral expected :: Int64
  Hasql.interp
    [HI.sql|
    WITH current_version AS (
      SELECT coalesce(max(version), 0) AS version FROM projects.memory_revisions
      WHERE project_id = #{pid} AND page_id = #{page}
    )
    INSERT INTO projects.memory_revisions (project_id, page_id, version, document, author)
    SELECT #{pid}, #{page}, #{version} + 1, #{Aeson document}, #{Aeson author}
    FROM current_version WHERE version = #{version} AND #{version} < 4294967295
    ON CONFLICT (project_id, page_id, version) DO NOTHING
    RETURNING page_id, version, document, author, created_at
  |]
