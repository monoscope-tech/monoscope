module Models.Projects.ProjectMemory (PagePath, mkPagePath, pagePathText, WriteError (..), pageAtPath, pageWindow, PageId, MemoryText, mkMemoryText, memoryText, Document (..), Author (..), Page (..), Edit (..), Request (..), Summary (..), summarize, pageLimit, latestPages, pageHistory, writePage) where

import Data.Aeson qualified as AE
import Data.Char (isAlphaNum, isAscii)
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff)
import Effectful.Exception (catch, throwIO)
import Hasql.Interpolate qualified as HI
import Hasql.Transaction.Sessions qualified as TxS
import Pkg.DeriveUtils (DB, UUIDId, refineText)
import Relude
import Web.HttpApiData (FromHttpApiData (..))


-- $setup
-- >>> :module Prelude
-- >>> import Data.Text qualified as T
-- >>> import Data.Coerce qualified as Coerce
-- >>> import Data.Aeson qualified as Aeson
-- >>> import Data.UUID qualified as UUID
-- >>> import Pkg.DeriveUtils (UUIDId (..))
-- >>> import Models.Projects.ProjectMemory qualified as Memory
-- >>> import Relude


type PageId = UUIDId "memory_page"


newtype PagePath = PagePath Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (AE.ToJSON, HI.EncodeValue)


-- | Paths are relative, bounded and cannot contain traversal or encoded separators.
-- >>> Memory.pagePathText <$> Memory.mkPagePath "runbooks/payments/retries.md"
-- Right "runbooks/payments/retries.md"
-- >>> map (isLeft . Memory.mkPagePath) ["", "/notes", "a//b", "a/../b", "a/./b", "a%2fb", "bad\\path", "a b", "a/", T.replicate 121 "x", T.intercalate "/" (replicate 17 "a")]
-- [True,True,True,True,True,True,True,True,True,True,True]
-- >>> fmap Memory.pagePathText (Aeson.eitherDecode @Memory.PagePath "\"runbooks/retries\"")
-- Right "runbooks/retries"
-- >>> map (isLeft . Aeson.eitherDecode @Memory.PagePath) ["\"../private\"", "\"a%2fb\"", "\"\""]
-- [True,True,True]
mkPagePath :: Text -> Either Text PagePath
mkPagePath value
  | T.null value || T.length value > 512 || length segments > 16 || any invalid segments = Left "Use a relative path with up to 16 segments and 512 characters: letters, numbers, dots, hyphens or underscores."
  | otherwise = Right $ PagePath value
  where
    segments = T.splitOn "/" value
    invalid part = T.null part || T.length part > 120 || part `elem` ([".", ".."] :: [Text]) || not (T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ("._-" :: String))) part)


pagePathText :: PagePath -> Text
pagePathText (PagePath path) = path


instance AE.FromJSON PagePath where
  parseJSON = AE.withText "Wiki path" $ either (fail . toString) pure . mkPagePath


instance FromHttpApiData PagePath where
  parseUrlPiece = mkPagePath


instance HI.DecodeValue PagePath where
  decodeValue = refineText "Wiki path" (rightToMaybe . mkPagePath)


data WriteError = StaleRevision | PathTaken
  deriving stock (Eq, Show)


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


data Page = Page {id :: PageId, version :: Int64, document :: Document, author :: Author, updatedAt :: UTCTime, path :: PagePath}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


data Edit = Edit {pageId :: PageId, version :: Word32, document :: Document}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Request = List | Read PageId | Write Edit
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Summary = Summary {id :: PageId, version :: Int64, title :: MemoryText 120, author :: Author, updatedAt :: UTCTime, path :: PagePath}
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


summarize :: Page -> Summary
summarize page = Summary page.id page.version page.document.title page.author page.updatedAt page.path


pageLimit :: Int
pageLimit = 100


latestPages :: DB es => UUIDId "project" -> Eff es [Page]
latestPages pid =
  Hasql.interp
    [HI.sql|
  SELECT pages.page_id, version, document, author, created_at, locations.path
  FROM (SELECT DISTINCT ON (page_id) * FROM projects.memory_revisions
        WHERE project_id = #{pid} ORDER BY page_id, version DESC) pages
  JOIN projects.memory_pages locations ON locations.project_id = #{pid} AND locations.page_id = pages.page_id
  ORDER BY created_at DESC, page_id LIMIT #{pageLimit}
|]


pageHistory :: DB es => UUIDId "project" -> PageId -> Eff es [Page]
pageHistory pid page =
  Hasql.interp
    [HI.sql|
  SELECT r.page_id, version, document, author, created_at, p.path FROM projects.memory_revisions r
  JOIN projects.memory_pages p USING (project_id, page_id)
  WHERE r.project_id = #{pid} AND r.page_id = #{page} ORDER BY version DESC LIMIT #{pageLimit}
|]


pageAtPath :: DB es => UUIDId "project" -> PagePath -> Eff es (Maybe PageId)
pageAtPath pid path = fmap HI.getOneColumn <$> Hasql.interpOne [HI.sql| SELECT page_id FROM projects.memory_pages WHERE project_id = #{pid} AND path = #{path} |]


pageWindow :: DB es => UUIDId "project" -> Maybe PagePath -> Eff es [Page]
pageWindow pid after =
  Hasql.interp
    [HI.sql|
    SELECT p.page_id, r.version, r.document, r.author, r.created_at, p.path
    FROM projects.memory_pages p CROSS JOIN LATERAL (
      SELECT version, document, author, created_at FROM projects.memory_revisions
      WHERE project_id = p.project_id AND page_id = p.page_id ORDER BY version DESC LIMIT 1
    ) r WHERE p.project_id = #{pid} AND (#{after}::text IS NULL OR p.path > #{after})
    ORDER BY p.path LIMIT #{pageLimit + 1}
  |]


writePage :: DB es => UUIDId "project" -> PageId -> Word32 -> Maybe PagePath -> Document -> Author -> Eff es (Either WriteError Page)
writePage pid page expected requested document author =
  Hasql.transaction
    TxS.ReadCommitted
    TxS.Write
    ( runExceptT do
        let initial = fromMaybe (PagePath page.toText) requested
        when (expected == 0)
          $ lift
          $ Hasql.executeTx
            [HI.sql|
      INSERT INTO projects.memory_pages (project_id, page_id, path) VALUES (#{pid}, #{page}, #{initial})
      ON CONFLICT (project_id, page_id) DO NOTHING
    |]
        present <- lift $ Hasql.queryTx @(Maybe (HI.OneColumn PagePath)) [HI.sql| SELECT path FROM projects.memory_pages WHERE project_id = #{pid} AND page_id = #{page} FOR UPDATE |]
        HI.OneColumn current <- hoistEither $ maybeToRight StaleRevision present
        HI.OneColumn version <- lift $ Hasql.oneTx @(HI.OneColumn Int64) [HI.sql| SELECT coalesce(max(version), 0) FROM projects.memory_revisions WHERE project_id = #{pid} AND page_id = #{page} |]
        when (version /= fromIntegral expected || expected == maxBound) $ hoistEither $ Left StaleRevision
        let location = fromMaybe current requested
        stored <-
          lift
            $ Hasql.queryTx @(Maybe Page)
              [HI.sql|
      INSERT INTO projects.memory_revisions (project_id, page_id, version, document, author)
      VALUES (#{pid}, #{page}, #{version} + 1, #{Aeson document}, #{Aeson author})
      ON CONFLICT (project_id, page_id, version) DO NOTHING
      RETURNING page_id, version, document, author, created_at, #{location}
    |]
        saved <- hoistEither $ maybeToRight StaleRevision stored
        lift $ Hasql.executeTx [HI.sql| UPDATE projects.memory_pages SET path = #{location} WHERE project_id = #{pid} AND page_id = #{page} |]
        pure saved
    )
    `catch` \err -> if Hasql.isUniqueViolation err then pure $ Left PathTaken else throwIO err
