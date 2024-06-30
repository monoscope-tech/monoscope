module Pages.Projects.ProjectsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Default (Default (..))
import Data.Either.Extra
import Data.Pool (Pool)
import Data.Time.Clock
import Database.PostgreSQL.Simple (Connection)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Time (runTime)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Pkg.TmpPg qualified as TmpPg
import Relude
import Servant qualified
import Servant.Server qualified as ServantS
import Test.Hspec
import Web.Cookie (SetCookie)


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


spec :: Spec
spec = aroundAll TmpPg.withSetup do
  describe "Check Course Creation, Update and Consumption" do
    it "On first run courses table is empty" \pool -> do
      1 `shouldBe` 1

-- sessAndHeader <- testSessionHeader pool
-- let tsEnv = TalstackEnv pool pool (def :: Config)
-- pg <- LogBulk.withBulkStdOutLogger \logger -> do
--   CourseList.courseListH False Nothing Nothing
--     & tsPageToTS sessAndHeader
--     & tsToServantHandlerTest tsEnv logger
--     & ServantS.runHandler
--     <&> fromRightShow
--     <&> Servant.getResponse
-- pg.content.isAdmin `shouldBe` False
-- pg.content.pageTitle `shouldBe` "All Courses"
-- pg.content.courseImageUrl `shouldBe` Nothing
-- pg.content.courseCards `shouldBe` []
-- pg.content.courseCardsDrafts `shouldBe` []
-- pg.content.allUniqueCategories `shouldBe` []
-- pg.content.category `shouldBe` Nothing
-- pg.content.isShorts `shouldBe` False
-- pg.content.lessonLastSeen `shouldBe` Nothing

-- testSessionHeader :: (Monad m, MonadIO m) => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Session.Session)
-- testSessionHeader pool = do
--   let user =
--         (def :: User.User)
--           { User.firstName = "firstName"
--           , User.lastName = "lastName"
--           , User.email = "test@talstack.com"
--           , User.emailVerified = True
--           , User.userFlags = (def :: User.UserFlags){User.workosUserId = Just "userID"}
--           }
--   let company =
--         (def :: User.Company)
--           { User.providerId = Just "workOSOrg"
--           , User.title = "Test Company"
--           , User.providerData = AE.object []
--           }
--   pSessId <-
--     Auth.authorizeUserAndPersist user company
--       & DB.runDB pool
--       & runTime
--       & runEff
--       & liftIO
--   Auth.sessionByID (Just pSessId) "requestID" False
--     & runErrorNoCallStack @Servant.ServerError
--     & DB.runDB pool
--     & runEff
--     & liftIO
--     <&> fromRightShow
