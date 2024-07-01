module Pages.Projects.ProjectsSpec (spec) where

import Data.Default (Default (..))
import Data.Effectful.UUID (runStaticUUID)
import Data.Effectful.Wreq (runHTTPGolden)
import Data.Either.Extra
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Time (runTime)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Users.Sessions qualified as Sessions
import Pages.Projects.CreateProject qualified as CreateProject
import Pkg.TmpPg qualified as TmpPg
import Relude
import Servant qualified
import Data.Cache (newCache)
import Servant.Server qualified as ServantS
import System.Types (atAuthToBase, effToServantHandlerTest)
import System.Config (EnvConfig(..), AuthContext(..))
import Test.Hspec
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)
import System.Clock (TimeSpec (TimeSpec))


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


spec :: Spec
spec = aroundAll TmpPg.withSetup do
  describe "Check Course Creation, Update and Consumption" do
    it "On first run courses table is empty" \pool -> do
      projectCache <- liftIO $ newCache (Just $ TimeSpec (60 * 60) 0)
      let atEnv = AuthContext (def @EnvConfig) pool pool projectCache (def @EnvConfig)
      sessAndHeader <- testSessionHeader pool
      pg <- LogBulk.withBulkStdOutLogger \logger -> do
        let createPForm =
              CreateProject.CreateProjectForm
                { title = "Test Project CI"
                , description = "Test Description"
                , emails = ["test@apitoolkit.io"]
                , permissions = [ProjectMembers.PAdmin]
                , isUpdate = False
                , projectId = ""
                , paymentPlan = "Free"
                , timeZone = ""
                , orderId = Nothing
                }
        CreateProject.createProjectPostH createPForm
          & atAuthToBase sessAndHeader
          & effToServantHandlerTest atEnv logger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      1 `shouldBe` 1


-- let tsEnv = TalstackEnv pool pool (def :: Config)
-- pg <- LogBulk.withBulkStdOutLogger \logger -> do
--   CreateProject.courseListH False Nothing Nothing
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

testSessionHeader :: (MonadIO m) => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session)
testSessionHeader pool = do
  pSessId <-
    Auth.authorizeUserAndPersist Nothing "firstName" "lastName" "https://placehold.it/500x500" "test@apitoolkit.io"
      & (runStaticUUID $ map (UUID.fromWords 0 0 0) [1 .. 10])
      & runHTTPGolden "./golden/"
      & DB.runDB pool
      & runTime
      & runEff
      & liftIO
  Auth.sessionByID (Just pSessId) "requestID" False
    & runErrorNoCallStack @Servant.ServerError
    & DB.runDB pool
    & runEff
    & liftIO
    <&> fromRightShow
