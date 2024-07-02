module Pages.Projects.ProjectsSpec (spec) where

import Data.Cache (Cache (..), newCache)
import Data.Default (Default (..))
import Data.Effectful.UUID (runStaticUUID)
import Data.Effectful.Wreq (runHTTPGolden)
import Data.Either.Extra
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Connection)
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Time (runTime)
import Log qualified
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper
import Pages.Projects.CreateProject qualified as CreateProject
import Pages.Projects.ListProjects qualified as ListProjects
import Pkg.TmpPg qualified as TmpPg
import Relude
import Servant qualified
import Servant.Server qualified as ServantS
import System.Clock (TimeSpec (TimeSpec))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (atAuthToBase, effToServantHandlerTest)
import Test.Hspec
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


-- New type to hold all our resources
data TestResources = TestResources
  { trPool :: Pool Connection
  , trProjectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , trSessAndHeader :: Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  , trATCtx :: AuthContext
  , trLogger :: Log.Logger
  }


-- Compose withSetup with additional IO actions
withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources f = TmpPg.withSetup $ \pool -> LogBulk.withBulkStdOutLogger \logger -> do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  sessAndHeader <- testSessionHeader pool
  let atAuthCtx =
        AuthContext (def @EnvConfig) pool pool projectCache $
          ( (def :: EnvConfig)
              { apiKeyEncryptionSecretKey = "apitoolkit123456123456apitoolkit"
              , convertkitApiKey = ""
              , convertkitApiSecret = ""
              }
          )
  f
    TestResources
      { trPool = pool
      , trProjectCache = projectCache
      , trSessAndHeader = sessAndHeader
      , trATCtx = atAuthCtx
      , trLogger = logger
      }


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Course Creation, Update and Consumption" do
    it "Create Project" \TestResources{..} -> do
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
      pg <-
        CreateProject.createProjectPostH createPForm
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      pg.form.title `shouldBe` "Test Project CI"
      pg.form.description `shouldBe` "Test Description"

    it "Non empty project list" \TestResources{..} -> do
      pg <-
        ListProjects.listProjectsGetH
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      length pg.unwrap.content `shouldBe` 2
      -- default demo project created in migrations 
      (pg.unwrap.content V.! 1).id.toText `shouldBe` "00000000-0000-0000-0000-000000000000"
      -- the project we actually created
      (pg.unwrap.content V.! 0).id.toText `shouldBe` "00000000-0000-0000-0000-000000000001"
      (pg.unwrap.content V.! 0).title `shouldBe` "Test Project CI"
      (pg.unwrap.content V.! 0).description `shouldBe` "Test Description"
      -- TODO: add more checks for the info we we display on list page

    it "Should update project with new details" \TestResources{..} -> do
      let createPForm =
            CreateProject.CreateProjectForm
              { title = "Test Project CI2"
              , description = "Test Description2"
              , emails = ["test@apitoolkit.io"]
              , permissions = [ProjectMembers.PAdmin]
              , isUpdate = True 
              , projectId = "00000000-0000-0000-0000-000000000001"
              , paymentPlan = "Free"
              , timeZone = ""
              , orderId = Nothing
              }
      pg <-
        CreateProject.createProjectPostH createPForm
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      pg.form.title `shouldBe` "Test Project CI2"
      pg.form.description `shouldBe` "Test Description2"

    -- FIXME: marked as pending with xit. Test is faily and should be investigated
    xit "Project in list should have new details" \TestResources{..} -> do
      pg <-
        ListProjects.listProjectsGetH
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      length pg.unwrap.content `shouldBe` 2
      (pg.unwrap.content V.! 0).id.toText `shouldBe` "00000000-0000-0000-0000-000000000001"
      (pg.unwrap.content V.! 0).title `shouldBe` "Test Project CI2"
      (pg.unwrap.content V.! 0).description `shouldBe` "Test Description2"
      -- TODO: add more checks for the info we we display on list page


-- | `testSessionHeader` would log a user in and automatically generate a session header 
-- which can be reused in subsequent tests
testSessionHeader :: MonadIO m => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session)
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
