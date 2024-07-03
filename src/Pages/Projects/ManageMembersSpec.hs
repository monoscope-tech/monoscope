module Pages.Projects.ManageMembersSpec (spec) where

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
import Pages.Projects.ManageMembers qualified as ManageMembers
import Pkg.TmpPg qualified as TmpPg
import Relude
import Relude.Unsafe qualified as Unsafe
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


testPid :: Projects.ProjectId
testPid = Unsafe.fromJust $ Projects.ProjectId <$> UUID.fromText "00000000-0000-0000-0000-000000000000"


spec :: Spec
spec = aroundAll withTestResources do
  describe "Members Creation, Update and Consumption" do
    it "Create member" \TestResources{..} -> do
      let member =
            ManageMembers.ManageMembersForm
              { emails = ["example@gmail.com"]
              , permissions = [ProjectMembers.PAdmin]
              }
      pg <-
        ManageMembers.manageMembersPostH testPid member
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      -- Check if the response contains the newly added member
      "example@gmail.com" `shouldSatisfy` (`elem` (pg.unwrapPost & V.toList & map (.email)))

    it "Update member permissions" \TestResources{..} -> do
      let member =
            ManageMembers.ManageMembersForm
              { emails = ["example@gmail.com"]
              , permissions = [ProjectMembers.PView]
              }
      pg <-
        ManageMembers.manageMembersPostH testPid member
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      -- Check if the member's permission is updated
      case pg of
        ManageMembers.ManageMembersPost projMembers -> do
          let memberM = projMembers & V.toList & find (\pm -> pm.email == "example@gmail.com")
          isJust memberM `shouldBe` True
          let mem = memberM & Unsafe.fromJust
          mem.permission `shouldBe` ProjectMembers.PView
          pass
        _ -> fail "Expected ManageMembersPost response"

    it "Get members" \TestResources{..} -> do
      pg <-
        ManageMembers.manageMembersGetH testPid
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      -- Check if the response contains the expected members
      case pg of
        ManageMembers.ManageMembersGet (PageCtx _ projMembers) -> do
          let emails = projMembers & V.toList & map (.email)
          "example@gmail.com" `shouldSatisfy` (`elem` emails)
          length projMembers `shouldBe` 1
        _ -> fail "Expected ManageMembersGet response"

    it "Delete member" \TestResources{..} -> do
      let member =
            ManageMembers.ManageMembersForm
              { emails = []
              , permissions = []
              }
      pg <-
        ManageMembers.manageMembersPostH testPid member
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      -- Check if the member is deleted
      case pg of
        ManageMembers.ManageMembersPost projMembers -> do
          let emails = projMembers & V.toList & map (.email)
          "example@gmail.com" `shouldNotSatisfy` (`elem` emails)
        _ -> fail "Expected ManageMembersPost response"


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
