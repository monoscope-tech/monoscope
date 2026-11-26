module Pages.OnboardingSpec (spec) where

import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client qualified as HC
import Pages.Onboarding qualified as Onboarding
import Pkg.OnboardingDiscovery qualified as Discovery
import Pkg.TestUtils
import System.Config (AuthContext (hasqlPool))
import System.IO.Error (userError)
import UnliftIO.Exception (throwIO)
import Web.Auth qualified as Auth
import Web.I18n qualified as I18n


spec :: Spec
spec = around withTestResources $ describe "Onboarding" do
  -- 2026-11 audit: quick-start and finish were gated at membership, so a read-only viewer
  -- renamed the project and marked onboarding steps complete.
  it "onboardingWrites_requireTheEditorRole" \tr -> do
    pid <- createTestProject tr "Acme Shop"
    let title = dbOne @_ @Text tr [sql|SELECT title FROM projects.projects WHERE id = ?|] (Only pid)
        completedSteps = dbOne @_ @Int tr [sql|SELECT coalesce(array_length(onboarding_steps_completed, 1), 0)::int FROM projects.projects WHERE id = ?|] (Only pid)
        quickStart name = Onboarding.onboardingQuickStartPostH pid (Onboarding.QuickStartForm name "usa" "url")
    setRole tr pid ProjectMembers.PView
    httpStatus tr (void $ quickStart "Renamed by a viewer") `shouldReturn` 403
    httpStatus tr (void $ Onboarding.onboardingFinishPostH pid) `shouldReturn` 403
    (,) <$> title <*> completedSteps `shouldReturn` ("Acme Shop", 0)

    setRole tr pid ProjectMembers.PEdit
    httpStatus tr (void $ quickStart "Renamed by an editor") `shouldReturn` 200
    title `shouldReturn` "Renamed by an editor"

  -- The same audit: discover-urls is an outbound fetch the caller chooses, so it must refuse
  -- the share-nothing demo guest and a viewer before any request leaves the host.
  it "discoverUrls_refusesTheDemoGuestAndViewerBeforeAnyFetch" \tr -> do
    Right guest <-
      runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider
        $ Auth.sessionByID Nothing "requestID" False "light" I18n.En Nothing Nothing (Just "/p/00000000-0000-0000-0000-000000000000/") Auth.ChallengeRedirect
    let form = Onboarding.DiscoverUrlForm "http://127.0.0.1.nip.io:9999/" Nothing
        asGuest = tr{trSessAndHeader = guest}
    httpStatus asGuest (void $ Onboarding.onboardingDiscoverUrlsPostH Projects.demoProjectId form) `shouldReturn` 403

    pid <- createTestProject tr "Acme Shop"
    setRole tr pid ProjectMembers.PView
    httpStatus tr (void $ Onboarding.onboardingDiscoverUrlsPostH pid form) `shouldReturn` 403

  -- The probe must run through the caller's manager: that is how the resolved-address guard
  -- ('System.Config.newProbeManagers') applies. A manager that refuses every connection
  -- therefore yields no status at all, even though the name is public.
  it "discovery_probesThroughTheSuppliedManager" \tr -> do
    let settings =
          HC.defaultManagerSettings
            { HC.managerRawConnection = pure \_ _ _ -> throwIO (userError "refused by the destination policy")
            , HC.managerTlsConnection = pure \_ _ _ -> throwIO (userError "refused by the destination policy")
            }
    mgr <- HC.newManager settings
    results <- runQueryEffect tr $ Discovery.probeCandidateUrls mgr "https://example.com"
    results `shouldSatisfy` (not . null)
    for_ results \r -> (r.status, isJust r.err) `shouldBe` (Nothing, True)
