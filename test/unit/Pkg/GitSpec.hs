-- | Contract tests for "Pkg.Git": the URL each host is asked for, and what we make of what it
-- answers.
--
-- These are the tests that stand between the multi-host code and a live GitLab. Doctests cover
-- the pure pieces; what they cannot cover is the shape of a real request-response round trip —
-- that we ask @\/api\/v4\/projects\/acme%2Fapi\/repository\/files\/…\/raw@ rather than
-- something GitLab has never heard of, that we read @id@ and not @sha@ out of its tree, and
-- that a 404 or a truncated page becomes an error rather than an empty repository.
--
-- The interpreter below serves canned vendor responses and records every request, so each
-- example asserts on both directions. No network, no database.
module Pkg.GitSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.Wreq (HTTP (..))
import Data.List (isInfixOf)
import Data.Text qualified as T
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
-- The Response *constructor* is only exported from the .Internal module; the public one
-- exports the type and its field selectors alone.
import Network.HTTP.Client.Internal (HttpException (..), HttpExceptionContent (..), Response (..), ResponseClose (..), createCookieJar, defaultRequest)
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types.Version (http11)
import Pkg.Git
import Relude
import Test.Hspec


-- | One canned reply: matched when every fragment appears in the request URL.
--
-- A fragment ending in @$@ must match the /end/ of the URL. Plain substring matching is too
-- loose for the two cases that matter here: @page=1@ is a substring of @page=11@, and a
-- directory path is a prefix of every file under it — both of which silently served the wrong
-- fixture and produced a green-looking test asserting the wrong thing.
data Canned = Canned {match :: [Text], status :: Int, respond :: LBS.ByteString}


ok :: [Text] -> LBS.ByteString -> Canned
ok m = Canned m 200


-- | A recorded outbound request: method and URL. Bodies are captured separately where a test
-- needs them, because the effect's @Postable@ existential cannot be inspected generically.
type Recorded = (Text, Text)


-- | Serve @canned@ and record what was asked for.
--
-- A request matching nothing is a 404 rather than a test-suite crash: "the host does not have
-- that" is a case every operation must handle, and several examples below rely on it.
runCanned :: IOE :> es => IORef [Recorded] -> [Canned] -> Eff (HTTP ': es) a -> Eff es a
runCanned seen canned = interpret \_ -> \case
  GetWith _ u -> reply "GET" u
  Get u -> reply "GET" u
  PostWith _ u _ -> reply "POST" u
  Post u _ -> reply "POST" u
  PutWith _ u _ -> reply "PUT" u
  Put u _ -> reply "PUT" u
  PatchWith _ u _ -> reply "PATCH" u
  Patch u _ -> reply "PATCH" u
  DeleteWith _ u -> reply "DELETE" u
  Delete u -> reply "DELETE" u
  where
    reply method u = liftIO do
      modifyIORef' seen ((method, toText u) :)
      let matches frag
            | Just prefix <- T.stripSuffix "$" frag = prefix `T.isSuffixOf` toText u
            | otherwise = toString frag `isInfixOf` u
          hit = find (all matches . (.match)) canned
      pure $ mkResponse (maybe 404 (.status) hit) (maybe "{}" (.respond) hit)


mkResponse :: Int -> LBS.ByteString -> Response LBS.ByteString
mkResponse code body =
  Response
    { responseStatus = Status code ""
    , responseVersion = http11
    , responseHeaders = []
    , responseBody = body
    , responseCookieJar = createCookieJar []
    , responseClose' = ResponseClose (pure ())
    , responseOriginalRequest = defaultRequest
    , responseEarlyHints = []
    }


-- | Run one operation against canned replies, returning its result and the URLs it asked for.
withHost :: GitHost -> Maybe Text -> [Canned] -> (GitConn -> Eff '[HTTP, IOE] a) -> IO (a, [Recorded])
withHost host origin canned act = do
  seen <- newIORef []
  conn <- either (fail . toString) pure $ mkGitConn host origin "tok"
  r <- runEff $ runCanned seen canned (act conn)
  reqs <- reverse <$> readIORef seen
  pure (r, reqs)


urlsOf :: [Recorded] -> [Text]
urlsOf = map snd


methodsOf :: [Recorded] -> [Text]
methodsOf = map fst


acmeApi :: RepoRef
acmeApi = RepoRef "acme" "api" "main"


-- Vendor response fixtures.
--
-- Captured from the live public APIs on 2026-08-12 and trimmed to the fields the decoders
-- read — not hand-written. That distinction is the whole point: a fixture I invented only
-- proves the decoder agrees with my memory of the vendor, which is exactly the thing that
-- cannot be trusted here. Paths are rewritten to @dashboards/a.yaml@ so the examples read
-- against one repository shape; every other value is verbatim.
--
-- To refresh:
--
-- @
-- curl 'https://api.github.com/repos/octocat/Hello-World/git/trees/master?recursive=1'
-- curl 'https://gitlab.com/api/v4/projects/gitlab-org%2Fgitlab-foss/repository/tree?ref=master'
-- curl 'https://gitea.com/api/v1/repos/gitea/tea/git/trees/main'
-- curl 'https://api.bitbucket.org/2.0/repositories/atlassian/aui/src/master/'
-- @
ghTree, giteaTree, glTree, bbTree, ghFile, ghCommit, glCommit, bbCommit :: LBS.ByteString
ghTree = "{\"sha\":\"7fd1a60b01f91b314f59955a4e4d4e80d8edf11d\",\"truncated\":false,\"tree\":[{\"path\":\"dashboards/a.yaml\",\"type\":\"blob\",\"sha\":\"b1\",\"size\":13}]}"

-- Gitea answers with git's own tree object, same shape as GitHub — which is why one decoder
-- serves both, and why this fixture exists to prove it rather than assume it.
giteaTree = "{\"sha\":\"075d94cc4ca799ce782722c025b256d489a027b6\",\"tree\":[{\"path\":\"dashboards/a.yaml\",\"mode\":\"100755\",\"type\":\"blob\",\"size\":918,\"sha\":\"b1\"}]}"

-- GitLab: the git object hash is `id`, and there is no `sha` key at all.
glTree = "[{\"id\":\"b1\",\"name\":\"a.yaml\",\"type\":\"blob\",\"path\":\"dashboards/a.yaml\",\"mode\":\"100644\"}]"

-- Bitbucket: `path`, `type`, `size` and nothing else identifying the content. The `commit`
-- object it also carries is the commit that was *listed*, not the one that last touched the
-- file — verified against the live API, where it equals the head commit for every entry.
bbTree = "{\"values\":[{\"path\":\"dashboards/a.yaml\",\"type\":\"commit_file\",\"size\":49}]}"

ghFile = "{\"content\":\"aGVsbG8K\"}" -- base64 "hello\n"
ghCommit = "{\"sha\":\"c1\"}"
glCommit = "[{\"id\":\"c1\",\"short_id\":\"c1\",\"title\":\"Add latest changes\"}]"
bbCommit = "{\"values\":[{\"type\":\"commit\",\"hash\":\"c1\"}]}"


spec :: Spec
spec = do
  describe "per-host request shapes" do
    -- The single most valuable assertion in this file: each host is addressed in its own
    -- dialect. A wrong path here is a 404 against a live vendor and nothing else catches it.
    it "reads a file from the endpoint each host actually exposes" \_ -> do
      let cases :: [(GitHost, Maybe Text, Text)]
          cases =
            [ (GitHub, Nothing, "https://api.github.com/repos/acme/api/contents/dashboards/a.yaml?ref=main")
            , (Gitea, Just "https://git.acme.dev", "https://git.acme.dev/api/v1/repos/acme/api/contents/dashboards/a.yaml?ref=main")
            , (GitLab, Nothing, "https://gitlab.com/api/v4/projects/acme%2Fapi/repository/files/dashboards%2Fa.yaml/raw?ref=main")
            , (Bitbucket, Nothing, "https://api.bitbucket.org/2.0/repositories/acme/api/src/main/dashboards/a.yaml")
            ]
      forM_ cases \(host, origin, expected) -> do
        (_, urls) <- withHost host origin [ok ["contents"] ghFile, ok ["/raw"] "hello\n", ok ["/src/"] "hello\n"] \c ->
          fetchFile c acmeApi "dashboards/a.yaml"
        urlsOf urls `shouldBe` [expected]

    it "decodes each host's own file encoding" \_ -> do
      -- GitHub and Gitea wrap the blob in base64 JSON; GitLab and Bitbucket return raw bytes.
      (gh, _) <- withHost GitHub Nothing [ok ["contents"] ghFile] \c -> fetchFile c acmeApi "a.yaml"
      gh `shouldBe` Right "hello\n"
      (gl, _) <- withHost GitLab Nothing [ok ["/raw"] "hello\n"] \c -> fetchFile c acmeApi "a.yaml"
      gl `shouldBe` Right "hello\n"
      (bb, _) <- withHost Bitbucket Nothing [ok ["/src/"] "hello\n"] \c -> fetchFile c acmeApi "a.yaml"
      bb `shouldBe` Right "hello\n"

    it "reads the blob sha from the field each host puts it in" \_ -> do
      -- GitHub says `sha`; GitLab says `id` and means the same git object hash.
      (ghR, _) <- withHost GitHub Nothing [ok ["git/trees"] ghTree, ok ["/commits/"] ghCommit] \c -> fetchTree c acmeApi "dashboards/"
      fmap (map (.sha) . snd) ghR `shouldBe` Right [Just "b1"]
      (glR, _) <- withHost GitLab Nothing [ok ["repository/tree"] glTree, ok ["repository/commits"] glCommit] \c -> fetchTree c acmeApi "dashboards/"
      fmap (map (.sha) . snd) glR `shouldBe` Right [Just "b1"]
      -- Gitea returns git's own tree object; the shared decoder must handle it unchanged,
      -- including the `mode` field GitHub omits.
      (giteaR, _) <- withHost Gitea (Just "https://git.acme.dev") [ok ["git/trees"] giteaTree, ok ["git/commits"] ghCommit] \c -> fetchTree c acmeApi "dashboards/"
      fmap (map (.sha) . snd) giteaR `shouldBe` Right [Just "b1"]

    it "backfills Bitbucket's missing blob shas from content, bounded by the prefix" \_ -> do
      -- Bitbucket's listing has no per-file hash, so entries under the prefix are hashed from
      -- their content. `computeContentSha "hello\n"` is git's own blob hash.
      (r, urls) <- withHost Bitbucket Nothing [ok ["max_depth"] bbTree, ok ["/src/main/dashboards/a.yaml$"] "hello\n", ok ["/commits/"] bbCommit] \c ->
        fetchTree c acmeApi "dashboards/"
      fmap (map (.sha) . snd) r `shouldBe` Right [Just (computeContentSha "hello\n")]
      -- One extra read per blob, and only for blobs under the prefix.
      length (filter ("a.yaml" `T.isInfixOf`) (urlsOf urls)) `shouldBe` 1

    it "does not backfill Bitbucket shas for a whole-acmeApi listing" \_ -> do
      -- An unscoped listing would be one request per file in the repository, so the shas stay
      -- Nothing — which `isDashboardFile` refuses rather than mistaking for "unchanged".
      (r, urls) <- withHost Bitbucket Nothing [ok ["max_depth"] bbTree, ok ["/commits/"] bbCommit] \c -> fetchTree c acmeApi ""
      fmap (map (.sha) . snd) r `shouldBe` Right [Nothing]
      filter ("a.yaml" `T.isInfixOf`) (urlsOf urls) `shouldBe` []

  describe "writes" do
    it "creates with POST and updates with PUT on GitLab" \_ -> do
      -- GitLab is the only host that distinguishes them by method; getting it backwards is a
      -- 400 on every write.
      (_, created) <- withHost GitLab Nothing [ok ["repository/files"] "{}", ok ["repository/commits"] glCommit] \c ->
        pushFile c acmeApi "dashboards/a.yaml" "body" Nothing "msg"
      (_, updated) <- withHost GitLab Nothing [ok ["repository/files"] "{}", ok ["repository/commits"] glCommit] \c ->
        pushFile c acmeApi "dashboards/a.yaml" "body" (Just "b1") "msg"
      methodsOf created `shouldContain` ["POST"]
      methodsOf updated `shouldContain` ["PUT"]

    it "computes the blob sha locally where the host returns none" \_ -> do
      -- GitLab and Bitbucket answer a write with no sha at all. `buildSyncPlan` compares shas
      -- later, so a wrong value here makes the next pull think every file changed.
      forM_ [GitLab, Bitbucket] \host -> do
        (r, _) <- withHost host Nothing [ok ["repository/files"] "{}", ok ["/src"] "", ok ["commits"] (if host == GitLab then glCommit else bbCommit)] \c ->
          pushFile c acmeApi "a.yaml" "hello\n" Nothing "msg"
        fmap fst r `shouldBe` Right (computeContentSha "hello\n")

    it "prefers the sha GitHub returns over a locally computed one" \_ -> do
      (r, _) <- withHost GitHub Nothing [ok ["contents"] "{\"content\":{\"sha\":\"server-sha\"}}", ok ["/commits/"] ghCommit] \c ->
        pushFile c acmeApi "a.yaml" "hello\n" Nothing "msg"
      fmap fst r `shouldBe` Right "server-sha"

  describe "pagination" do
    it "follows every page rather than stopping at the first" \_ -> do
      -- A short repository list looks exactly like an account with fewer repositories, which
      -- is why the picker would silently lose repos without this.
      let full = "{\"repositories\":[" <> LBS.intercalate "," (replicate 100 "{\"full_name\":\"acme/r\",\"default_branch\":\"main\"}") <> "]}"
          lastPage = "{\"repositories\":[{\"full_name\":\"acme/last\",\"default_branch\":\"main\"}]}"
      seen <- newIORef []
      conn <- either (fail . toString) pure $ mkGitConn GitHub Nothing "tok"
      r <- runEff $ runCanned seen [Canned ["page=1$"] 200 full, Canned ["page=2$"] 200 lastPage] (listRepos conn)
      fmap length r `shouldBe` Right 101

    it "fails loudly rather than returning a truncated list" \_ -> do
      -- Every page comes back full, so the walk never terminates on its own; the guard must
      -- turn that into an error, never a partial answer the caller reads as complete.
      let full = "{\"repositories\":[" <> LBS.intercalate "," (replicate 100 "{\"full_name\":\"acme/r\",\"default_branch\":\"main\"}") <> "]}"
      (r, _) <- withHost GitHub Nothing [ok ["installation/repositories"] full] listRepos
      r `shouldSatisfy` isLeft
      either (`shouldSatisfy` T.isInfixOf "refusing to continue with a partial list") (const $ expectationFailure "expected a page-limit error") r

    it "refuses a listing the host itself says it truncated" \_ -> do
      let truncatedTree = "{\"sha\":\"t1\",\"truncated\":true,\"tree\":[]}"
      (r, _) <- withHost GitHub Nothing [ok ["git/trees"] truncatedTree] \c -> fetchTree c acmeApi "dashboards/"
      r `shouldSatisfy` isLeft

  describe "failure responses" do
    it "reports a non-2xx as an error, not as an empty repository" \_ -> do
      -- The distinction that matters on the sync path: "I could not read the acmeApi" must not
      -- reach `buildSyncPlan`, which would read an empty listing as "delete every dashboard".
      forM_ [GitHub, GitLab, Bitbucket] \host -> do
        (r, _) <- withHost host Nothing [] \c -> fetchTree c acmeApi "dashboards/"
        r `shouldSatisfy` isLeft

    it "reports malformed JSON as an error" \_ -> do
      (r, _) <- withHost GitHub Nothing [ok ["contents"] "not json at all"] \c -> fetchFile c acmeApi "a.yaml"
      r `shouldSatisfy` isLeft

    it "falls back to main when the host will not name a default branch" \_ -> do
      -- A default branch is a nicety; failing the whole connect flow over it is not.
      (b, _) <- withHost GitHub Nothing [] \c -> defaultBranchOf c acmeApi
      b `shouldBe` "main"
      (bb, _) <- withHost Bitbucket Nothing [ok ["repositories/acme/api"] "{\"mainbranch\":{\"name\":\"trunk\"}}"] \c -> defaultBranchOf c acmeApi
      bb `shouldBe` "trunk"

  describe "credentials" do
    it "never renders the token" \_ -> do
      -- A GitConn in an exception or a log line must not leak the credential; Sensitive is
      -- what makes that structural rather than a review comment.
      show (Sensitive @Text "glpat-secret") `shouldBe` ("<redacted>" :: String)

  describe "throttling and non-2xx statuses" do
    -- The canned interpreter above returns a status but never *throws*, whereas wreq raises
    -- `StatusCodeException` on any non-2xx. So the existing "reports a non-2xx as an error"
    -- example actually passes on a JSON decode failure of the `{}` fallback body, and the
    -- status-reporting path — the one a live vendor exercises — had no coverage at all.
    -- These drive `formatHttpError` directly, which is what `get_` funnels every host error
    -- through, so they cover it without changing the interpreter's semantics.
    it "names the status a host rejected us with, so a 429 is not read as an empty repository" \_ -> do
      -- 429 is the one that matters operationally: rate limiting is indistinguishable from
      -- "the repository is empty" if the status is dropped, and on the sync path an empty
      -- listing means "delete every dashboard".
      let rejected code = formatHttpError (HttpExceptionRequest defaultRequest (StatusCodeException (mkResponse code "" $> ()) ""))
      rejected 429 `shouldBe` "HTTP 429"
      rejected 403 `shouldBe` "HTTP 403" -- GitHub rate-limits as 403 with a header, not 429
      rejected 404 `shouldBe` "HTTP 404"
      rejected 500 `shouldBe` "HTTP 500"

    it "describes a transport failure instead of pretending it was a status" \_ -> do
      -- A connection reset has no status code; reporting it as one would send a reader
      -- looking for a permissions problem that does not exist.
      formatHttpError (HttpExceptionRequest defaultRequest ResponseTimeout) `shouldSatisfy` T.isInfixOf "request failed"
      formatHttpError (InvalidUrlException "http://[" "bad host") `shouldSatisfy` T.isInfixOf "invalid URL"
