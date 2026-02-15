module Pages.Settings (
  -- S3
  bringS3GetH,
  brings3PostH,
  getMinioConnectInfo,
  brings3RemoveH,
  -- Api
  apiGetH,
  apiPostH,
  apiActivateH,
  apiDeleteH,
  GenerateAPIKeyForm (..),
  ApiGet (..),
  ApiMut (..),
  -- Integrations
  TestForm (..),
  notificationsTestPostH,
  notificationsTestHistoryGetH,
  TestHistory (..),
  NotificationTestHistoryGet (..),
  -- LemonSqueezy
  webhookPostH,
  WebhookData (..),
  DataVals (..),
  MetaData (..),
  CustomData (..),
  Attributes (..),
  FirstSubItem (..),
  manageBillingGetH,
  BillingGet (..),
) where

import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.Default
import Data.Text qualified as T
import Data.Time (UTCTime (..), getZonedTime, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, Only (..), ToRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL qualified as DB
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxConfirm_, hxDelete_, hxGet_, hxIndicator_, hxPatch_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.Integrations (DiscordData (..), PagerdutyData (..), SlackData (..), getDiscordDataByProjectId, getPagerdutyByProjectId, getProjectSlackData)
import Models.Apis.Issues (IssueType (..), parseIssueType)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Data.CaseInsensitive qualified as CI
import Models.Projects.ProjectMembers (Team (..), getTeamsById, resolveTeamEmails)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.Minio qualified as Minio
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper)
import Pages.Components (BadgeColor (..), FieldCfg (..), FieldSize (..), ModalCfg (..), confirmModal_, connectionBadge_, formField_, iconBadgeLg_, iconBadgeXs_, iconBadge_, modalWith_, paymentPlanPicker)
import Pkg.Components.Table qualified as Table
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.EmailTemplates qualified as ET
import Pkg.Mail (sampleAlert, sampleReport, sendDiscordAlert, sendPagerdutyAlertToService, sendRenderedEmail, sendSlackAlert, sendWhatsAppAlert)
import Relude hiding (ask, asks)
import Servant (err400, err404, errBody)
import System.Config
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Text.Printf (printf)
import Utils (LoadingSize (..), faSprite_, htmxIndicator_)
import Web.FormUrlEncoded (FromForm)
import "base64" Data.ByteString.Base64.URL qualified as B64


----------------------------------------------------------------------
-- S3
----------------------------------------------------------------------

getMinioConnectInfo :: Text -> Text -> Text -> Text -> Text -> Minio.ConnectInfo
getMinioConnectInfo accessKey secretKey region bucket endpoint = Minio.setCreds (Minio.CredentialValue accessKey' secretKey' Nothing) withRegion
  where
    withRegion = Minio.setRegion (fromString $ toString region) info
    info = if T.null endpoint then Minio.awsCI else fromString $ toString endpoint
    accessKey' = fromString $ toString accessKey
    secretKey' = fromString $ toString secretKey


brings3PostH :: Projects.ProjectId -> Projects.ProjectS3Bucket -> ATAuthCtx (RespHeaders (Html ()))
brings3PostH pid s3Form = do
  let connectInfo = getMinioConnectInfo s3Form.accessKey s3Form.secretKey s3Form.region s3Form.bucket s3Form.endpointUrl
  res <- liftIO $ Minio.runMinio connectInfo do
    Minio.bucketExists s3Form.bucket
  case res of
    Left err -> do
      addErrorToast (show err) Nothing
      addRespHeaders $ connectionBadge_ "Not connected"
    Right bExists ->
      if bExists
        then do
          _ <- Projects.updateProjectS3Bucket pid $ Just s3Form
          addSuccessToast "Connected succesfully" Nothing
          addRespHeaders $ connectionBadge_ "Connected"
        else do
          addErrorToast "Bucket does not exist" Nothing
          addRespHeaders $ connectionBadge_ "Not connected"


brings3RemoveH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
brings3RemoveH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  _ <- Projects.updateProjectS3Bucket pid Nothing

  addSuccessToast "Removed S3 bucket" Nothing
  addRespHeaders $ connectionBadge_ "Not connected"


bringS3GetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
bringS3GetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext -- Get auth context
  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Your S3 bucket", isSettingsPage = True, config = appCtx.config}
  addRespHeaders $ bodyWrapper bwconf $ bringS3Page pid project.s3Bucket


bringS3Page :: Projects.ProjectId -> Maybe Projects.ProjectS3Bucket -> Html ()
bringS3Page pid s3BucketM = div_ [class_ "w-full h-full overflow-y-auto"] do
  section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
    -- Header
    div_ [class_ "mb-2"] do
      h2_ [class_ "text-textStrong text-xl font-semibold"] "Bring Your Own S3 Bucket"
      p_ [class_ "text-textWeak text-sm mt-1"] "Connect your own S3 or S3-compatible storage for OpenTelemetry and session replay data"

    form_ [class_ "space-y-6", hxPost_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd", hxIndicator_ "#indicator"] do
      -- Connection status card
      div_ [class_ "surface-raised rounded-2xl p-4"] do
        div_ [class_ "flex items-center justify-between"] do
          div_ [class_ "flex items-center gap-3"] do
            iconBadge_ BrandBadge "bucket"
            div_ do
              h3_ [class_ "text-sm font-medium text-textStrong"] "Connection Status"
              p_ [class_ "text-xs text-textWeak"] "Your bucket connection state"
          div_ [id_ "connectedInd"] $ connectionBadge_ $ bool "Not connected" "Connected" (isJust s3BucketM)

      -- Credentials card
      div_ [class_ "surface-raised rounded-2xl p-4 space-y-4"] do
        label_ [class_ "text-sm font-medium text-textStrong block"] "Bucket Credentials"
        div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] do
          formField_ FieldSm def{value = maybe "" (.accessKey) s3BucketM, placeholder = "Access Key ID"} "Access Key ID" "accessKey" True Nothing
          formField_ FieldSm def{inputType = "password", value = maybe "" (.secretKey) s3BucketM, placeholder = "Secret Access Key"} "Secret Access Key" "secretKey" True Nothing
          formField_ FieldSm def{value = maybe "" (.region) s3BucketM, placeholder = "Region"} "Region" "region" True Nothing
          formField_ FieldSm def{value = maybe "" (.bucket) s3BucketM, placeholder = "Bucket Name"} "Bucket Name" "bucket" True Nothing
        formField_ FieldSm def{value = maybe "" (.endpointUrl) s3BucketM, placeholder = "https://s3.example.com", extraAttrs = [pattern_ "https?://.*"]} "Custom Endpoint" "endpointUrl" False Nothing
        p_ [class_ "text-xs text-textWeak"] "Optional: For S3-compatible providers like MinIO, DigitalOcean Spaces, etc."

      -- Actions
      div_ [class_ "flex items-center justify-between"] do
        div_ [class_ "flex items-center gap-3"] do
          button_ [class_ "btn btn-sm btn-outline gap-1"] do
            "Validate & Save"
            htmxIndicator_ "indicator" LdXS
          span_ [class_ "text-xs text-textWeak"] "Auto-saves on success"
        when (isJust s3BucketM) $ label_ [class_ "btn btn-sm btn-ghost text-textError hover:bg-fillError-weak", Lucid.for_ "remove-modal"] do
          faSprite_ "trash" "regular" "w-3 h-3"
          span_ "Remove"

    confirmModal_ "remove-modal" "Remove bucket?" "This will disconnect your S3 bucket. Data already stored will remain in your bucket." [hxDelete_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd"] "Remove bucket"


----------------------------------------------------------------------
-- Api Keys
----------------------------------------------------------------------

data GenerateAPIKeyForm = GenerateAPIKeyForm
  { title :: Text
  , from :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


apiPostH :: Projects.ProjectId -> GenerateAPIKeyForm -> ATAuthCtx (RespHeaders ApiMut)
apiPostH pid apiKeyForm = do
  (sess, project) <- Sessions.sessionAndProject pid
  authCtx <- ask @AuthContext
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 authCtx.config.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
  let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 encryptedKey
  pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID (title apiKeyForm) encryptedKeyB64
  apiKeys <- do
    ProjectApiKeys.insertProjectApiKey pApiKey
    V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  addSuccessToast "Created API Key Successfully" Nothing
  addTriggerEvent "closeModal" ""
  case from apiKeyForm of
    Just v -> addRespHeaders $ ApiPostCopy (Just (pApiKey, encryptedKeyB64)) True
    Nothing -> addRespHeaders $ ApiPost pid apiKeys (Just (pApiKey, encryptedKeyB64))


apiDeleteH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiDeleteH pid keyid = do
  (sess, project) <- Sessions.sessionAndProject pid
  res <- ProjectApiKeys.revokeApiKey keyid
  apikeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then addSuccessToast "Revoked API Key Successfully" Nothing
    else addErrorToast "Something went wrong" Nothing
  addRespHeaders $ ApiPost pid apikeys Nothing


apiActivateH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiActivateH pid keyid = do
  (sess, project) <- Sessions.sessionAndProject pid
  res <- ProjectApiKeys.activateApiKey keyid
  apikeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then addSuccessToast "Activated API Key Successfully" Nothing
    else addErrorToast "Something went wrong" Nothing
  addRespHeaders $ ApiPost pid apikeys Nothing


data ApiMut
  = ApiPost Projects.ProjectId (V.Vector ProjectApiKeys.ProjectApiKey) (Maybe (ProjectApiKeys.ProjectApiKey, Text))
  | ApiPostCopy (Maybe (ProjectApiKeys.ProjectApiKey, Text)) Bool


instance ToHtml ApiMut where
  toHtml (ApiPost pid apiKeys m) = toHtml $ apiMainContent pid apiKeys m
  toHtml (ApiPostCopy m b) = toHtml $ copyNewApiKey m b
  toHtmlRaw = toHtml


-- | apiGetH renders the api keys list page which includes a modal for creating the apikeys.
apiGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders ApiGet)
apiGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  apiKeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API keys"
          , isSettingsPage = True
          , config = appCtx.config
          }
  addRespHeaders $ ApiGet $ PageCtx bwconf (pid, apiKeys)


newtype ApiGet = ApiGet (PageCtx (Projects.ProjectId, V.Vector ProjectApiKeys.ProjectApiKey))


instance ToHtml ApiGet where
  toHtml (ApiGet (PageCtx bwconf (pid, apiKeys))) = toHtml $ PageCtx bwconf $ apiKeysPage pid apiKeys
  toHtmlRaw = toHtml


apiKeysPage :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Html ()
apiKeysPage pid apiKeys = do
  section_ [class_ "w-full mx-auto px-16 py-16 overflow-hidden overflow-y-scroll"] do
    div_ [class_ "flex justify-between items-center mb-6"] do
      div_ [class_ "flex flex-col gap-2"] do
        h2_ [class_ "text-xl font-semibold text-textWeak leading-7"] "Manage API keys"
        p_ [class_ "text-sm text-textWeak leading-tight"] "Create and revoke your API keys"
      pass
    apiMainContent pid apiKeys Nothing

    modalWith_ "apikey-modal" def{boxClass = "p-8"} (Just $ span_ [class_ "btn btn-primary"] "Create an API key") do
      iconBadgeLg_ BrandBadge "key"
      span_ [class_ "text-textStrong text-2xl font-semibold mb-1"] "Generate an API key"
      form_ [hxPost_ $ "/p/" <> pid.toText <> "/apis", class_ "flex flex-col gap-4", hxTarget_ "#main-content"] do
        div_ [class_ "flex flex-col"] do
          p_ [class_ "text-textWeak"] "Please input a title for your API key."
          div_ $ input_ [class_ "input px-4 py-2 mt-6 border w-full", type_ "text", placeholder_ "Enter your API key title", name_ "title"]
        div_ [class_ "flex w-full"] $ button_ [type_ "submit", class_ "btn btn-primary w-full"] "Create key"


apiMainContent :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Html ()
apiMainContent pid apiKeys newKeyM = section_ [id_ "main-content"] do
  copyNewApiKey newKeyM False
  let activeKeys = V.filter (\x -> x.active) apiKeys
      revokedKeys = V.filter (\x -> not x.active) apiKeys
      activeTable = makeApiKeysTable pid activeKeys "active_content"
      revokedTable = makeApiKeysTable pid revokedKeys "revoked_content"
      tabs =
        Table.TabFilter
          { current = "Active keys"
          , currentURL = ""
          , clientSide = True
          , options =
              [ Table.TabFilterOpt{name = "Active keys", count = Just $ V.length activeKeys, targetId = Just "#active_content"}
              , Table.TabFilterOpt{name = "Archived keys", count = Just $ V.length revokedKeys, targetId = Just "#revoked_content"}
              ]
          }

  toHtml tabs
  div_ [class_ "a-tab-content", id_ "active_content"] $ toHtml activeTable
  div_ [class_ "hidden a-tab-content", id_ "revoked_content"] $ toHtml revokedTable


makeApiKeysTable :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Text -> Table.Table (Int, ProjectApiKeys.ProjectApiKey)
makeApiKeysTable pid apiKeys elemId =
  Table.Table
    { config = def{Table.elemID = elemId, Table.renderAsTable = True}
    , columns = apiKeyColumns pid
    , rows = V.indexed apiKeys
    , features = Table.Features{rowLink = Nothing, rowId = Nothing, rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"], selectRow = Nothing, bulkActions = [], search = Nothing, tabs = Nothing, sort = Nothing, sortableColumns = Nothing, tableHeaderActions = Nothing, pagination = Nothing, zeroState = Nothing, header = Nothing, treeConfig = Nothing}
    }


apiKeyColumns :: Projects.ProjectId -> [Table.Column (Int, ProjectApiKeys.ProjectApiKey)]
apiKeyColumns pid =
  [ Table.col "Title" \(_, apiKey) ->
      span_ [class_ "text-textStrong font-semibold text-sm truncate"] $ toHtml apiKey.title
  , Table.col "Key" \(i, apiKey) -> do
      let idx = "key-" <> show i
      div_ [class_ "whitespace-nowrap w-full flex items-center text-sm text-textWeak"] do
        span_ [class_ $ "mr-2 w-full " <> idx] $ toHtml $ T.take 8 apiKey.keyPrefix <> T.replicate 20 "*"
        div_ [class_ "hidden group-hover/row:flex justify-between items-center gap-3"] do
          button_
            [ class_ "text-textBrand"
            , term "data-key" apiKey.keyPrefix
            , term "data-state" "hide"
            , type_ "button"
            , term "data-tippy-content" "Show key"
            , term "data-prefix" (T.take 8 apiKey.keyPrefix <> T.replicate 20 "*")
            , term
                "_"
                [text|on click
                 if my @data-state is "hide"
                   put my @data-key into <.$idx/>
                   put "show" into my @data-state
                   put "Hide key" into my @data-tippy-content
                 else
                   put my @data-prefix into <.$idx/>
                   put "hide" into my @data-state
                   put "Show key" into my @data-tippy-content
                 end |]
            ]
            $ faSprite_ "eye" "regular" "h-4 w-4 text-textWeak"
          button_
            [ class_ "text-textBrand cursor-pointer"
            , type_ "button"
            , term "data-key" apiKey.keyPrefix
            , [__| on click if 'clipboard' in window.navigator then
                            call navigator.clipboard.writeText(my @data-key)
                            send successToast(value:['API Key has been copied to the Clipboard']) to <body/>
                          end |]
            , term "data-tippy-content" "Copy key"
            ]
            $ faSprite_ "clipboard-copy" "regular" "h-4 w-4 text-textWeak"
          if apiKey.active
            then button_
              [ class_ "text-textWeak flex gap-2 items-center cursor-pointer"
              , hxDelete_ $ "/p/" <> pid.toText <> "/apis/" <> apiKey.id.toText
              , hxConfirm_ $ "Are you sure you want to revoke " <> apiKey.title <> " API Key?"
              , hxTarget_ "#main-content"
              , id_ $ "key" <> show i
              ]
              do
                faSprite_ "circle-xmark" "regular" "h-4 w-4 text-textError"
                span_ [class_ "text-textWeak"] "Revoke"
            else button_
              [ class_ "text-textWeak flex gap-2 items-center cursor-pointer"
              , hxPatch_ $ "/p/" <> pid.toText <> "/apis/" <> apiKey.id.toText
              , hxConfirm_ $ "Are you sure you want to activate " <> apiKey.title <> " API Key?"
              , hxTarget_ "#main-content"
              , id_ $ "key" <> show i
              ]
              do
                faSprite_ "circle-check" "regular" "h-4 w-4 text-textWeak"
                span_ [class_ "text-textWeak"] "Activate"
  ]


copyNewApiKey :: Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Bool -> Html ()
copyNewApiKey newKeyM hasNext =
  case newKeyM of
    Nothing -> ""
    Just (keyObj, newKey) -> do
      div_ [id_ "apiFeedbackSection", class_ "pb-8"] do
        div_ [class_ "rounded-md bg-fillSuccess-weak p-4"] do
          div_ [class_ "flex"] do
            div_ [class_ "shrink-0"] do
              faSprite_ "circle-check" "regular" "h-5 w-5 text-textSuccess"
            div_ [class_ "ml-3"] do
              h3_ [class_ " font-medium text-textSuccess"] "API Key was generated successfully"
              div_ [class_ "mt-2  text-textSuccess py-2"] do
                strong_ [class_ "block pt-2", id_ "newKey"] $ toHtml newKey
              div_ [class_ "mt-4"] do
                div_ [class_ "-mx-2 -my-1.5 flex"] do
                  button_
                    [ type_ "button"
                    , class_ "bg-fillSuccess-strong px-2 py-1.5 text-white rounded-md  font-medium text-textSuccess hover:bg-fillSuccess-weak focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-offset-fillSuccess-weak focus:ring-strokeSuccess-strong"
                    , [__|
                      on click
                        if 'clipboard' in window.navigator then
                          call navigator.clipboard.writeText(#newKey's innerText)
                          send successToast(value:['API Key has been added to the Clipboard']) to <body/>
                        end
                        |]
                    ]
                    "Copy Key"
                  if not hasNext
                    then do
                      button_
                        [ type_ "button"
                        , class_ "ml-3 bg-green-50 px-2 py-1.5 rounded-md  font-medium text-textSuccess hover:bg-fillSuccess-weak focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-offset-fillSuccess-weak focus:ring-strokeSuccess-strong"
                        , [__|on click remove #apiFeedbackSection|]
                        ]
                        "Dismiss"
                    else do
                      button_
                        [ type_ "button"
                        , class_ "ml-6 font-medium px-2 py-1.5 rounded-md font-medium text-textBrand"
                        , [__|on click call window.location.reload()|]
                        ]
                        "Next"


----------------------------------------------------------------------
-- Integrations
----------------------------------------------------------------------

data TestForm = TestForm
  { issueType :: Text
  , channel :: Text
  , teamId :: Maybe UUID.UUID
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data TestHistory = TestHistory
  { id :: UUIDId "test_history"
  , projectId :: Projects.ProjectId
  , issueType :: Text
  , channel :: Text
  , target :: Text
  , status :: Text
  , error :: Maybe Text
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)


newtype NotificationTestHistoryGet = NotificationTestHistoryGet {tests :: [TestHistory]}
  deriving stock (Eq, Generic, Show)


instance ToHtml NotificationTestHistoryGet where
  toHtml (NotificationTestHistoryGet tests) = toHtmlRaw $ historyHtml_ tests
  toHtmlRaw (NotificationTestHistoryGet tests) = toHtmlRaw $ historyHtml_ tests


notificationsTestPostH :: Projects.ProjectId -> TestForm -> ATAuthCtx (RespHeaders (Html ()))
notificationsTestPostH pid TestForm{..} = do
  -- Rate limit: check if test was sent in last 60 seconds
  recentTests <- DB.query [sql|SELECT COUNT(*) FROM apis.notification_test_history WHERE project_id = ? AND created_at > now() - interval '60 seconds'|] (Only pid)
  when (maybe 0 fromOnly (listToMaybe recentTests) > (0 :: Int))
    $ throwError err400{errBody = "Rate limit: Please wait 60 seconds between test notifications"}

  project <- Projects.projectById pid >>= maybe (throwError err404) pure
  let alert = bool (sampleAlert (fromMaybe APIChange $ parseIssueType issueType) project.title) (sampleReport project.title) (issueType == "report")
      getTeam tid = listToMaybe <$> getTeamsById pid (V.singleton tid)

  Log.logTrace "Sending test notification" (channel, pid, issueType)
  appCtx <- ask @AuthContext

  let projectUrl = "/p/" <> pid.toText
      fullProjectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
      testTemplate = case issueType of
        "runtime_exception" -> ET.runtimeErrorsEmail project.title (fullProjectUrl <> "/issues/") []
        "report" -> ET.sampleWeeklyReport "" ""
        _ -> ET.anomalyEndpointEmail "Test User" project.title (fullProjectUrl <> "/issues") ["GET /api/v1/test"]
      sendTestEmail email = let (subj, html) = testTemplate; subj' = "[Test] " <> subj in sendRenderedEmail email subj' (ET.renderEmail subj' html)

  let resolveEmails t = map CI.original <$> resolveTeamEmails pid t

  case (channel, teamId) of
    ("all", Just tid) ->
      getTeam tid >>= traverse_ \t -> do
        resolveEmails t >>= mapM_ sendTestEmail
        forM_ t.slack_channels \c -> sendSlackAlert alert pid project.title (Just c)
        forM_ t.discord_channels \c -> sendDiscordAlert alert pid project.title (Just c)
        when (not $ V.null t.phone_numbers) $ sendWhatsAppAlert alert pid project.title t.phone_numbers
        forM_ t.pagerduty_services \k -> sendPagerdutyAlertToService k alert project.title projectUrl
    ("email", Just tid) -> getTeam tid >>= traverse_ \t -> resolveEmails t >>= mapM_ sendTestEmail
    ("email", Nothing) -> forM_ project.notifyEmails sendTestEmail
    ("slack", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.slack_channels \c -> sendSlackAlert alert pid project.title (Just c)
    ("slack", Nothing) -> getProjectSlackData pid >>= traverse_ \s -> sendSlackAlert alert pid project.title (Just s.channelId)
    ("discord", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.discord_channels \c -> sendDiscordAlert alert pid project.title (Just c)
    ("discord", Nothing) -> getDiscordDataByProjectId pid >>= traverse_ \d -> forM_ d.notifsChannelId \c -> sendDiscordAlert alert pid project.title (Just c)
    ("whatsapp", _) -> sendWhatsAppAlert alert pid project.title project.whatsappNumbers
    ("pagerduty", Just tid) -> getTeam tid >>= traverse_ \t -> forM_ t.pagerduty_services \k -> sendPagerdutyAlertToService k alert project.title projectUrl
    ("pagerduty", Nothing) -> getPagerdutyByProjectId pid >>= traverse_ \pd -> sendPagerdutyAlertToService pd.integrationKey alert project.title projectUrl
    _ -> throwError err400{errBody = "Unknown notification channel"}

  void
    $ DB.execute
      [sql|INSERT INTO apis.notification_test_history (project_id, issue_type, channel, target, status, error) VALUES (?, ?, ?, ?, ?, ?)|]
      (pid, issueType, channel, "" :: Text, "sent" :: Text, Nothing :: Maybe Text)

  Log.logTrace "Test notification sent" (channel, pid)
  let msg = if channel == "all" then "Test notification sent to all channels!" else "Test " <> channel <> " notification sent!"
  addSuccessToast msg Nothing >> addRespHeaders mempty


notificationsTestHistoryGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders NotificationTestHistoryGet)
notificationsTestHistoryGetH pid = do
  tests <- DB.query [sql|SELECT * FROM apis.notification_test_history WHERE project_id = ? ORDER BY created_at DESC LIMIT 20|] (Only pid)
  addRespHeaders $ NotificationTestHistoryGet tests


historyHtml_ :: [TestHistory] -> Html ()
historyHtml_ tests = if null tests then emptyMsg else renderTable tests
  where
    emptyMsg = div_ [class_ "text-center py-12"] do
      div_ [class_ "text-textWeak mb-2"] "No test notifications sent yet"
      p_ [class_ "text-sm text-textWeaker"] "Test your integrations to see results here"
    renderTable ts = div_ [class_ "bg-bgRaised rounded-lg border border-strokeWeak overflow-hidden"] $ table_ [class_ "table table-sm w-full"] (thead_ [class_ "text-xs text-left text-textStrong font-semibold uppercase bg-fillWeaker border-b border-strokeWeak"] (tr_ (th_ [class_ "p-3"] "Status" <> th_ [class_ "p-3"] "Channel" <> th_ [class_ "p-3"] "Alert Type" <> th_ [class_ "p-3 text-right"] "Time")) <> tbody_ [class_ "text-sm divide-y divide-strokeWeak"] (foldMap' renderRow ts))
    renderRow t = tr_ [class_ "hover-only:hover:bg-fillWeaker transition-colors"] (td_ [class_ "p-3"] (if t.status == "sent" then span_ [class_ "badge badge-success badge-sm gap-1"] (faSprite_ "check" "solid" "h-3 w-3" >> "Sent") else span_ [class_ "badge badge-error badge-sm gap-1"] (faSprite_ "xmark" "solid" "h-3 w-3" >> "Failed")) <> td_ [class_ "p-3 capitalize font-medium"] (toHtml $ if t.channel == "all" then "All channels" else t.channel) <> td_ [class_ "p-3 text-textWeak"] (toHtml $ T.replace "_" " " t.issueType) <> td_ [class_ "p-3 text-right tabular-nums text-textWeak"] (toHtml $ formatTime defaultTimeLocale "%b %d, %H:%M" t.createdAt))


----------------------------------------------------------------------
-- LemonSqueezy
----------------------------------------------------------------------

data FirstSubItem = FirstSubItem
  { id :: Int
  , subscriptionId :: Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake FirstSubItem


newtype CustomData = CustomData
  { projectId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake CustomData


data MetaData = MetaData
  {customData :: Maybe CustomData, eventName :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetaData


data Attributes = Attributes
  { firstSubscriptionItem :: FirstSubItem
  , productName :: Text
  , orderId :: Int
  , userEmail :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Attributes


data DataVals = DataVals
  { id :: Text
  , attributes :: Attributes
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake DataVals


data WebhookData = WebhookData
  { dataVal :: DataVals
  , meta :: MetaData
  }
  deriving stock (Generic, Show)


instance AE.FromJSON WebhookData where
  parseJSON = AE.withObject "WebhookData" $ \obj -> do
    dataVal <- obj AE..: "data"
    meta <- obj AE..: "meta"
    return (WebhookData{dataVal = dataVal, meta = meta})


webhookPostH :: Maybe Text -> WebhookData -> ATBaseCtx (Html ())
webhookPostH secretHeaderM dat = do
  envConfig <- asks env
  let orderId = dat.dataVal.attributes.orderId
  let subItem = dat.dataVal.attributes.firstSubscriptionItem
  case dat.meta.eventName of
    "subscription_created" -> do
      currentTime <- liftIO getZonedTime
      subId <- Projects.LemonSubId <$> liftIO UUIDV4.nextRandom
      let projectId = case dat.meta.customData of
            Nothing -> ""
            Just d -> fromMaybe "" d.projectId
      let sub =
            Projects.LemonSub
              { id = subId
              , createdAt = currentTime
              , updatedAt = currentTime
              , projectId = projectId
              , subscriptionId = subItem.subscriptionId
              , orderId
              , firstSubId = subItem.id
              , productName = dat.dataVal.attributes.productName
              , userEmail = dat.dataVal.attributes.userEmail
              }
      _ <- Projects.addSubscription sub
      pure "subscription created"
    "subscription_cancelled" -> do
      _ <- Projects.downgradeToFree orderId subItem.subscriptionId subItem.id
      pure "downgraded"
    "subscription_resumed" -> do
      _ <- Projects.upgradeToPaid orderId subItem.subscriptionId subItem.id
      pure "Upgraded"
    "subscription_expired" -> do
      _ <- Projects.downgradeToFree orderId subItem.subscriptionId subItem.id
      pure "Downgraded to free,sub expired"
    _ -> pure ""


newtype BillingGet = BillingGet (PageCtx (Projects.ProjectId, Int64, Text, Text, Text, Text, Text, Bool, Bool))


instance ToHtml BillingGet where
  toHtml (BillingGet (PageCtx bwconf (pid, totalReqs, amount, last_reported, lemonUrl, critical, paymentPlan, enableFreetier, basicAuthEnabled))) = toHtml $ PageCtx bwconf $ billingPage pid totalReqs amount last_reported lemonUrl critical paymentPlan enableFreetier basicAuthEnabled
  toHtmlRaw = toHtml


manageBillingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders BillingGet)
manageBillingGetH pid from = do
  (sess, project) <- Sessions.sessionAndProject pid
  let dat = fromMaybe project.createdAt project.billingDay
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  currentTime <- Time.currentTime
  let cycleStart = calculateCycleStartDate dat currentTime
  totalRequests <- Projects.getTotalUsage pid cycleStart
  let requestAfter = totalRequests - 20_000_000
  let estimatedAmount = show $ if requestAfter <= 0 then "34.00" else printf "%.2f" (fromIntegral requestAfter / 500_000 + 34.00)
  let last_reported = show project.usageLastReported
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Manage billing"
          , isSettingsPage = True
          , config = appCtx.config
          }
  let lemonUrl = envCfg.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
      critical = envCfg.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
  addRespHeaders $ BillingGet $ PageCtx bwconf (pid, totalRequests, estimatedAmount, last_reported, lemonUrl, critical, project.paymentPlan, envCfg.enableFreetier, envCfg.basicAuthEnabled)


billingPage :: Projects.ProjectId -> Int64 -> Text -> Text -> Text -> Text -> Text -> Bool -> Bool -> Html ()
billingPage pid reqs amount last_reported lemonUrl critical paymentPlan enableFreetier basicAuthEnabled = div_ [id_ "main-content", class_ "w-full h-full overflow-y-auto"] do
  let pidTxt = pid.toText
  section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
    div_ [class_ "mb-2"] do
      h2_ [class_ "text-textStrong text-xl font-semibold"] "Manage billing"
      p_ [class_ "text-textWeak text-sm mt-1"] "Track your usage and estimated costs"

    div_ [class_ "surface-raised rounded-2xl p-6 space-y-6"] do
      div_ [class_ "flex items-center gap-2 mb-4"] do
        iconBadgeXs_ BrandBadge "chart-line"
        label_ [class_ "text-sm font-medium text-textStrong"] "Usage Overview"

      div_ [class_ "grid grid-cols-2 gap-4"] do
        div_ [class_ "flex flex-col gap-2"] do
          div_ [class_ "text-4xl font-bold text-textStrong tabular-nums"] $ toHtml $ formatNumberWithCommas reqs
          div_ [class_ "text-textWeak text-sm"] "Total Requests Made"
          div_ [class_ "text-textWeak text-xs"] "*Calculation may not be up-to-date"
        div_ [class_ "flex flex-col gap-2"] do
          div_ [class_ "text-4xl font-bold text-textStrong tabular-nums"] $ toHtml $ "$" <> if paymentPlan == "Free" then "0" else T.replace "\"" "" amount
          div_ [class_ "text-textWeak text-sm"] "Estimated Cost"
          div_ [class_ "text-textWeak text-xs"] "*Based on current usage"

      div_ [class_ "border-t border-strokeWeak pt-4 space-y-3"] do
        div_ [class_ "flex items-center gap-2 text-textWeak text-sm"] do
          faSprite_ "regular-calendar-days-clock" "regular" "h-4 w-4"
          span_ $ toHtml $ "Latest data: " <> T.take 19 last_reported
        unless (paymentPlan == "Free") do
          a_ [class_ "flex items-center gap-2 text-textBrand hover:underline cursor-pointer text-sm font-medium", hxGet_ [text| /p/$pidTxt/manage_subscription |]] do
            faSprite_ "link-simple" "regular" "h-4 w-4"
            span_ "View on LemonSqueezy"

    div_ [class_ "surface-raised rounded-2xl p-6 space-y-4"] do
      div_ [class_ "flex items-center gap-2 mb-2"] do
        iconBadgeXs_ SuccessBadge "dollar"
        label_ [class_ "text-sm font-medium text-textStrong"] "Current Plan"

      div_ [class_ "flex items-center justify-between p-4 border border-strokeWeak rounded-xl bg-fillWeaker"] do
        div_ [class_ "flex flex-col gap-2"] do
          span_ [class_ "text-textStrong font-semibold text-lg"] $ toHtml paymentPlan
          span_ [class_ "rounded-lg text-textWeak bg-fillWeak border border-strokeWeak py-1 px-2.5 text-xs w-max"] "Active"
        div_ [class_ "flex items-baseline gap-1"] do
          span_ [class_ "text-textStrong text-xl"] "$"
          span_ [class_ "text-4xl text-textStrong font-bold tabular-nums"] $ if paymentPlan == "Free" then "0" else if paymentPlan == "Bring your own storage" then "199" else "29"
          span_ [class_ "text-textWeak text-sm ml-1"] "/month"

      div_ [class_ "border-t border-strokeWeak pt-4"] do
        div_ [class_ "text-textStrong text-sm font-semibold mb-2"] "Upgrade plan"
        p_ [class_ "text-textWeak text-sm mb-4"] "Monoscope pricing, click on compare feature below to select the option that best suit your project."
        pass

  modalWith_ "pricing-modal" def{boxClass = "w-[1250px] max-w-[1300px] py-16 px-32", wrapperClass = "p-8"} (Just $ span_ [class_ "btn btn-primary btn-sm"] "Change plan") do
    div_ [class_ "text-center text-sm text-textWeak w-full mx-auto max-w-96"] do
      span_ [class_ "text-textStrong text-2xl font-semibold"] "What's Included?"
      p_ [class_ "mt-2 mb-4"] "See and compare what you get in each plan."
      p_ [] "Please adjust the bar below to see difference in price as your events increase"
    paymentPlanPicker pid lemonUrl critical paymentPlan enableFreetier basicAuthEnabled


calculateCycleStartDate :: UTCTime -> UTCTime -> UTCTime
calculateCycleStartDate start current =
  let (_startYear, _startMonth, startDay) = toGregorian $ utctDay start
      (currentYear, currentMonth, currentDay) = toGregorian $ utctDay current
      timeOfDay = timeToTimeOfDay $ utctDayTime start
      cycleStartDay
        | currentDay > startDay = fromGregorian currentYear currentMonth startDay
        | currentMonth == 1 = fromGregorian (currentYear - 1) 12 startDay
        | otherwise = fromGregorian currentYear (currentMonth - 1) startDay
   in UTCTime cycleStartDay (timeOfDayToTime timeOfDay)


formatNumberWithCommas :: Int64 -> String
formatNumberWithCommas n = reverse $ insertCommas $ reverse (show n)
  where
    insertCommas [] = []
    insertCommas xs = case splitAt 3 xs of
      (chunk, []) -> chunk
      (chunk, rest) -> chunk ++ "," ++ insertCommas rest
