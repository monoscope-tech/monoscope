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
  BillingData (..),
  -- Stripe
  stripeWebhookPostH,
  createStripeCheckoutSession,
  createStripePortalSession,
  cancelStripeSubscription,
  cancelLemonSqueezySubscription,
  lemonSqueezyOpts,
  verifyStripeSignature,
  verifyLemonSqueezySignature,
) where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Aeson.Types (parseMaybe)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.CaseInsensitive qualified as CI
import Data.Default
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Notify qualified as Notify
import Data.Text qualified as T
import Data.Time (Day, UTCTime (..), addUTCTime, getZonedTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Log qualified as Log
import Hasql.Interpolate qualified as HI

import BackgroundJobs (errorTrendChartUrl)
import BackgroundJobs qualified as BJ
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Htmx (hxConfirm_, hxDelete_, hxGet_, hxIndicator_, hxPatch_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers (Team (..), getTeamsById, resolveTeamEmails)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.Minio qualified as Minio
import Network.Wreq qualified as Wreq
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper, settingsContentTarget)
import Pages.Components (BadgeColor (..), FieldCfg (..), FieldSize (..), ModalCfg (..), confirmModal_, connectionBadge_, formField_, iconBadgeLg_, modalWith_, paymentPlanPicker, sectionLabel_, settingsH2_, settingsSection_)
import Pkg.Components.Table qualified as Table
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.EmailTemplates qualified as ET
import Pkg.Mail (NotificationAlerts (..), sampleAlertByIssueTypeText, sampleReport, sendDiscordAlert, sendPagerdutyAlertToService, sendRenderedEmail, sendSlackAlert, sendWhatsAppAlert)
import Relude hiding (ask, asks)
import Servant (err400, errBody)
import System.Config
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Text.Printf (printf)
import Utils (LoadingSize (..), calculateCycleStartDate, faSprite_, formatUTC, htmxIndicator_)
import Web.FormUrlEncoded (FromForm)
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


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
          sess <- Projects.getSession
          Projects.logAuditS pid Projects.AES3Configured sess Nothing
          addSuccessToast "Connected succesfully" Nothing
          addRespHeaders $ connectionBadge_ "Connected"
        else do
          addErrorToast "Bucket does not exist" Nothing
          addRespHeaders $ connectionBadge_ "Not connected"


brings3RemoveH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
brings3RemoveH pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  _ <- Projects.updateProjectS3Bucket pid Nothing
  Projects.logAuditS pid Projects.AES3Removed sess Nothing
  addSuccessToast "Removed S3 bucket" Nothing
  addRespHeaders $ connectionBadge_ "Not connected"


bringS3GetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
bringS3GetH pid = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext -- Get auth context
  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Integrations", isSettingsPage = True, config = appCtx.config}
  addRespHeaders $ bodyWrapper bwconf $ bringS3Page pid project.s3Bucket


bringS3Page :: Projects.ProjectId -> Maybe Projects.ProjectS3Bucket -> Html ()
bringS3Page pid s3BucketM = settingsSection_ do
  div_ [class_ "flex items-center justify-between"] do
    settingsH2_ "S3 Bucket"
    div_ [id_ "connectedInd"] $ connectionBadge_ $ bool "Not connected" "Connected" (isJust s3BucketM)

  form_ [class_ "space-y-4", hxPost_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd", hxIndicator_ "#indicator"] do
    div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
      formField_ FieldSm def{value = maybe "" (.accessKey) s3BucketM, placeholder = "Access Key ID"} "Access Key ID" "accessKey" True Nothing
      formField_ FieldSm def{inputType = "password", value = maybe "" (.secretKey) s3BucketM, placeholder = "Secret Access Key"} "Secret Access Key" "secretKey" True Nothing
      formField_ FieldSm def{value = maybe "" (.region) s3BucketM, placeholder = "Region"} "Region" "region" True Nothing
      formField_ FieldSm def{value = maybe "" (.bucket) s3BucketM, placeholder = "Bucket Name"} "Bucket Name" "bucket" True Nothing
    formField_ FieldSm def{value = maybe "" (.endpointUrl) s3BucketM, placeholder = "https://s3.example.com", extraAttrs = [pattern_ "https?://.*"]} "Custom Endpoint" "endpointUrl" False Nothing
    p_ [class_ "text-xs text-textWeak"] "For S3-compatible providers like MinIO, DigitalOcean Spaces, etc."

    div_ [class_ "flex items-center justify-between pt-2"] do
      div_ [class_ "flex items-center gap-3"] do
        button_ [class_ "btn btn-sm btn-primary gap-1"] do
          "Validate & Save"
          htmxIndicator_ "indicator" LdXS
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
  (sess, project) <- Projects.sessionAndProject pid
  authCtx <- ask @AuthContext
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let encryptedKeyB64 = ProjectApiKeys.encodeApiKeyB64 authCtx.config.apiKeyEncryptionSecretKey projectKeyUUID
  pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID (title apiKeyForm) encryptedKeyB64
  apiKeys <- do
    ProjectApiKeys.insertProjectApiKey pApiKey
    V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  Projects.logAuditS pid Projects.AEApiKeyCreated sess
    $ Just
    $ AE.object ["key_title" AE..= title apiKeyForm]
  addSuccessToast "Created API Key Successfully" Nothing
  addTriggerEvent "closeModal" ""
  case from apiKeyForm of
    Just v -> addRespHeaders $ ApiPostCopy (Just (pApiKey, encryptedKeyB64)) True
    Nothing -> addRespHeaders $ ApiPost pid apiKeys (Just (pApiKey, encryptedKeyB64))


apiDeleteH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiDeleteH pid keyid = do
  (sess, project) <- Projects.sessionAndProject pid
  res <- ProjectApiKeys.revokeApiKey keyid
  apikeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then do
      Projects.logAuditS pid Projects.AEApiKeyRevoked sess Nothing
      addSuccessToast "Revoked API Key Successfully" Nothing
    else addErrorToast "Something went wrong" Nothing
  addRespHeaders $ ApiPost pid apikeys Nothing


apiActivateH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiActivateH pid keyid = do
  (sess, project) <- Projects.sessionAndProject pid
  res <- ProjectApiKeys.activateApiKey keyid
  apikeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then do
      Projects.logAuditS pid Projects.AEApiKeyActivated sess Nothing
      addSuccessToast "Activated API Key Successfully" Nothing
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
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  apiKeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API Keys"
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
  settingsSection_ do
    div_ [class_ "flex justify-between items-center"] do
      settingsH2_ "API Keys"
      modalWith_ "apikey-modal" def{boxClass = "p-8"} (Just $ span_ [class_ "btn btn-sm btn-primary gap-1.5"] $ do faSprite_ "plus" "regular" "w-3 h-3"; "New Key") do
        iconBadgeLg_ BrandBadge "key"
        span_ [class_ "text-textStrong text-2xl font-semibold mb-1"] "Generate an API key"
        form_ [hxPost_ $ "/p/" <> pid.toText <> "/apis", class_ "flex flex-col gap-4", hxTarget_ settingsContentTarget] do
          div_ [class_ "flex flex-col"] do
            p_ [class_ "text-textWeak"] "Please input a title for your API key."
            div_ $ input_ [class_ "input px-4 py-2 mt-6 border w-full", type_ "text", placeholder_ "Enter your API key title", name_ "title", required_ "true", maxlength_ "100"]
          div_ [class_ "flex w-full"] $ button_ [type_ "submit", class_ "btn btn-primary w-full"] "Create key"
    apiMainContent pid apiKeys Nothing


apiMainContent :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Html ()
apiMainContent pid apiKeys newKeyM = section_ [] do
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
    , features = Table.Features{rowLink = Nothing, rowId = Nothing, rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"], selectRow = Nothing, bulkActions = [], search = Nothing, tabs = Nothing, sort = Nothing, sortableColumns = Nothing, tableHeaderActions = Nothing, pagination = Nothing, zeroState = Just Table.ZeroState{icon = "key", title = "No API keys", description = "Create an API key to start integrating with your project.", actionText = "", destination = Right ""}, header = Nothing, treeConfig = Nothing}
    }


apiKeyColumns :: Projects.ProjectId -> [Table.Column (Int, ProjectApiKeys.ProjectApiKey)]
apiKeyColumns pid =
  [ Table.col "Title" \(_, apiKey) ->
      span_ [class_ "text-textStrong font-semibold text-sm truncate min-w-0 block max-w-48"] $ toHtml apiKey.title
  , Table.col "Key" \(i, apiKey) -> do
      let idx = "key-" <> show i
      div_ [class_ "whitespace-nowrap w-full flex items-center gap-2 text-sm text-textWeak"] do
        span_ [class_ $ "min-w-0 " <> idx] $ toHtml $ T.take 8 apiKey.keyPrefix <> T.replicate 20 "*"
        div_ [class_ "flex items-center gap-1.5 shrink-0 ml-auto"] do
          button_
            [ class_ "p-1 rounded hover:bg-fillWeaker cursor-pointer"
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
            $ faSprite_ "eye" "regular" "h-3.5 w-3.5 text-iconNeutral"
          button_
            [ class_ "p-1 rounded hover:bg-fillWeaker cursor-pointer"
            , type_ "button"
            , term "data-key" apiKey.keyPrefix
            , [__| on click if 'clipboard' in window.navigator then
                            call navigator.clipboard.writeText(my @data-key)
                            send successToast(value:['API Key has been copied to the Clipboard']) to <body/>
                          end |]
            , term "data-tippy-content" "Copy key"
            ]
            $ faSprite_ "clipboard-copy" "regular" "h-3.5 w-3.5 text-iconNeutral"
          let (hoverCls, hxMethod, tip, icon, iconCls) =
                if apiKey.active
                  then ("hover:bg-fillError-weak", hxDelete_, "Revoke key", "circle-xmark", "text-iconError")
                  else ("hover:bg-fillSuccess-weak", hxPatch_, "Activate key", "circle-check", "text-iconSuccess")
              confirmMsg = "Are you sure you want to " <> (if apiKey.active then "revoke " else "activate ") <> apiKey.title <> " API Key?"
          button_
            [ class_ $ "p-1 rounded cursor-pointer " <> hoverCls
            , hxMethod $ "/p/" <> pid.toText <> "/apis/" <> apiKey.id.toText
            , hxConfirm_ confirmMsg
            , hxTarget_ settingsContentTarget
            , id_ $ "key" <> show i
            , term "data-tippy-content" tip
            ]
            $ faSprite_ icon "regular"
            $ "h-3.5 w-3.5 "
            <> iconCls
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
              faSprite_ "circle-check" "regular" "h-5 w-5 text-iconSuccess"
            div_ [class_ "ml-3"] do
              h3_ [class_ " font-medium text-textSuccess"] "API Key was generated successfully"
              div_ [class_ "mt-2  text-textSuccess py-2"] do
                strong_ [class_ "block pt-2", id_ "newKey"] $ toHtml newKey
              div_ [class_ "mt-4"] do
                div_ [class_ "-mx-2 -my-1.5 flex"] do
                  button_
                    [ type_ "button"
                    , class_ "btn btn-sm btn-success"
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
                        , class_ "btn btn-sm btn-ghost text-textSuccess ml-2"
                        , [__|on click remove #apiFeedbackSection|]
                        ]
                        "Dismiss"
                    else do
                      button_
                        [ type_ "button"
                        , class_ "btn btn-sm btn-ghost text-textBrand ml-4"
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
  deriving anyclass (FromRow, HI.DecodeRow, ToRow)


newtype NotificationTestHistoryGet = NotificationTestHistoryGet {tests :: [TestHistory]}
  deriving stock (Eq, Generic, Show)


instance ToHtml NotificationTestHistoryGet where
  toHtml (NotificationTestHistoryGet tests) = toHtmlRaw $ historyHtml_ tests
  toHtmlRaw (NotificationTestHistoryGet tests) = toHtmlRaw $ historyHtml_ tests


notificationsTestPostH :: Projects.ProjectId -> TestForm -> ATAuthCtx (RespHeaders (Html ()))
notificationsTestPostH pid TestForm{..} = do
  -- Rate limit: check if test was sent in last 60 seconds
  now <- Time.currentTime
  recentCount <- fromMaybe (0 :: Int) <$> Hasql.interpOne [HI.sql|SELECT COUNT(*)::INT FROM apis.notification_test_history WHERE project_id = #{pid} AND created_at > #{now}::timestamptz - interval '60 seconds'|]
  when (recentCount > 0)
    $ throwError err400{errBody = "Rate limit: Please wait 60 seconds between test notifications"}

  (_, project) <- Projects.sessionAndProject pid
  let baseAlert = bool (sampleAlertByIssueTypeText issueType project.title) (sampleReport project.title) (issueType == "report")
      getTeam tid = listToMaybe <$> getTeamsById pid (V.singleton tid)
  -- Build a real signed widget URL so the test exercises the same image path
  -- as production alerts (webhook transport lifts it into attachments.image_url).
  -- The chart may be empty for the sample hash; what matters is that signing,
  -- URL length, and the webhook flatten/lift logic are all on the critical path.
  appCtxForChart <- ask @AuthContext
  nowUtc <- Time.currentTime
  let fromUtc = addUTCTime (-3600) nowUtc
  alert <- case baseAlert of
    RuntimeErrorAlert{errorData} -> do
      let errHash = (errorData :: ErrorPatterns.ATError).hash
      chartUrlM <- errorTrendChartUrl appCtxForChart pid errHash (formatUTC fromUtc) (formatUTC nowUtc)
      pure baseAlert{chartUrl = chartUrlM}
    _ -> pure baseAlert

  Log.logTrace "Sending test notification" (channel, pid, issueType)
  appCtx <- ask @AuthContext

  let projectUrl = "/p/" <> pid.toText
      fullProjectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
      testTemplate = case issueType of
        "runtime_exception" -> ET.runtimeErrorsEmail project.title (fullProjectUrl <> "/issues/") [] Nothing Nothing Nothing
        "escalating_errors" -> ET.escalatingErrorsEmail project.title (fullProjectUrl <> "/issues/") [] Nothing Nothing Nothing
        "regressed_errors" -> ET.regressedErrorsEmail project.title (fullProjectUrl <> "/issues/") [] Nothing Nothing Nothing
        "error_spike" -> ET.errorSpikesEmail project.title (fullProjectUrl <> "/issues/") [] Nothing Nothing Nothing
        "report" -> ET.sampleWeeklyReport "" ""
        _ -> ET.anomalyEndpointEmail "Test User" project.title (fullProjectUrl <> "/issues") [ET.EndpointAlertRow "GET /api/v1/test" (Just "api.example.com") (Just "api-service") (Just "production")]
      sendTestEmail email = let (subj, html) = testTemplate; subj' = "[Test] " <> subj in sendRenderedEmail email subj' (ET.renderEmail subj' html)

  -- The @everyone team is the single source of truth for project notification settings,
  -- so tests always resolve against a team — the provided one, or @everyone when omitted.
  let targetTeam = case teamId of
        Just tid -> getTeam tid
        Nothing -> ProjectMembers.getEveryoneTeam pid
      countingSend mChKey (count, reason) run =
        targetTeam >>= \case
          Nothing -> pure (count, Just "no_team")
          Just t | Just chKey <- mChKey, not (ProjectMembers.isChannelEnabled chKey t) -> pure (count, Just "channel_disabled")
          Just t -> (,reason) . (+ count) <$> run t
      sent = pure
      -- per-channel attempt counts
      slack t = do forM_ t.slack_channels (sendSlackAlert alert pid project.title . Just); sent (V.length t.slack_channels)
      discord t = do forM_ t.discord_channels (sendDiscordAlert alert pid project.title . Just); sent (V.length t.discord_channels)
      whatsapp t = if V.null t.phone_numbers then sent 0 else sent (V.length t.phone_numbers) <* sendWhatsAppAlert alert pid project.title t.phone_numbers
      pagerduty t = do forM_ t.pagerduty_services \k -> sendPagerdutyAlertToService k alert project.title projectUrl; sent (V.length t.pagerduty_services)
      email t = let emails = map CI.original (resolveTeamEmails t) in forM_ emails sendTestEmail *> sent (length emails)

  (attempts, skipReason) <- case channel of
    "all" -> countingSend Nothing (0, Nothing) \t -> do
      e <- email t
      s <- slack t
      d <- discord t
      w <- whatsapp t
      p <- pagerduty t
      pure (e + s + d + w + p)
    "email" -> countingSend Nothing (0, Nothing) email
    "slack" -> countingSend (Just "slack") (0, Nothing) slack
    "discord" -> countingSend (Just "discord") (0, Nothing) discord
    "whatsapp" -> countingSend (Just "phone") (0, Nothing) whatsapp
    "pagerduty" -> countingSend (Just "pagerduty") (0, Nothing) pagerduty
    _ -> throwError err400{errBody = "Unknown notification channel"}

  let (status, err) = case (attempts, skipReason) of
        (_, Just r) -> ("skipped" :: Text, Just r)
        (0, _) -> ("skipped", Just "no_targets")
        _ -> ("sent", Nothing)
  void
    $ Hasql.interpExecute
      [HI.sql|INSERT INTO apis.notification_test_history (project_id, issue_type, channel, target, status, error) VALUES (#{pid}, #{issueType}, #{channel}, #{("" :: Text)}, #{status}, #{err})|]

  Log.logTrace "Test notification complete" (channel, pid, status, attempts)
  let toast = case status of
        "sent" -> addSuccessToast (if channel == "all" then "Test notification sent to all channels!" else "Test " <> channel <> " notification sent!") Nothing
        _ -> addErrorToast ("Test skipped: " <> fromMaybe "unknown" err) Nothing
  toast >> addRespHeaders mempty


notificationsTestHistoryGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders NotificationTestHistoryGet)
notificationsTestHistoryGetH pid = do
  tests <- Hasql.interp [HI.sql|SELECT * FROM apis.notification_test_history WHERE project_id = #{pid} ORDER BY created_at DESC LIMIT 20|]
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
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier (DAE.Rename "dataVal" "data")] WebhookData


-- | LS signs a hex-encoded HMAC-SHA256 of the raw body in the X-Signature header.
--
-- >>> import "monoscope" Pages.Settings qualified as PS
-- >>> import Data.ByteString.Base16 qualified as B16
-- >>> import Data.ByteArray qualified as BA
-- >>> import "cryptonite" Crypto.MAC.HMAC qualified as HMAC
-- >>> import "cryptonite" Crypto.Hash (SHA256)
-- >>> let body = "{\"event\":\"x\"}" :: ByteString
-- >>> let secret = "s3cret" :: ByteString
-- >>> let sig = decodeUtf8 (B16.encode (BA.convert (HMAC.hmac secret body :: HMAC.HMAC SHA256) :: ByteString)) :: Text
-- >>> PS.verifyLemonSqueezySignature sig body secret
-- True
-- >>> PS.verifyLemonSqueezySignature sig body "wrong"
-- False
-- >>> PS.verifyLemonSqueezySignature sig "tampered" secret
-- False
-- >>> PS.verifyLemonSqueezySignature "not-hex!" body secret
-- False
-- >>> PS.verifyLemonSqueezySignature "" body secret
-- False
verifyLemonSqueezySignature :: Text -> ByteString -> ByteString -> Bool
verifyLemonSqueezySignature sigHeader payload secret =
  case B16.decode (encodeUtf8 sigHeader) of
    Right provided ->
      let expected = BA.convert (HMAC.hmac secret payload :: HMAC.HMAC SHA256) :: ByteString
       in BA.constEq expected provided
    Left _ -> False


webhookPostH :: Maybe Text -> ByteString -> ATBaseCtx (Html ())
webhookPostH sigHeaderM rawBody = do
  envConfig <- asks env
  let secret = encodeUtf8 envConfig.lemonSqueezyWebhookSecret
      sigValid = case sigHeaderM of
        Just h -> verifyLemonSqueezySignature h rawBody secret
        Nothing -> False
  unless sigValid $ Log.logAttention "ls_webhook_sig_mismatch" (T.take 8 <$> sigHeaderM, BS.length rawBody)
  when envConfig.lsWebhookSigEnforce $ case sigHeaderM of
    Nothing -> throwError err400{errBody = "missing signature"}
    Just _ | not sigValid -> throwError err400{errBody = "invalid signature"}
    _ -> pass
  dat <- case AE.eitherDecodeStrict rawBody :: Either String WebhookData of
    Right d -> pure d
    Left err -> do
      Log.logAttention "LS webhook invalid JSON" (err, decodeUtf8 @Text (BS.take 256 rawBody))
      throwError err400{errBody = "invalid json"}
  let orderId = dat.dataVal.attributes.orderId
      subItem = dat.dataVal.attributes.firstSubscriptionItem
      plan = dat.dataVal.attributes.productName
      billingUrl pid = envConfig.hostUrl <> "p/" <> pid.toText <> "/manage_billing"
      notifyMembers pid (subj, html) = Notify.runNotifyProduction do
        users <- Projects.usersByProjectId pid
        forM_ users \u -> sendRenderedEmail (CI.original u.email) subj (ET.renderEmail subj html)
      downgrade reason = do
        projectM <- Projects.projectByOrderId (show orderId)
        case projectM of
          Just project -> do
            rows <- Projects.downgradeToFree orderId subItem.subscriptionId subItem.id
            when (rows == 0) $ Log.logAttention "LS downgrade touched 0 rows" (show orderId :: Text, reason)
            when (rows > 1) $ Log.logAttention "LS downgrade touched multiple rows" (show orderId :: Text, rows)
            notifyMembers project.id $ ET.planDowngradedEmail project.title reason (billingUrl project.id)
          Nothing ->
            Log.logAttention "LS downgrade: no project for order_id" (show orderId :: Text, dat.meta.eventName, reason)
        pure "downgraded"
      upgrade = do
        rows <- Projects.upgradeToPaid orderId subItem.subscriptionId subItem.id plan
        when (rows == 0) $ Log.logAttention "LS upgrade touched 0 rows" (show orderId :: Text, subItem.subscriptionId, plan)
        when (rows > 1) $ Log.logAttention "LS upgrade touched multiple rows" (show orderId :: Text, rows)
        projectM <- Projects.projectBySubId (show subItem.subscriptionId)
        case projectM of
          Just project -> notifyMembers project.id $ ET.planUpgradedEmail project.title plan (billingUrl project.id)
          Nothing -> Log.logAttention "LS upgrade: no project for sub_id" (show orderId :: Text, subItem.subscriptionId, plan)
        pure "upgraded"
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
              , productName = plan
              , userEmail = dat.dataVal.attributes.userEmail
              }
      _ <- Projects.addSubscription sub
      -- Safety net: update project billing if frontend checkout callback failed
      whenJust (Projects.projectIdFromText projectId) \pid -> do
        void $ Projects.updateProjectBilling pid plan (show subItem.subscriptionId) (show subItem.id) (show orderId)
        whenJustM (Projects.projectById pid) \project ->
          notifyMembers pid $ ET.planUpgradedEmail project.title plan (billingUrl pid)
      pure "subscription created"
    "subscription_cancelled" -> downgrade "was cancelled"
    "subscription_expired" -> downgrade "has expired"
    -- Dunning retry: notify only, don't downgrade. Mirrors Stripe's invoice.payment_failed policy.
    -- subscription_expired/_cancelled will downgrade if retries ultimately fail.
    "subscription_payment_failed" -> do
      whenJustM (Projects.projectByOrderId (show orderId)) \project ->
        notifyMembers project.id $ ET.planDowngradedEmail project.title "payment failed" (billingUrl project.id)
      Log.logAttention "LS subscription_payment_failed (no downgrade)" (show orderId :: Text, subItem.subscriptionId)
      pure "payment failed notified"
    "subscription_paused" -> downgrade "was paused"
    "subscription_resumed" -> upgrade
    "subscription_unpaused" -> upgrade
    "subscription_payment_success" -> upgrade
    "subscription_payment_recovered" -> upgrade
    "subscription_payment_refunded" -> downgrade "payment refunded"
    "subscription_plan_changed" -> upgrade
    "subscription_updated" -> do
      Log.logInfo "LS subscription_updated (no-op)" (show orderId :: Text, subItem.subscriptionId, plan)
      pure "updated"
    other -> do
      Log.logInfo "LS webhook unhandled event" other
      pure ""


data BillingData = BillingData
  { pid :: Projects.ProjectId
  , totalReqs :: Int64
  , lastReported :: Text
  , lemonUrl :: Text
  , critical :: Text
  , paymentPlan :: Text
  , enableFreetier :: Bool
  , basicAuthEnabled :: Bool
  , provider :: Projects.BillingProvider
  , dailyUsage :: [(Day, Int64)]
  , cycleStart :: Day
  }


newtype BillingGet = BillingGet (PageCtx BillingData)


instance ToHtml BillingGet where
  toHtml (BillingGet (PageCtx bwconf d)) = toHtml $ PageCtx bwconf $ billingPage d
  toHtmlRaw = toHtml


manageBillingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders BillingGet)
manageBillingGetH pid from = do
  (sess, project) <- Projects.sessionAndProject pid
  let dat = fromMaybe project.createdAt project.billingDay
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  currentTime <- Time.currentTime
  let cycleStart = calculateCycleStartDate dat currentTime
      thirtyDaysAgo = addUTCTime (negate $ 30 * 86400) currentTime
      breakdownStart = min cycleStart thirtyDaysAgo
  totalRequests <- Projects.getTotalUsage pid cycleStart
  dailyUsage <- Projects.getDailyUsageBreakdown pid breakdownStart
  let last_reported = toText (formatTime defaultTimeLocale "%b %-d" project.usageLastReported)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Billing"
          , isSettingsPage = True
          , config = appCtx.config
          }
  let lemonUrl = envCfg.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
      critical = envCfg.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
  let provider = if project.paymentPlan == "Free" then Projects.NoBillingProvider else Projects.billingProvider project.subId
  addRespHeaders $ BillingGet $ PageCtx bwconf BillingData{pid, totalReqs = totalRequests, lastReported = last_reported, lemonUrl, critical, paymentPlan = project.paymentPlan, enableFreetier = envCfg.enableFreetier, basicAuthEnabled = envCfg.basicAuthEnabled, provider, dailyUsage, cycleStart = utctDay cycleStart}


billingPage :: BillingData -> Html ()
billingPage d = div_ [] do
  let pid = d.pid
      reqs = d.totalReqs
      last_reported = d.lastReported
      lemonUrl = d.lemonUrl
      critical = d.critical
      paymentPlan = d.paymentPlan
      enableFreetier = d.enableFreetier
      basicAuthEnabled = d.basicAuthEnabled
      provider = d.provider
      pidTxt = pid.toText
      isFree = paymentPlan == "Free"
      basePriceNum =
        if
          | isFree -> 0
          | paymentPlan == "Bring your own storage" -> 199
          | otherwise -> 29 :: Int64
      planPrice = show basePriceNum
      overageNum = max 0 (reqs - 20_000_000)
      overageCost = fromIntegral overageNum / 1_000_000 :: Double
      totalCost = fromIntegral basePriceNum + overageCost
      fmtUSD n = "$" <> toText (printf "%.2f" (n :: Double) :: String)
      estCost = if isFree then "$0" else fmtUSD totalCost
      cycleStartText = toText (formatTime defaultTimeLocale "%b %-d" d.cycleStart)
  settingsSection_ do
    settingsH2_ "Billing"

    -- Current plan row
    div_ [class_ "flex items-center justify-between"] do
      div_ [class_ "flex items-center gap-3"] do
        span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml paymentPlan
        span_ [class_ "rounded-md text-textWeak bg-fillWeak border border-strokeWeak py-0.5 px-2 text-xs"] "Active"
      div_ [class_ "flex items-baseline gap-0.5"] do
        span_ [class_ "text-2xl text-textStrong font-bold tabular-nums"] $ toHtml $ "$" <> planPrice
        span_ [class_ "text-textWeak text-sm"] "/mo"

    -- Usage section
    div_ [class_ "border-t border-strokeWeak pt-6 space-y-4"] do
      sectionLabel_ $ "Billing cycle (since " <> cycleStartText <> ")"
      -- Estimated cost as the primary stat; requests as supporting context
      div_ [] do
        div_ [class_ "text-2xl font-bold text-textStrong tabular-nums"] $ toHtml estCost
        div_ [class_ "text-sm text-textWeak mt-0.5"] "Estimated this cycle"
        div_ [class_ "text-xs text-textWeak mt-1 tabular-nums"]
          $ toHtml
          $ if isFree || overageNum <= 0
            then fmt (commaizeF reqs) <> " requests"
            else "$" <> planPrice <> " plan + " <> fmtUSD overageCost <> " usage (" <> fmt (commaizeF reqs) <> " requests)"
      unless (T.null last_reported)
        $ div_ [class_ "text-xs text-textWeak"]
        $ toHtml ("Last reported " <> last_reported)

    -- Actions (kept above the breakdown so they remain near the headline numbers)
    div_ [class_ "border-t border-strokeWeak pt-6 flex items-center gap-3"] do
      label_ [Lucid.for_ "pricing-modal", class_ "btn btn-sm btn-primary cursor-pointer"] "Change plan"
      unless isFree
        $ a_ [class_ "btn btn-sm btn-ghost text-textBrand", hxGet_ [text| /p/$pidTxt/manage_subscription |]] "Manage subscription"

    -- Daily breakdown
    dailyUsageBreakdown_ isFree d.cycleStart d.dailyUsage

  modalWith_ "pricing-modal" def{boxClass = "w-[1250px] max-w-[1300px] py-16 px-32", wrapperClass = "p-8"} Nothing do
    div_ [class_ "text-center text-sm text-textWeak w-full mx-auto max-w-96"] do
      span_ [class_ "text-textStrong text-2xl font-semibold"] "Compare Plans"
      p_ [class_ "mt-2 mb-4"] "Drag the slider to estimate costs at different usage levels."
    paymentPlanPicker pid lemonUrl critical paymentPlan enableFreetier basicAuthEnabled False provider


-- | Per-day usage table for the last 30 days. Cost shown is the marginal
-- contribution past the 20M-included tier ($1 per 1M), assuming chronological
-- accumulation across the cycle. Days entirely below the threshold show "—".
dailyUsageBreakdown_ :: Bool -> Day -> [(Day, Int64)] -> Html ()
dailyUsageBreakdown_ isFree cycleStartDay rows = div_ [class_ "border-t border-strokeWeak pt-6 space-y-3"] do
  div_ [class_ "flex items-baseline justify-between"] do
    sectionLabel_ "Daily breakdown"
    span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml @Text $ fmt (commaizeF (sum (map snd rows))) <> " requests"
  if null rows
    then div_ [class_ "text-sm text-textWeak py-4"] "No usage recorded yet this cycle."
    else do
      let activeDays = length rows
          included = 20_000_000 :: Int64
          maxDay = foldr (max . snd) 1 rows
          ascending = sortOn fst rows
          -- Running cumulative resets at cycleStartDay so pre-cycle rows (shown
          -- for context) don't inflate the included-tier counter and produce
          -- incorrect "Est. cost" for current-cycle days.
          -- Fold over ascending order; cons prepends so the result is newest-first.
          withRunning =
            fst
              $ foldl'
                ( \(xs, acc) (day, n) ->
                    let acc' = (if day < cycleStartDay then 0 else acc) + n
                     in ((day, n, acc' - n, acc') : xs, acc')
                )
                ([], 0 :: Int64)
                ascending
          dayCostText prev cur
            | isFree = "—"
            | dayOverage <= 0 = "—"
            | otherwise = "$" <> toText (printf "%.2f" (fromIntegral dayOverage / 1_000_000 :: Double) :: String)
            where
              dayOverage = max 0 (cur - included) - max 0 (prev - included)
      div_ [class_ "border border-strokeWeak rounded-md overflow-hidden max-h-96 overflow-y-auto"] do
        table_ [class_ "w-full text-sm tabular-nums"] do
          thead_ [class_ "bg-fillWeak text-textWeak text-xs uppercase tracking-wide sticky top-0"] do
            tr_ do
              th_ [class_ "text-left font-medium px-3 py-2"] "Date"
              th_ [class_ "text-right font-medium px-3 py-2"] "Requests"
              th_ [class_ "text-left font-medium px-3 py-2 w-1/3"] "Volume"
              th_ [class_ "text-right font-medium px-3 py-2"] "Est. cost"
          tbody_ do
            forM_ withRunning \(day, n, prev, cur) -> do
              let pct = max 1 $ min 100 $ (n * 100) `div` maxDay
              tr_ [class_ "border-t border-strokeWeak"] do
                td_ [class_ "px-3 py-2 text-textStrong"] $ toHtml $ toText (formatTime defaultTimeLocale "%a %b %e" day)
                td_ [class_ "px-3 py-2 text-right text-textStrong"] $ toHtml @Text $ fmt (commaizeF n)
                td_ [class_ "px-3 py-2"] do
                  div_ [class_ "h-1.5 bg-fillWeak rounded-full overflow-hidden"] do
                    div_ [class_ "h-full bg-fillBrand", style_ ("width: " <> show pct <> "%")] mempty
                td_ [class_ "px-3 py-2 text-right text-textWeak"] $ toHtml $ dayCostText prev cur
      let cycleStartText = toText (formatTime defaultTimeLocale "%b %-d" cycleStartDay)
      when (activeDays < 30)
        $ div_ [class_ "text-xs text-textWeak"]
        $ toHtml
        $ show activeDays
        <> " day"
        <> (if activeDays == 1 then "" else "s")
        <> " with activity since "
        <> cycleStartText
        <> "."
      div_ [class_ "text-xs text-textWeak"] do
        if isFree
          then "Free plan — usage shown for reference only."
          else toHtml $ "Cycle started " <> cycleStartText <> ". First 20M requests are included in the $29 plan price. Overage is $1 per 1M requests."


----------------------------------------------------------------------
-- Stripe
----------------------------------------------------------------------

verifyStripeSignature :: UTCTime -> Text -> ByteString -> ByteString -> Bool
verifyStripeSignature now sigHeader payload secret =
  case (parseTimestamp sigHeader, parseSignatures sigHeader) of
    (Just ts, sigs)
      | not (null sigs)
      , Just epoch <- readMaybe @Integer (decodeUtf8 ts)
      , abs (round (utcTimeToPOSIXSeconds now) - epoch) <= (300 :: Integer) ->
          let signedPayload = ts <> "." <> payload
              expected = BA.convert (HMAC.hmac secret signedPayload :: HMAC.HMAC SHA256) :: ByteString
           in any (BA.constEq expected) (mapMaybe decodeHexSig sigs)
    _ -> False
  where
    parts = T.splitOn ","
    parseTimestamp h = listToMaybe [encodeUtf8 v | p <- parts h, Just v <- [T.stripPrefix "t=" p]]
    parseSignatures h = [v | p <- parts h, Just v <- [T.stripPrefix "v1=" p]]
    decodeHexSig = rightToMaybe . B16.decode . encodeUtf8


stripeOpts :: Text -> Wreq.Options
stripeOpts apiKey = Wreq.defaults & (Wreq.header "Authorization" .~ ["Bearer " <> encodeUtf8 apiKey])


stripeRequest :: Text -> Text -> [(ByteString, ByteString)] -> IO (Wreq.Response LByteString)
stripeRequest apiKey endpoint =
  Wreq.postWith (stripeOpts apiKey) ("https://api.stripe.com/v1/" <> toString endpoint)


-- | `trialEligible` grants a 30-day trial at checkout. Callers should pass True only
-- for first-time upgrades on GraduatedPricing (not plan switches / migrations / BYOS),
-- to avoid giving repeat trials to already-paying or returning customers.
createStripeCheckoutSession :: Bool -> Text -> Text -> Projects.ProjectId -> Text -> Text -> Text -> Text -> IO (Maybe Text)
createStripeCheckoutSession trialEligible apiKey hostUrl pid plan priceIdGraduated priceIdOverage priceIdBYOS = do
  let basePrice = case plan of
        "SystemsPricing" -> priceIdBYOS
        _ -> priceIdGraduated
      prices =
        [ ("line_items[0][price]", encodeUtf8 basePrice)
        , ("line_items[0][quantity]", "1")
        , ("line_items[1][price]", encodeUtf8 priceIdOverage)
        ]
      trialParams = [("subscription_data[trial_period_days]", "30") | trialEligible && plan /= "SystemsPricing"]
      params =
        prices
          <> trialParams
          <> [ ("mode", "subscription")
             , ("client_reference_id", encodeUtf8 pid.toText)
             , ("metadata[project_id]", encodeUtf8 pid.toText)
             , ("metadata[plan]", encodeUtf8 plan)
             , ("success_url", encodeUtf8 $ hostUrl <> "p/" <> pid.toText <> "/manage_billing?stripe_success=1")
             , ("cancel_url", encodeUtf8 $ hostUrl <> "p/" <> pid.toText <> "/manage_billing")
             ]
  resp <- stripeRequest apiKey "checkout/sessions" params
  let body = resp ^. Wreq.responseBody
  pure $ AE.decode @AE.Value body >>= jsonField "url"


createStripePortalSession :: Text -> Text -> Text -> IO (Maybe Text)
createStripePortalSession apiKey customerId returnUrl = do
  resp <-
    stripeRequest
      apiKey
      "billing_portal/sessions"
      [ ("customer", encodeUtf8 customerId)
      , ("return_url", encodeUtf8 returnUrl)
      ]
  let body = resp ^. Wreq.responseBody
  pure $ AE.decode @AE.Value body >>= jsonField "url"


cancelStripeSubscription :: Text -> Text -> IO ()
cancelStripeSubscription apiKey subId =
  void $ Wreq.deleteWith (stripeOpts apiKey) ("https://api.stripe.com/v1/subscriptions/" <> toString subId)


lemonSqueezyOpts :: Text -> Wreq.Options
lemonSqueezyOpts apiKey =
  Wreq.defaults
    & (Wreq.header "Authorization" .~ ["Bearer " <> encodeUtf8 apiKey])
    & (Wreq.header "Content-Type" .~ ["application/vnd.api+json"])


cancelLemonSqueezySubscription :: Text -> Text -> IO ()
cancelLemonSqueezySubscription apiKey subId =
  void $ Wreq.deleteWith (lemonSqueezyOpts apiKey) ("https://api.lemonsqueezy.com/v1/subscriptions/" <> toString subId)


-- Extract a field from a JSON value (safe, returns Nothing for non-objects)
jsonField :: AE.FromJSON a => AE.Key -> AE.Value -> Maybe a
jsonField k = parseMaybe (AE.withObject "" (AE..: k))


stripeWebhookPostH :: Maybe Text -> ByteString -> ATBaseCtx (Html ())
stripeWebhookPostH sigHeaderM rawBody = do
  envConfig <- asks env
  now <- Time.currentTime
  let secret = envConfig.stripeWebhookSecret
  case sigHeaderM of
    Nothing -> do
      Log.logAttention "Stripe webhook missing signature header" ()
      throwError err400{errBody = "missing signature"}
    Just sigHeader
      | not (verifyStripeSignature now sigHeader rawBody (encodeUtf8 secret)) -> do
          Log.logAttention "Stripe webhook invalid signature" ()
          throwError err400{errBody = "invalid signature"}
    Just _ -> case AE.eitherDecodeStrict rawBody of
      Left err -> do
        Log.logAttention "Stripe webhook invalid JSON" err
        throwError err400{errBody = "invalid json"}
      Right event -> do
        let eventType = jsonField "type" event :: Maybe Text
            sessionObj = jsonField "data" event >>= jsonField "object" :: Maybe AE.Value
            notifyMembers pid (subj, html) = Notify.runNotifyProduction do
              users <- Projects.usersByProjectId pid
              forM_ users \u -> sendRenderedEmail (CI.original u.email) subj (ET.renderEmail subj html)
            billingUrl pid = envConfig.hostUrl <> "p/" <> pid.toText <> "/manage_billing"
        case (eventType, sessionObj) of
          (Just "checkout.session.completed", Just obj) -> handleStripeCheckout envConfig obj notifyMembers billingUrl
          (Just "customer.subscription.deleted", Just obj) -> handleStripeSubDeleted obj notifyMembers billingUrl
          (Just "customer.subscription.updated", Just obj) -> handleStripeSubUpdated obj
          (Just "customer.subscription.paused", Just obj) -> handleStripeSubPaused obj notifyMembers billingUrl
          (Just "customer.subscription.resumed", Just obj) -> handleStripeSubResumed envConfig obj notifyMembers billingUrl
          (Just "invoice.payment_failed", Just obj) -> handleStripePaymentFailed obj notifyMembers billingUrl
          _ -> pure ""


handleStripeCheckout :: EnvConfig -> AE.Value -> (Projects.ProjectId -> (Text, Html ()) -> ATBaseCtx ()) -> (Projects.ProjectId -> Text) -> ATBaseCtx (Html ())
handleStripeCheckout envConfig obj notifyMembers billingUrl = do
  let pidM = jsonField "client_reference_id" obj :: Maybe Text
      subIdM = jsonField "subscription" obj :: Maybe Text
      customerIdM = jsonField "customer" obj :: Maybe Text
      planFromMetadata = (jsonField "metadata" obj :: Maybe AE.Value) >>= jsonField "plan" :: Maybe Text
  case (pidM >>= Projects.projectIdFromText, subIdM, customerIdM) of
    (Just pid, Just subId, Just customerId) -> do
      let plan = fromMaybe "GraduatedPricing" planFromMetadata
      projectM <- Projects.projectById pid
      -- Idempotency: skip if this subscription is already set on the project
      if maybe False (\p -> p.subId == Just subId) projectM
        then pure "already processed"
        else do
          -- Cancel any existing LemonSqueezy subscription (auto-migration)
          whenJust projectM \project ->
            case Projects.billingProvider project.subId of
              Projects.LemonSqueezyProvider ->
                whenJust project.subId
                  $ liftIO
                  . cancelLemonSqueezySubscription envConfig.lemonSqueezyApiKey
              _ -> pass
          subDetails <- liftIO $ BJ.getStripeSubDetails envConfig.stripeSecretKey subId
          when (isNothing subDetails) $ Log.logAttention "Stripe sub fetch failed after checkout" (pid.toText, subId)
          let subItemId = maybe "" (\BJ.StripeSubDetails{subItemId = i} -> i) subDetails
          void $ Projects.updateStripeProjectBilling pid plan subId subItemId customerId
          void $ ProjectMembers.activateAllMembers pid
          case subDetails of
            Just BJ.StripeSubDetails{trialEnd = Just epoch} -> BJ.scheduleTrialReminders pid epoch
            Just BJ.StripeSubDetails{status = "trialing"} -> Log.logAttention "Stripe sub trialing but trial_end missing; no reminders scheduled" (pid.toText, subId)
            _ -> pass
          whenJust projectM \project ->
            notifyMembers pid $ ET.planUpgradedEmail project.title plan (billingUrl pid)
          pure "checkout processed"
    _ -> do
      Log.logAttention "Stripe checkout missing project/sub/customer" (pidM, subIdM, customerIdM)
      pure "missing fields"


-- Returns (first subscription item id, first price id) so callers can reverse-map
-- the price id back to our internal plan name.
getStripeSubItemAndPrice :: Text -> Text -> IO (Maybe (Text, Text))
getStripeSubItemAndPrice apiKey subId =
  fmap (\BJ.StripeSubDetails{subItemId = i, priceId = p} -> (i, p)) <$> BJ.getStripeSubDetails apiKey subId


handleStripeSubDeleted :: AE.Value -> (Projects.ProjectId -> (Text, Html ()) -> ATBaseCtx ()) -> (Projects.ProjectId -> Text) -> ATBaseCtx (Html ())
handleStripeSubDeleted obj notifyMembers billingUrl = do
  let subIdM = jsonField "id" obj :: Maybe Text
  case subIdM of
    Just subId -> do
      rows <- Projects.downgradeToFreeBySubId subId
      when (rows == 0) $ Log.logAttention "Stripe subscription.deleted: no project for sub_id" subId
      when (rows > 1) $ Log.logAttention "Stripe subscription.deleted touched multiple rows" (subId, rows)
      whenJustM (Projects.projectBySubId subId) \project ->
        notifyMembers project.id $ ET.planDowngradedEmail project.title "was cancelled" (billingUrl project.id)
      pure "subscription deleted"
    Nothing -> do
      Log.logAttention "Stripe subscription.deleted: missing sub id" ()
      pure "missing sub id"


handleStripeSubUpdated :: AE.Value -> ATBaseCtx (Html ())
handleStripeSubUpdated obj = do
  let subIdM = jsonField "id" obj :: Maybe Text
      newItemIdM = (jsonField "items" obj >>= jsonField "data" :: Maybe [AE.Value]) >>= listToMaybe >>= jsonField "id" :: Maybe Text
  case (subIdM, newItemIdM) of
    (Just subId, Just newItemId) ->
      void $ Projects.updateSubItemIdBySubId newItemId subId
    _ -> Log.logAttention "Stripe subscription.updated missing sub_id or item_id" (subIdM, newItemIdM)
  pure "subscription updated"


-- Notification only — Stripe retries payments automatically over its dunning period.
-- Downgrade happens via customer.subscription.deleted when Stripe gives up.
handleStripePaymentFailed :: AE.Value -> (Projects.ProjectId -> (Text, Html ()) -> ATBaseCtx ()) -> (Projects.ProjectId -> Text) -> ATBaseCtx (Html ())
handleStripePaymentFailed obj notifyMembers billingUrl = do
  let customerIdM = jsonField "customer" obj :: Maybe Text
      attemptCount = jsonField "attempt_count" obj :: Maybe Int
      nextAttempt = jsonField "next_payment_attempt" obj :: Maybe Int
  case customerIdM of
    Just customerId -> do
      whenJustM (Projects.projectByCustomerId customerId) \project -> do
        Log.logAttention "Stripe invoice.payment_failed" (project.id, attemptCount, nextAttempt)
        notifyMembers project.id $ ET.planDowngradedEmail project.title "payment failed" (billingUrl project.id)
    Nothing -> pass
  pure "payment failed handled"


-- Preserve IDs on pause so resume can restore plan without re-checkout.
handleStripeSubPaused :: AE.Value -> (Projects.ProjectId -> (Text, Html ()) -> ATBaseCtx ()) -> (Projects.ProjectId -> Text) -> ATBaseCtx (Html ())
handleStripeSubPaused obj notifyMembers billingUrl = do
  let subIdM = jsonField "id" obj :: Maybe Text
  case subIdM of
    Just subId -> do
      rows <- Projects.downgradeToFreeBySubId subId
      when (rows == 0) $ Log.logAttention "Stripe subscription.paused: no project for sub_id" subId
      when (rows > 1) $ Log.logAttention "Stripe subscription.paused touched multiple rows" (subId, rows)
      when (rows > 0)
        $ whenJustM (Projects.projectBySubId subId) \project ->
          notifyMembers project.id $ ET.planDowngradedEmail project.title "was paused" (billingUrl project.id)
      pure "subscription paused"
    Nothing -> do
      Log.logAttention "Stripe subscription.paused: missing sub id" ()
      pure "missing sub id"


-- Look up the sub's price via the Stripe API, reverse-map to our plan name, and re-upgrade.
handleStripeSubResumed :: EnvConfig -> AE.Value -> (Projects.ProjectId -> (Text, Html ()) -> ATBaseCtx ()) -> (Projects.ProjectId -> Text) -> ATBaseCtx (Html ())
handleStripeSubResumed envConfig obj notifyMembers billingUrl = do
  let subIdM = jsonField "id" obj :: Maybe Text
  case subIdM of
    Just subId -> do
      itemAndPriceM <- liftIO $ getStripeSubItemAndPrice envConfig.stripeSecretKey subId
      case itemAndPriceM of
        Just (itemId, priceId) -> do
          let plan
                | priceId == envConfig.stripePriceIdByos = "SystemsPricing"
                | otherwise = "GraduatedPricing"
          rows <- Projects.setPlanBySubId plan itemId subId
          when (rows == 0) $ Log.logAttention "Stripe subscription.resumed: no project for sub_id" (subId, plan)
          when (rows > 1) $ Log.logAttention "Stripe subscription.resumed touched multiple rows" (subId, rows)
          when (rows > 0)
            $ whenJustM (Projects.projectBySubId subId) \project ->
              notifyMembers project.id $ ET.planUpgradedEmail project.title plan (billingUrl project.id)
          pure "subscription resumed"
        Nothing -> do
          Log.logAttention "Stripe subscription.resumed: could not fetch sub items" subId
          pure "missing items"
    Nothing -> do
      Log.logAttention "Stripe subscription.resumed: missing sub id" ()
      pure "missing sub id"
