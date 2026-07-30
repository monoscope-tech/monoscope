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
  -- Prometheus
  prometheusGetH,
  prometheusPostH,
  prometheusUpdateH,
  prometheusTestH,
  prometheusDeleteH,
  prometheusToggleH,
  PrometheusForm (..),
  PrometheusGet (..),
  PrometheusMut (..),
  safeScrapeUrl,
  parseLabelsText,
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
import Data.Char (isDigit, isHexDigit)
import Data.Default
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Notify qualified as Notify
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (Day, UTCTime (..), addDays, addUTCTime, diffUTCTime, getZonedTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.Log qualified as Log
import Hasql.Interpolate qualified as HI

import BackgroundJobs (errorTrendChartUrl)
import BackgroundJobs qualified as BJ
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Effectful.Wreq qualified as W
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Htmx (hxConfirm_, hxDelete_, hxGet_, hxIndicator_, hxPatch_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers (Team (..), getTeamsById, resolveTeamEmails)
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (urlEncode)
import Network.Minio qualified as Minio
import Network.URI (parseURI, uriAuthority, uriRegName, uriScheme)
import Network.Wreq qualified as Wreq
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, settingsContentTarget, withSettingsPage)
import Pages.Components (BadgeColor (..), EmptyStateCfg (..), FieldCfg (..), FieldSize (..), ModalCfg (..), confirmModal_, connectionBadge_, emptyState_, formField_, headerRow_, iconBadgeLg_, localTimeFmt_, modalWith_, paymentPlanPicker, sectionLabel_, settingsH2_, settingsSection_)
import Pkg.Components.Table qualified as Table
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.EmailTemplates qualified as ET
import Pkg.Mail (NotificationAlerts (..), sampleAlertByIssueTypeText, sampleReport, sendDiscordAlert, sendPagerdutyAlertToService, sendRenderedEmail, sendSlackAlert, sendWhatsAppAlert)
import Pkg.Prometheus qualified as Prom
import Relude hiding (ask, asks)
import Servant (err400, errBody)
import System.Config
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent)
import Text.Printf (printf)
import UnliftIO.Exception (throwIO, try, tryAny)
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


-- | Turn a minio-hs error into something a user can act on, instead of leaking
-- the library's raw @Show@ (e.g. @MErrValidation (MErrVRegionNotSupported "EU")@).
--
-- The humanized message must never expose the library constructor and must
-- steer the user to a valid region:
--
-- >>> let msg = humanizeMinioErr (Minio.MErrValidation (Minio.MErrVRegionNotSupported "EU"))
-- >>> ("MErrV" `T.isInfixOf` msg, "us-east-1" `T.isInfixOf` msg)
-- (False,True)
humanizeMinioErr :: Minio.MinioErr -> Text
humanizeMinioErr = \case
  Minio.MErrValidation (Minio.MErrVRegionNotSupported r) ->
    "\"" <> r <> "\" isn't a valid AWS region. Use a region code like us-east-1 or eu-west-1. For MinIO, DigitalOcean Spaces, Cloudflare R2 or other S3-compatible providers, set the Custom Endpoint below — then any region your provider expects is accepted."
  Minio.MErrValidation (Minio.MErrVInvalidBucketName b) -> "\"" <> b <> "\" isn't a valid bucket name."
  Minio.MErrValidation Minio.MErrVMissingCredentials -> "Access Key ID and Secret Access Key are required."
  Minio.MErrValidation _ -> "Couldn't validate the S3 settings. Double-check the region, bucket name and endpoint."
  Minio.MErrService Minio.NoSuchBucket -> "That bucket doesn't exist for these credentials in this region."
  Minio.MErrService Minio.InvalidBucketName -> "The bucket name is invalid."
  Minio.MErrService (Minio.ServiceErr code msg)
    | code `elem` ["AccessDenied", "InvalidAccessKeyId", "SignatureDoesNotMatch", "InvalidSecurity"] ->
        "Access denied — check that the Access Key ID and Secret Access Key are correct and can access this bucket."
    | otherwise -> "S3 rejected the request (" <> code <> "): " <> msg
  Minio.MErrService _ -> "S3 rejected the request. Check your credentials and bucket permissions."
  Minio.MErrHTTP _ -> "Couldn't reach the S3 endpoint. Check the Custom Endpoint URL, region and bucket name, then try again."
  Minio.MErrIO _ -> "Network error while reaching S3. Check your connection and endpoint, then try again."


-- | Common AWS region codes for the region combobox. Free text is still
-- allowed so S3-compatible providers (which ignore the region) can pass anything.
awsRegions :: [(Text, Text)]
awsRegions =
  [ ("us-east-1", "US East (N. Virginia)")
  , ("us-east-2", "US East (Ohio)")
  , ("us-west-1", "US West (N. California)")
  , ("us-west-2", "US West (Oregon)")
  , ("ca-central-1", "Canada (Central)")
  , ("eu-west-1", "Europe (Ireland)")
  , ("eu-west-2", "Europe (London)")
  , ("eu-west-3", "Europe (Paris)")
  , ("eu-central-1", "Europe (Frankfurt)")
  , ("eu-north-1", "Europe (Stockholm)")
  , ("ap-south-1", "Asia Pacific (Mumbai)")
  , ("ap-southeast-1", "Asia Pacific (Singapore)")
  , ("ap-southeast-2", "Asia Pacific (Sydney)")
  , ("ap-northeast-1", "Asia Pacific (Tokyo)")
  , ("sa-east-1", "South America (São Paulo)")
  ]


brings3PostH :: Projects.ProjectId -> Projects.ProjectS3Bucket -> ATAuthCtx (RespHeaders (Html ()))
brings3PostH pid s3Form = do
  let connectInfo = getMinioConnectInfo s3Form.accessKey s3Form.secretKey s3Form.region s3Form.bucket s3Form.endpointUrl
  res <- liftIO $ Minio.runMinio connectInfo $ Minio.bucketExists s3Form.bucket
  case res of
    Left err -> do
      addErrorToast (humanizeMinioErr err) Nothing
      addRespHeaders $ connectionBadge_ "Not connected"
    Right bExists ->
      if bExists
        then do
          _ <- Projects.updateProjectS3Bucket pid $ Just s3Form
          sess <- Projects.getSession
          Projects.logAuditS pid Projects.AES3Configured sess Nothing
          addSuccessToast "Connected successfully" Nothing
          addRespHeaders $ connectionBadge_ "Connected"
        else do
          addErrorToast "Bucket does not exist" Nothing
          addRespHeaders $ connectionBadge_ "Not connected"


brings3RemoveH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
brings3RemoveH pid = do
  (sess, _) <- Projects.sessionAndProject pid
  _ <- Projects.updateProjectS3Bucket pid Nothing
  Projects.logAuditS pid Projects.AES3Removed sess Nothing
  addSuccessToast "Removed S3 bucket" Nothing
  addRespHeaders $ connectionBadge_ "Not connected"


bringS3GetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
bringS3GetH pid = withSettingsPage pid "Integrations" \project -> pure $ bringS3Page pid project.s3Bucket


bringS3Page :: Projects.ProjectId -> Maybe Projects.ProjectS3Bucket -> Html ()
bringS3Page pid s3BucketM = settingsSection_ do
  headerRow_ [] do
    settingsH2_ "S3 Bucket"
    div_ [id_ "connectedInd"] $ connectionBadge_ $ bool "Not connected" "Connected" (isJust s3BucketM)

  form_ [class_ "space-y-4", hxPost_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd", hxIndicator_ "#indicator"] do
    div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
      formField_ FieldSm def{value = maybe "" (.accessKey) s3BucketM, placeholder = "Access Key ID"} "Access Key ID" "accessKey" True Nothing
      formField_ FieldSm def{inputType = "password", value = maybe "" (.secretKey) s3BucketM, placeholder = "Secret Access Key"} "Secret Access Key" "secretKey" True Nothing
      formField_ FieldSm def{value = maybe "us-east-1" (.region) s3BucketM, placeholder = "us-east-1", extraAttrs = [term "list" "aws-regions"]} "Region" "region" True Nothing
      formField_ FieldSm def{value = maybe "" (.bucket) s3BucketM, placeholder = "Bucket Name"} "Bucket Name" "bucket" True Nothing
    datalist_ [id_ "aws-regions"] $ forM_ awsRegions \(code, label) -> option_ [value_ code] $ toHtml label
    formField_ FieldSm def{value = maybe "" (.endpointUrl) s3BucketM, placeholder = "https://s3.example.com", extraAttrs = [pattern_ "https?://.*"]} "Custom Endpoint" "endpointUrl" False Nothing
    p_ [class_ "text-xs text-textWeak"] "Leave blank for AWS S3. For S3-compatible providers (MinIO, DigitalOcean Spaces, Cloudflare R2, Backblaze, OVH), set the endpoint URL and enter the region your provider expects — e.g. auto for R2, nyc3 for DigitalOcean, or us-east-1 for MinIO."

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
  (sess, _) <- Projects.sessionAndProject pid
  authCtx <- ask @AuthContext
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let encryptedKeyB64 = ProjectApiKeys.encodeApiKeyB64 authCtx.config.apiKeyEncryptionSecretKey projectKeyUUID
  pApiKey <- ProjectApiKeys.newProjectApiKeys pid projectKeyUUID apiKeyForm.title encryptedKeyB64
  ProjectApiKeys.insertProjectApiKey pApiKey
  apiKeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  Projects.logAuditS pid Projects.AEApiKeyCreated sess
    $ Just
    $ AE.object ["key_title" AE..= apiKeyForm.title]
  addSuccessToast "Created API Key Successfully" Nothing
  addTriggerEvent "closeModal" ""
  let newKey = Just (pApiKey, encryptedKeyB64)
  addRespHeaders $ maybe (ApiPost pid apiKeys newKey) (const $ ApiPostCopy newKey True) apiKeyForm.from


apiDeleteH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiDeleteH pid = apiKeySetActive pid ProjectApiKeys.revokeApiKey Projects.AEApiKeyRevoked "Revoked"


apiActivateH :: Projects.ProjectId -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiActivateH pid = apiKeySetActive pid ProjectApiKeys.activateApiKey Projects.AEApiKeyActivated "Activated"


apiKeySetActive :: Projects.ProjectId -> (ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx Int64) -> Projects.AuditEvent -> Text -> ProjectApiKeys.ProjectApiKeyId -> ATAuthCtx (RespHeaders ApiMut)
apiKeySetActive pid act event verb keyid = do
  (sess, _) <- Projects.sessionAndProject pid
  res <- act keyid
  apikeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  if res > 0
    then do
      Projects.logAuditS pid event sess Nothing
      addSuccessToast (verb <> " API Key Successfully") Nothing
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
  (_, _, bw) <- mkPageCtx pid
  apiKeys <- V.fromList <$> ProjectApiKeys.projectApiKeysByProjectId pid
  addRespHeaders $ ApiGet $ PageCtx bw{pageTitle = "API Keys", isSettingsPage = True} (pid, apiKeys)


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
  let (activeKeys, revokedKeys) = V.partition (.active) apiKeys
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
  div_ [class_ "a-tab-content", id_ "active_content"] $ toHtml $ makeApiKeysTable pid activeKeys "active_content"
  div_ [class_ "hidden a-tab-content", id_ "revoked_content"] $ toHtml $ makeApiKeysTable pid revokedKeys "revoked_content"


makeApiKeysTable :: Projects.ProjectId -> V.Vector ProjectApiKeys.ProjectApiKey -> Text -> Table.Table ProjectApiKeys.ProjectApiKey
makeApiKeysTable pid apiKeys elemId =
  Table.Table
    { config = def{Table.elemID = elemId, Table.renderAsTable = True}
    , columns = apiKeyColumns pid
    , rows = apiKeys
    , features = Table.Features{rowLink = Nothing, rowId = Nothing, rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"], selectRow = Nothing, bulkActions = [], search = Nothing, tabs = Nothing, sort = Nothing, sortableColumns = Nothing, tableHeaderActions = Nothing, pagination = Nothing, zeroState = Just Table.ZeroState{icon = "key", title = "No API keys", description = "Create an API key to start integrating with your project.", actionText = "", destination = Right ""}, header = Nothing, treeConfig = Nothing}
    }


apiKeyColumns :: Projects.ProjectId -> [Table.Column ProjectApiKeys.ProjectApiKey]
apiKeyColumns pid =
  [ Table.col "Title" \apiKey ->
      span_ [class_ "text-textStrong font-semibold text-sm truncate min-w-0 block max-w-48"] $ toHtml apiKey.title
  , Table.col "Key" \apiKey -> do
      -- Reveal is pure CSS: the sr-only checkbox flips both spans and both eye labels
      -- via `group-has-[:checked]` on the row container (no hyperscript, survives morph).
      -- Keyed by key id, not row index — the active and archived tables both index from 0.
      let revealId = "reveal-key-" <> apiKey.id.toText
      div_ [class_ "group whitespace-nowrap w-full flex items-center gap-2 text-sm text-textWeak"] do
        input_ [type_ "checkbox", id_ revealId, class_ "sr-only"]
        span_ [class_ "min-w-0 group-has-[:checked]:hidden"] $ toHtml $ T.take 8 apiKey.keyPrefix <> T.replicate 20 "*"
        span_ [class_ "min-w-0 hidden group-has-[:checked]:inline"] $ toHtml apiKey.keyPrefix
        div_ [class_ "flex items-center gap-1.5 shrink-0 ml-auto"] do
          label_ [Lucid.for_ revealId, class_ "p-1 rounded hover:bg-fillWeaker cursor-pointer group-has-[:checked]:hidden", term "data-tippy-content" "Show key"]
            $ faSprite_ "eye" "regular" "h-3.5 w-3.5 text-iconNeutral"
          label_ [Lucid.for_ revealId, class_ "p-1 rounded hover:bg-fillWeaker cursor-pointer hidden group-has-[:checked]:block", term "data-tippy-content" "Hide key"]
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
            , term "data-tippy-content" tip
            ]
            $ faSprite_ icon "regular"
            $ "h-3.5 w-3.5 "
            <> iconCls
  ]


copyNewApiKey :: Maybe (ProjectApiKeys.ProjectApiKey, Text) -> Bool -> Html ()
copyNewApiKey newKeyM hasNext = whenJust newKeyM \(_, newKey) ->
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
              if hasNext
                then button_ [type_ "button", class_ "btn btn-sm btn-ghost text-textBrand ml-4", [__|on click call window.location.reload()|]] "Next"
                else button_ [type_ "button", class_ "btn btn-sm btn-ghost text-textSuccess ml-2", [__|on click remove #apiFeedbackSection|]] "Dismiss"


----------------------------------------------------------------------
-- Prometheus scrape targets
----------------------------------------------------------------------

data PrometheusForm = PrometheusForm
  { name :: Text
  , url :: Text
  , scrapeInterval :: Maybe Int
  , authHeader :: Maybe Text
  , extraLabels :: Maybe Text
  , clearAuth :: Maybe Text -- "on" when the "Clear saved token" box is ticked
  }
  deriving stock (Generic)
  deriving anyclass (FromForm)


newtype PrometheusGet = PrometheusGet (PageCtx (Projects.ProjectId, V.Vector PromCfg.PrometheusScrapeConfig))


instance ToHtml PrometheusGet where
  toHtml (PrometheusGet (PageCtx bwconf (pid, cfgs))) = toHtml $ PageCtx bwconf $ prometheusPage pid cfgs
  toHtmlRaw = toHtml


newtype PrometheusMut = PrometheusMut (Projects.ProjectId, V.Vector PromCfg.PrometheusScrapeConfig)


instance ToHtml PrometheusMut where
  toHtml (PrometheusMut (pid, cfgs)) = toHtml $ prometheusTargetsList pid cfgs
  toHtmlRaw = toHtml


prometheusGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders PrometheusGet)
prometheusGetH pid = do
  (_, _, bw) <- mkPageCtx pid
  cfgs <- PromCfg.configsByProjectId pid
  addRespHeaders $ PrometheusGet $ PageCtx bw{pageTitle = "Prometheus", isSettingsPage = True} (pid, cfgs)


-- | A 'PrometheusForm' that passed validation: trimmed, SSRF-checked, interval floored.
data ScrapeTarget = ScrapeTarget
  { name :: Text
  , url :: Text
  , interval :: Int
  , authHeader :: Maybe Text
  , labels :: AE.Value
  }


-- | Validate + normalise a target form. Name is required because it becomes the
-- @service.name@ the scraped metrics are grouped under — blank names would silently
-- collide multiple targets into one series namespace.
validatePrometheusForm :: PrometheusForm -> Either Text ScrapeTarget
validatePrometheusForm form = do
  url <- validateScrapeUrl form.url
  when (T.null name) $ Left "A name is required — it groups the scraped metrics under service.name"
  Right
    ScrapeTarget
      { name
      , url
      , interval = max 60 (fromMaybe 60 form.scrapeInterval) -- floor at the 60s dispatcher cadence
      , authHeader = mfilter (not . T.null) (T.strip <$> form.authHeader)
      , labels = parseLabelsText (fromMaybe "" form.extraLabels)
      }
  where
    name = T.strip form.name


-- | The URL guards shared by save-validation and the Test handler: non-empty + SSRF floor.
-- Returns the stripped URL so callers don't re-strip or re-check.
validateScrapeUrl :: Text -> Either Text Text
validateScrapeUrl raw
  | T.null url = Left "A /metrics URL is required"
  | not (safeScrapeUrl url) = Left "URL must be a public http:// or https:// endpoint"
  | otherwise = Right url
  where
    url = T.strip raw


-- | Reject non-http(s) schemes and hosts that resolve to the internal network
-- (loopback, link-local, RFC-1918, ULA, IPv4-mapped IPv6, and obfuscated numeric
-- literals — octal/decimal/short forms), to blunt SSRF via the scrape URL. DNS-rebinding
-- remains a known gap (we only vet the literal host; resolve-and-pin is the full fix).
--
-- >>> map safeScrapeUrl ["https://metrics.example.com/m", "http://10.0.0.1/m", "http://[::ffff:127.0.0.1]/m", "file:///etc/passwd"]
-- [True,False,False,False]
-- >>> map safeScrapeUrl ["http://2130706433/m", "http://0177.0.0.1/m", "http://localhost./m", "http://172.160.0.1/m"]
-- [False,False,False,True]
-- >>> map safeScrapeUrl ["http://0x7f000001/m", "http://0x7f.0x0.0x0.0x1/m"]
-- [False,False]
-- >>> map safeScrapeUrl ["http://[fe9f::1]/m", "http://[fc00::1]/m", "http://[2001:db8::1]/m"]
-- [False,False,True]
safeScrapeUrl :: Text -> Bool
safeScrapeUrl u = case parseURI (toString (T.strip u)) of
  Nothing -> False
  Just uri ->
    T.toLower (toText (uriScheme uri))
      `elem` ["http:", "https:"]
      && not (internalHost (T.toLower (toText (maybe "" uriRegName (uriAuthority uri)))))
  where
    internalHost raw =
      let h = fromMaybe raw (T.stripSuffix "." raw) -- normalise the trailing-dot FQDN form
          ip6 = fromMaybe h (T.stripPrefix "[" h >>= T.stripSuffix "]") -- inside of a [..] literal
       in T.null h
            || h
            == "localhost"
            || any (`T.isPrefixOf` ip6) ["::1", "fc", "fd", "fe8", "fe9", "fea", "feb", "::ffff:"] -- fc/fd = ULA fc00::/7; fe8-feb = link-local fe80::/10
            || internalIPv4 h
            || obfuscatedNumericHost h
            || hexNumericHost h
    -- A clean dotted-decimal quad whose address is loopback/RFC-1918/link-local/unspecified.
    internalIPv4 h = case traverse octet (T.splitOn "." h) of
      Just [a, b, _, _] -> a `elem` [0, 127] || a == 10 || (a == 169 && b == 254) || (a == 172 && b >= 16 && b <= 31) || (a == 192 && b == 168)
      _ -> False
    -- Any all-numeric host that is not a clean 4-octet quad: bare integer (2130706433),
    -- octal octets (0177.0.0.1), or short forms (127.1) — all of which an OS resolver
    -- expands to an internal address.
    obfuscatedNumericHost h =
      not (T.null h)
        && T.all (\c -> isDigit c || c == '.') h
        && maybe True ((/= 4) . length) (traverse octet (T.splitOn "." h))
    -- Reject hex IP literals (0x7f000001, 0x7f.0x0.0x0.0x1): any component is "0x"+hex digits.
    hexNumericHost h = any hexLiteral (T.splitOn "." h)
      where
        hexLiteral p = case T.stripPrefix "0x" p of
          Just rest -> not (T.null rest) && T.all isHexDigit rest
          Nothing -> False
    octet o = do
      guard $ not (T.null o) && (T.length o == 1 || T.head o /= '0') -- reject leading-zero (octal) octets
      n <- readMaybe (toString o) :: Maybe Int
      n <$ guard (n >= 0 && n <= 255)


prometheusPostH :: Projects.ProjectId -> PrometheusForm -> ATAuthCtx (RespHeaders PrometheusMut)
prometheusPostH pid form = promSave pid form "Added" \t -> PromCfg.insertConfig pid t.name t.url t.interval t.authHeader t.labels


prometheusUpdateH :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfigId -> PrometheusForm -> ATAuthCtx (RespHeaders PrometheusMut)
prometheusUpdateH pid cid form = promSave pid form "Updated" \t -> do
  -- The edit form never echoes the saved token back into the password field, so a blank
  -- auth field means "keep the existing token" — unless "Clear saved token" was ticked.
  keptAuth <- case t.authHeader of
    Just a -> pure (Just a)
    Nothing | isJust form.clearAuth -> pure Nothing
    Nothing -> PromCfg.getConfigByProject pid cid <&> (>>= (.authHeader))
  PromCfg.updateConfig pid cid t.name t.url t.interval keptAuth t.labels


-- | Shared create/edit flow. The DB UNIQUE(project_id, name) constraint (migration 0103)
-- is the real guard: catching its violation here — rather than a pre-check scan that two
-- concurrent saves can both pass before either inserts — closes the race, saves a roundtrip,
-- and still turns the collision into a friendly message.
promSave :: Projects.ProjectId -> PrometheusForm -> Text -> (ScrapeTarget -> ATAuthCtx Int64) -> ATAuthCtx (RespHeaders PrometheusMut)
promSave pid form verb persist = do
  _ <- Projects.sessionAndProject pid
  case validatePrometheusForm form of
    Left err -> addErrorToast err Nothing
    Right target ->
      try (persist target) >>= \case
        -- 0 rows means the edit targeted a row that no longer matches (concurrently deleted,
        -- or a cid that isn't this project's) — don't claim success for a write that did nothing.
        Right 0 -> addErrorToast "That Prometheus target no longer exists" Nothing
        Right _ -> do
          addSuccessToast (verb <> " Prometheus target") Nothing
          addTriggerEvent "closeModal" ""
        Left e
          | Hasql.isUniqueViolation e -> addErrorToast ("A target named “" <> target.name <> "” already exists") Nothing
          | otherwise -> throwIO e
  prometheusMut pid


-- | One-shot scrape used by the form's Test button: fetch + parse, no DB write, no ingest.
-- Reports sample count + latency, or a (truncated) error, so a user validates a target
-- before saving instead of saving blind and squinting at last_status a minute later.
prometheusTestH :: Projects.ProjectId -> PrometheusForm -> ATAuthCtx (RespHeaders (Html ()))
prometheusTestH pid form = do
  _ <- Projects.sessionAndProject pid
  case validateScrapeUrl form.url of
    Left err -> addRespHeaders $ prometheusTestResult $ Just $ Left err
    Right url -> do
      let opts = PromCfg.prometheusScrapeOpts (mfilter (not . T.null) (T.strip <$> form.authHeader))
      t0 <- Time.currentTime
      res <- tryAny (W.getWith opts (toString url))
      t1 <- Time.currentTime
      let ms = round (realToFrac (diffUTCTime t1 t0) * 1000 :: Double) :: Int
      addRespHeaders $ prometheusTestResult $ Just $ case res of
        Left e -> Left (T.take 300 (show e))
        Right resp ->
          -- Count only the samples ingestScrapedBody would store (shared isFiniteSample).
          let finite = filter Prom.isFiniteSample (Prom.parsePrometheus (decodeUtf8 (resp ^. Wreq.responseBody)))
           in Right (length finite, ms)


prometheusDeleteH :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfigId -> ATAuthCtx (RespHeaders PrometheusMut)
prometheusDeleteH pid cid = do
  _ <- Projects.sessionAndProject pid
  void $ PromCfg.deleteConfig pid cid
  addSuccessToast "Removed Prometheus target" Nothing
  prometheusMut pid


prometheusToggleH :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfigId -> ATAuthCtx (RespHeaders PrometheusMut)
prometheusToggleH pid cid = do
  _ <- Projects.sessionAndProject pid
  void $ PromCfg.toggleEnabled pid cid
  prometheusMut pid


prometheusMut :: Projects.ProjectId -> ATAuthCtx (RespHeaders PrometheusMut)
prometheusMut pid = do
  cfgs <- PromCfg.configsByProjectId pid
  addRespHeaders $ PrometheusMut (pid, cfgs)


-- | Parse @k=v, k2=v2@ static-label text into a JSON object, trimming whitespace and
-- dropping pairs with an empty key or empty value (so @"env= "@ is dropped, not stored blank).
--
-- >>> parseLabelsText "env = prod "
-- Object (fromList [("env",String "prod")])
-- >>> parseLabelsText "env= , =orphan"
-- Object (fromList [])
parseLabelsText :: Text -> AE.Value
parseLabelsText t =
  AE.object
    [ (AEK.fromText k', AE.String v')
    | pair <- T.splitOn "," t
    , let (k, rest) = T.breakOn "=" pair
    , let k' = T.strip k
    , not (T.null k')
    , Just v' <- [T.strip <$> T.stripPrefix "=" rest] -- explicit "skip the =", drops valueless keys
    , not (T.null v')
    ]


-- | Inverse of 'parseLabelsText', to prefill the edit form.
labelsToText :: AE.Value -> Text
labelsToText (AE.Object o) = T.intercalate ", " [AEK.toText k <> "=" <> v | (k, AE.String v) <- AEKM.toList o]
labelsToText _ = ""


-- | Scrape health derived from last_status ("ok: …" / "error: …"). Distinct from the
-- on/off state so a target that's enabled-but-failing never shows a reassuring green.
data ScrapeHealth = HealthOk | HealthError | HealthPending


configHealth :: PromCfg.PrometheusScrapeConfig -> ScrapeHealth
configHealth cfg = case T.toLower . T.strip <$> cfg.lastStatus of
  Just s | "ok" `T.isPrefixOf` s -> HealthOk
  Just s | "error" `T.isPrefixOf` s -> HealthError
  _ -> HealthPending


healthBadge_ :: ScrapeHealth -> Html ()
healthBadge_ = \case
  HealthOk -> span_ [class_ "cbadge-sm badge-success inline-flex items-center gap-1"] $ faSprite_ "circle-check" "solid" "w-3 h-3" >> "healthy"
  HealthError -> span_ [class_ "cbadge-sm badge-error inline-flex items-center gap-1"] $ faSprite_ "circle-exclamation" "solid" "w-3 h-3" >> "failing"
  HealthPending -> span_ [class_ "cbadge-sm badge-neutral"] "pending"


prometheusPage :: Projects.ProjectId -> V.Vector PromCfg.PrometheusScrapeConfig -> Html ()
prometheusPage pid cfgs = settingsSection_ do
  div_ [class_ "flex justify-between items-center"] do
    settingsH2_ "Prometheus targets"
    modalWith_ "prometheus-modal" def{boxClass = "p-8"} (Just $ span_ [class_ "btn btn-sm btn-primary gap-1.5"] $ do faSprite_ "plus" "regular" "w-3 h-3"; "Add target") do
      div_ [class_ "flex flex-col gap-5"] do
        div_ do
          h2_ [class_ "text-textStrong text-xl font-semibold"] "Scrape a Prometheus endpoint"
          p_ [class_ "text-sm text-textWeak mt-1"] "We poll this endpoint on your schedule, parse the metrics exposition format, and ingest the samples as series you can chart and alert on."
        form_ [hxPost_ $ "/p/" <> pid.toText <> "/settings/prometheus", class_ "flex flex-col gap-4", hxTarget_ "#prometheus-targets", hxSwap_ "outerHTML"]
          $ prometheusFields_ pid "prometheus-modal" "Add scrape target" Nothing
  prometheusTargetsList pid cfgs


-- | Shared add/edit form body: prefilled from a config when editing. Carries its own
-- action row (Test / Cancel / submit) so callers just wrap it in a form. @submitLabel@
-- names the commit button and @modalId@ wires Cancel to close the enclosing modal.
prometheusFields_ :: Projects.ProjectId -> Text -> Text -> Maybe PromCfg.PrometheusScrapeConfig -> Html ()
prometheusFields_ pid modalId submitLabel mcfg = do
  field_ "name" "Name" "api-gateway" "text" True Nothing (maybe "" (.name) mcfg)
  field_ "url" "Metrics URL" "http://service:9090/metrics" "url" True (Just "Must be reachable from Monoscope and return the Prometheus text exposition format.") (maybe "" (.url) mcfg)
  label_ [class_ "flex flex-col gap-1 text-sm"] do
    span_ [class_ "text-textWeak"] "Scrape interval"
    -- No sub-minute options: the dispatcher ticks once per minute, so a shorter
    -- interval can't be honoured. Always include the config's current value (even if
    -- off-list) so editing an unrelated field never silently rewrites the interval.
    let cur = maybe 60 (.scrapeIntervalSeconds) mcfg
        presets = [(60, "1 minute"), (300, "5 minutes"), (900, "15 minutes"), (3600, "1 hour")] :: [(Int, Text)]
        opts = if any ((== cur) . fst) presets then presets else sortWith fst ((cur, show cur <> "s") : presets)
    select_ [class_ "select select-bordered w-full", name_ "scrapeInterval"]
      $ forM_ opts \(v, l) ->
        option_ ([value_ (show v)] <> [selected_ "selected" | v == cur]) (toHtml l)
  -- Optional fields stay collapsed until needed; auto-expanded when editing a target
  -- that already has them set, so existing values are never hidden behind the toggle.
  let hasAdvanced = maybe False (\c -> isJust c.authHeader || not (T.null (labelsToText c.extraLabels))) mcfg
  details_ ([class_ "group"] <> [term "open" "open" | hasAdvanced]) do
    summary_ [class_ "cursor-pointer select-none text-sm text-textWeak flex items-center gap-1.5"] do
      faSprite_ "chevron-right" "solid" "w-3 h-3 transition-transform group-open:rotate-90"
      "Advanced"
    div_ [class_ "flex flex-col gap-3 mt-3"] do
      -- Never echo the saved token into the HTML (it would sit in the page source / proxy
      -- caches): show an empty field, and on edit treat blank as "keep the saved token".
      let hasToken = maybe False (isJust . (.authHeader)) mcfg
          (authPh, authHelp) =
            if hasToken
              then ("Leave blank to keep the saved token", "A token is saved. Enter a new value to replace it.")
              else ("Bearer <token>", "Sent as the Authorization header on every scrape.")
      field_ "authHeader" "Authorization header" authPh "password" False (Just authHelp) ""
      when hasToken $ label_ [class_ "flex items-center gap-2 text-sm text-textWeak -mt-1"] do
        input_ [type_ "checkbox", name_ "clearAuth", class_ "checkbox checkbox-sm"]
        "Clear saved token"
      field_ "extraLabels" "Static labels" "env=prod, team=core" "text" False (Just "Comma-separated key=value pairs, added to every series from this target.") (maybe "" (labelsToText . (.extraLabels)) mcfg)
  div_ [class_ "flex items-center gap-2 border-t border-strokeWeak pt-4"] do
    button_
      [ type_ "button"
      , class_ "btn btn-ghost gap-1.5"
      , hxPost_ $ "/p/" <> pid.toText <> "/settings/prometheus/test"
      , term "hx-include" "closest form"
      , hxTarget_ "next .prom-test-result"
      , hxSwap_ "outerHTML"
      ]
      $ do faSprite_ "circle-play" "regular" "w-3.5 h-3.5"; "Test connection"
    label_ [class_ "btn btn-ghost ml-auto", Lucid.for_ modalId] "Cancel"
    button_ [type_ "submit", class_ "btn btn-primary"] (toHtml submitLabel)
  prometheusTestResult Nothing
  where
    field_ :: Text -> Text -> Text -> Text -> Bool -> Maybe Text -> Text -> Html ()
    field_ nm lbl ph ty req helpM val = label_ [class_ "flex flex-col gap-1 text-sm"] do
      span_ [class_ "text-textWeak"] (toHtml lbl)
      input_ $ [class_ "input input-bordered w-full", type_ ty, name_ nm, placeholder_ ph, value_ val] <> [required_ "true" | req]
      whenJust helpM $ span_ [class_ "text-xs text-textWeak"] . toHtml


-- | @Nothing@ is the not-yet-tested placeholder the form renders on first paint.
prometheusTestResult :: Maybe (Either Text (Int, Int)) -> Html ()
prometheusTestResult res = div_ [class_ "prom-test-result text-sm"] $ case res of
  Nothing -> mempty
  Just (Right (n, ms)) -> span_ [class_ "cbadge-sm badge-success inline-flex items-center gap-1"] $ faSprite_ "circle-check" "solid" "w-3 h-3" >> toHtml ("Scraped " <> show n <> " samples · " <> show ms <> "ms" :: Text)
  Just (Left err) -> span_ [class_ "cbadge-sm badge-error inline-flex items-center gap-1 max-w-full"] $ faSprite_ "circle-exclamation" "solid" "w-3 h-3 shrink-0" >> span_ [class_ "break-all"] (toHtml err)


prometheusTargetsList :: Projects.ProjectId -> V.Vector PromCfg.PrometheusScrapeConfig -> Html ()
prometheusTargetsList pid cfgs = div_ [id_ "prometheus-targets", class_ "mt-4"] do
  if V.null cfgs
    then prometheusEmptyState
    else do
      input_
        [ class_ "input input-bordered input-sm w-full mb-3"
        , type_ "search"
        , placeholder_ "Filter targets…"
        , term "_" "on input show .itemsListItem in #prometheus-targets when its textContent.toLowerCase() contains my value.toLowerCase()"
        ]
      div_ [class_ "flex flex-col gap-2"] $ V.forM_ cfgs (prometheusTargetRow pid)


prometheusEmptyState :: Html ()
prometheusEmptyState = emptyState_ def{icon = Just "objects-column"} "Scrape your Prometheus endpoints" "Point Monoscope at any /metrics endpoint. We poll it on your schedule, parse the exposition format, and ingest the samples as metrics you can chart and alert on — grouped under the name you give each target. Use “Add target” to start."


prometheusTargetRow :: Projects.ProjectId -> PromCfg.PrometheusScrapeConfig -> Html ()
prometheusTargetRow pid cfg = div_ [class_ "itemsListItem flex items-center justify-between gap-3 border border-strokeWeak rounded-md p-3"] do
  div_ [class_ "flex flex-col min-w-0 gap-1"] do
    div_ [class_ "flex items-center gap-2 flex-wrap"] do
      healthBadge_ (configHealth cfg)
      span_ [class_ "text-textStrong font-medium"] $ toHtml cfg.name
      unless cfg.enabled $ span_ [class_ "cbadge-sm badge-neutral"] "paused"
      when (isJust cfg.authHeader) $ faSprite_ "lock" "solid" "w-3 h-3 text-iconNeutral"
    span_ [class_ "text-xs text-textWeak truncate"] $ toHtml cfg.url
    div_ [class_ "text-xs text-textWeak flex items-center gap-1.5 flex-wrap"] do
      toHtml ("every " <> show cfg.scrapeIntervalSeconds <> "s" :: Text)
      whenJust cfg.lastScrapedAt \t -> span_ [] $ "· scraped " >> localTimeFmt_ "MMM dd, HH:mm" t
      whenJust cfg.lastStatus \s -> span_ [class_ "truncate max-w-xs", title_ s] $ toHtml ("· " <> s)
  div_ [class_ "flex items-center gap-1 shrink-0"] do
    -- Filter the metrics explorer to this target's series (metric_source = service_name,
    -- which sampleToMetricRecord sets to the config name).
    a_ [class_ "btn btn-xs btn-ghost", href_ $ "/p/" <> pid.toText <> "/metrics?metric_source=" <> decodeUtf8 (urlEncode True (encodeUtf8 cfg.name))] "View metrics"
    modalWith_ ("prom-edit-" <> cfg.id.toText) def{boxClass = "p-8"} (Just $ span_ [class_ "btn btn-xs btn-ghost"] "Edit") do
      div_ [class_ "flex flex-col gap-5"] do
        div_ do
          h2_ [class_ "text-textStrong text-xl font-semibold"] "Edit Prometheus target"
          p_ [class_ "text-sm text-textWeak mt-1"] "Update how Monoscope scrapes this endpoint. Changes take effect on the next scrape."
        form_ [hxPost_ $ "/p/" <> pid.toText <> "/settings/prometheus/" <> cfg.id.toText <> "/edit", class_ "flex flex-col gap-4", hxTarget_ "#prometheus-targets", hxSwap_ "outerHTML"]
          $ prometheusFields_ pid ("prom-edit-" <> cfg.id.toText) "Save changes" (Just cfg)
    button_ [class_ "btn btn-xs btn-ghost", hxPatch_ $ "/p/" <> pid.toText <> "/settings/prometheus/" <> cfg.id.toText, hxTarget_ "#prometheus-targets", hxSwap_ "outerHTML"] $ toHtml (bool "Resume" "Pause" cfg.enabled :: Text)
    button_ [class_ "btn btn-xs btn-ghost text-textError", hxDelete_ $ "/p/" <> pid.toText <> "/settings/prometheus/" <> cfg.id.toText, hxTarget_ "#prometheus-targets", hxSwap_ "outerHTML", hxConfirm_ "Remove this Prometheus target?"] "Delete"


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
  deriving anyclass (HI.DecodeRow)


newtype NotificationTestHistoryGet = NotificationTestHistoryGet {tests :: [TestHistory]}
  deriving stock (Eq, Generic, Show)


instance ToHtml NotificationTestHistoryGet where
  toHtml (NotificationTestHistoryGet tests) = toHtmlRaw $ historyHtml_ tests
  toHtmlRaw = toHtml


notificationsTestPostH :: Projects.ProjectId -> TestForm -> ATAuthCtx (RespHeaders (Html ()))
notificationsTestPostH pid TestForm{..} = do
  -- Rate limit: check if test was sent in last 60 seconds
  now <- Time.currentTime
  recentCount <- fromMaybe (0 :: Int) <$> Hasql.interpOne [HI.sql|SELECT COUNT(*)::BIGINT FROM apis.notification_test_history WHERE project_id = #{pid} AND created_at > #{now}::timestamptz - interval '60 seconds'|]
  when (recentCount > 0)
    $ throwError err400{errBody = "Rate limit: Please wait 60 seconds between test notifications"}

  (_, project) <- Projects.sessionAndProject pid
  let baseAlert = bool (sampleAlertByIssueTypeText issueType project.title) (sampleReport project.title) (issueType == "report")
  -- Build a real signed widget URL so the test exercises the same image path
  -- as production alerts (webhook transport lifts it into attachments.image_url).
  -- The chart may be empty for the sample hash; what matters is that signing,
  -- URL length, and the webhook flatten/lift logic are all on the critical path.
  appCtx <- ask @AuthContext
  alert <- case baseAlert of
    RuntimeErrorAlert{errorData} -> do
      let errHash = (errorData :: ErrorPatterns.ATError).hash
      chartUrlM <- errorTrendChartUrl appCtx pid errHash (formatUTC (addUTCTime (-3600) now)) (formatUTC now)
      pure baseAlert{chartUrl = chartUrlM}
    _ -> pure baseAlert

  Log.logTrace "Sending test notification" (channel, pid, issueType)

  let projectUrl = "/p/" <> pid.toText
      fullProjectUrl = appCtx.env.hostUrl <> "p/" <> pid.toText
      testTemplate
        | Just mk <- lookup issueType [("runtime_exception", ET.runtimeErrorsEmail), ("escalating_errors", ET.escalatingErrorsEmail), ("regressed_errors", ET.regressedErrorsEmail), ("error_spike", ET.errorSpikesEmail)] =
            mk project.title (fullProjectUrl <> "/issues/") [] Nothing Nothing Nothing
        | issueType == "report" = ET.sampleWeeklyReport "" ""
        | otherwise = ET.anomalyEndpointEmail "Test User" project.title (fullProjectUrl <> "/issues") [ET.EndpointAlertRow "GET /api/v1/test" (Just "api.example.com") (Just "api-service") (Just "production")]
      sendTestEmail email = let (subj, html) = testTemplate; subj' = "[Test] " <> subj in sendRenderedEmail email subj' (ET.renderEmail subj' html)

  -- The @everyone team is the single source of truth for project notification settings,
  -- so tests always resolve against a team — the provided one, or @everyone when omitted.
  let targetTeam = maybe (ProjectMembers.getEveryoneTeam pid) (\tid -> listToMaybe <$> getTeamsById pid (V.singleton tid)) teamId
      countingSend mChKey run =
        targetTeam >>= \case
          Nothing -> pure (0 :: Int, Just "no_team")
          Just t | Just chKey <- mChKey, not (ProjectMembers.isChannelEnabled chKey t) -> pure (0, Just "channel_disabled")
          Just t -> (,Nothing) <$> run t
      -- per-channel attempt counts, keyed by the form's `channel` value
      senders :: [(Text, Maybe ProjectMembers.NotificationChannel, Team -> ATAuthCtx Int)]
      senders =
        [ ("email", Nothing, \t -> let emails = map CI.original (resolveTeamEmails t) in forM_ emails sendTestEmail $> length emails)
        , ("slack", Just ProjectMembers.Slack, \t -> forM_ t.slack_channels (sendSlackAlert alert pid project.title . Just) $> V.length t.slack_channels)
        , ("discord", Just ProjectMembers.Discord, \t -> forM_ t.discord_channels (sendDiscordAlert alert pid project.title . Just) $> V.length t.discord_channels)
        , ("whatsapp", Just ProjectMembers.Phone, \t -> if V.null t.phone_numbers then pure 0 else sendWhatsAppAlert alert pid project.title t.phone_numbers $> V.length t.phone_numbers)
        , ("pagerduty", Just ProjectMembers.Pagerduty, \t -> forM_ t.pagerduty_services (\k -> sendPagerdutyAlertToService k alert project.title projectUrl) $> V.length t.pagerduty_services)
        ]

  (attempts, skipReason) <- case channel of
    "all" -> countingSend Nothing \t -> sum <$> traverse (\(_, _, run) -> run t) senders
    c -> case find (\(n, _, _) -> n == c) senders of
      Just (_, chKey, run) -> countingSend chKey run
      Nothing -> throwError err400{errBody = "Unknown notification channel"}

  let (status, err) = case (attempts, skipReason) of
        (_, Just r) -> ("skipped" :: Text, Just r)
        (0, _) -> ("skipped", Just "no_targets")
        _ -> ("sent", Nothing)
  void
    $ Hasql.interpExecute
      [HI.sql|INSERT INTO apis.notification_test_history (project_id, issue_type, channel, target, status, error) VALUES (#{pid}, #{issueType}, #{channel}, #{("" :: Text)}, #{status}, #{err})|]

  Log.logTrace "Test notification complete" (channel, pid, status, attempts)
  if status == "sent"
    then addSuccessToast (if channel == "all" then "Test notification sent to all channels!" else "Test " <> channel <> " notification sent!") Nothing
    else addErrorToast ("Test skipped: " <> fromMaybe "unknown" err) Nothing
  addRespHeaders mempty


notificationsTestHistoryGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders NotificationTestHistoryGet)
notificationsTestHistoryGetH pid = do
  tests <- Hasql.interp [HI.sql|SELECT * FROM apis.notification_test_history WHERE project_id = #{pid} ORDER BY created_at DESC LIMIT 20|]
  addRespHeaders $ NotificationTestHistoryGet tests


historyHtml_ :: [TestHistory] -> Html ()
historyHtml_ [] = div_ [class_ "text-center py-12"] do
  div_ [class_ "text-textWeak mb-2"] "No test notifications sent yet"
  p_ [class_ "text-sm text-textWeaker"] "Test your integrations to see results here"
historyHtml_ tests = div_ [class_ "bg-bgRaised rounded-lg border border-strokeWeak overflow-hidden"] $ table_ [class_ "table table-sm w-full"] (thead_ [class_ "text-xs text-left text-textStrong font-semibold uppercase bg-fillWeaker border-b border-strokeWeak"] (tr_ (th_ [class_ "p-3"] "Status" <> th_ [class_ "p-3"] "Channel" <> th_ [class_ "p-3"] "Alert Type" <> th_ [class_ "p-3 text-right"] "Time")) <> tbody_ [class_ "text-sm divide-y divide-strokeWeak"] (foldMap' renderRow tests))
  where
    renderRow t = tr_ [class_ "hover-only:hover:bg-fillWeaker transition-colors"] (td_ [class_ "p-3"] (if t.status == "sent" then span_ [class_ "badge badge-success badge-sm gap-1"] (faSprite_ "check" "solid" "h-3 w-3" >> "Sent") else span_ [class_ "badge badge-error badge-sm gap-1"] (faSprite_ "xmark" "solid" "h-3 w-3" >> "Failed")) <> td_ [class_ "p-3 capitalize font-medium"] (toHtml $ if t.channel == "all" then "All channels" else t.channel) <> td_ [class_ "p-3 text-textWeak"] (toHtml $ T.replace "_" " " t.issueType) <> td_ [class_ "p-3 text-right tabular-nums text-textWeak"] (localTimeFmt_ "MMM dd, HH:mm" t.createdAt))


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
      sigValid = maybe False (\h -> verifyLemonSqueezySignature h rawBody secret) sigHeaderM
  unless sigValid $ Log.logAttention "ls_webhook_sig_mismatch" (T.take 8 <$> sigHeaderM, BS.length rawBody)
  when (envConfig.lsWebhookSigEnforce && not sigValid)
    $ throwError err400{errBody = bool "invalid signature" "missing signature" (isNothing sigHeaderM)}
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
        Projects.projectByOrderId (show orderId) >>= \case
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
        Projects.projectBySubId (show subItem.subscriptionId) >>= \case
          Just project -> notifyMembers project.id $ ET.planUpgradedEmail project.title plan (billingUrl project.id)
          Nothing -> Log.logAttention "LS upgrade: no project for sub_id" (show orderId :: Text, subItem.subscriptionId, plan)
        pure "upgraded"
  case dat.meta.eventName of
    "subscription_created" -> do
      currentTime <- liftIO getZonedTime
      subId <- Projects.LemonSubId <$> liftIO UUIDV4.nextRandom
      let projectId = fromMaybe "" (dat.meta.customData >>= (.projectId))
          sub =
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
  , totalBytes :: Int64
  , lastReported :: Text
  , lemonUrl :: Text
  , critical :: Text
  , paymentPlan :: Text
  , enableFreetier :: Bool
  , basicAuthEnabled :: Bool
  , provider :: Projects.BillingProvider
  , dailyUsage :: [(Day, Int64, Int64, Int64, Int64)]
  -- ^ (day, total_requests, metrics, eventBytes, metricBytes); events = total_requests - metrics
  , cycleStart :: Day
  , pastCycles :: [(Day, Day, Int64, Int64)]
  -- ^ (cycleStart, cycleEndExclusive, totalRequests, totalBytes) for prior cycles, newest first
  }


newtype BillingGet = BillingGet (PageCtx BillingData)


instance ToHtml BillingGet where
  toHtml (BillingGet (PageCtx bwconf d)) = toHtml $ PageCtx bwconf $ billingPage d
  toHtmlRaw = toHtml


manageBillingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders BillingGet)
manageBillingGetH pid from = do
  (_, project, bw) <- mkPageCtx pid
  let dat = fromMaybe project.createdAt project.billingDay
      envCfg = bw.config
  currentTime <- Time.currentTime
  let cycleStart = calculateCycleStartDate dat currentTime
      thirtyDaysAgo = addUTCTime (negate $ 30 * 86400) currentTime
      -- Walk cycle anchors backwards: each previous boundary is calculated by
      -- evaluating the cycle that ended 1s before the current one started.
      prevCycleStart cs = calculateCycleStartDate dat (addUTCTime (-1) cs)
      pastCycleCount = 6 :: Int
      pastBoundaries = take (pastCycleCount + 1) $ iterate prevCycleStart cycleStart
      historyStart = fromMaybe cycleStart (viaNonEmpty last pastBoundaries)
      breakdownStart = min cycleStart thirtyDaysAgo
      fetchStart = min breakdownStart historyStart
  (totalRequests, totalBytes) <- Projects.getTotalUsage pid cycleStart
  allDaily <- Projects.getDailyUsageBreakdown pid fetchStart
  let breakdownDay = utctDay breakdownStart
      dailyUsage = filter (\(day, _, _, _, _) -> day >= breakdownDay) allDaily
      -- Bucket prior cycles. pastBoundaries is newest-first; pair each cycle's
      -- start with the next-newer boundary as its exclusive end.
      pastCycles =
        [ (utctDay cStart, utctDay cEnd, sum [n | (_, n, _, _, _) <- inCycle], sum [eb + mb | (_, _, _, eb, mb) <- inCycle])
        | (cEnd, cStart) <- zip pastBoundaries (drop 1 pastBoundaries)
        , let s = utctDay cStart
              e = utctDay cEnd
              inCycle = filter (\(d, _, _, _, _) -> d >= s && d < e) allDaily
        , not (null inCycle)
        ]
  let last_reported = toText (formatTime defaultTimeLocale "%b %-d" project.usageLastReported)
      bwconf = bw{pageTitle = "Billing", isSettingsPage = True}
      lemonUrl = envCfg.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
      critical = envCfg.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
      -- Free-tier display shows no provider even for a historically-paid-then-downgraded project
      -- (which keeps its stored provider so trial-reminder/auto-migration logic still works).
      provider = if Projects.isFreeTier project.paymentPlan then Projects.NoBillingProvider else Projects.projectProvider project
  addRespHeaders $ BillingGet $ PageCtx bwconf BillingData{pid, totalReqs = totalRequests, totalBytes, lastReported = last_reported, lemonUrl, critical, paymentPlan = project.paymentPlan, enableFreetier = envCfg.enableFreetier, basicAuthEnabled = envCfg.basicAuthEnabled, provider, dailyUsage, cycleStart = utctDay cycleStart, pastCycles}


billingPage :: BillingData -> Html ()
billingPage d = div_ [] do
  let reqs = d.totalReqs
      pidTxt = d.pid.toText
      isFree = Projects.isFreeTier d.paymentPlan
      basePriceNum =
        if
          | isFree -> 0
          | d.paymentPlan == "Bring your own storage" -> 199
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
    headerRow_ [] do
      div_ [class_ "flex items-center gap-3"] do
        span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml d.paymentPlan
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
        let bytesSuffix = if d.totalBytes > 0 then " · " <> humanBytes d.totalBytes else ""
            usageLine =
              if isFree || overageNum <= 0
                then fmt (commaizeF reqs) <> " requests" <> bytesSuffix
                else "$" <> planPrice <> " plan + " <> fmtUSD overageCost <> " usage (" <> fmt (commaizeF reqs) <> " requests" <> bytesSuffix <> ")"
        div_ [class_ "text-xs text-textWeak mt-1 tabular-nums"] $ toHtml usageLine
      unless (T.null d.lastReported)
        $ div_ [class_ "text-xs text-textWeak"]
        $ toHtml ("Last reported " <> d.lastReported)

    -- Actions (kept above the breakdown so they remain near the headline numbers)
    div_ [class_ "border-t border-strokeWeak pt-6 flex items-center gap-3"] do
      label_ [Lucid.for_ "pricing-modal", class_ "btn btn-sm btn-primary cursor-pointer"] "Change plan"
      unless isFree
        $ a_ [class_ "btn btn-sm btn-ghost text-textBrand", hxGet_ $ "/p/" <> pidTxt <> "/manage_subscription"] "Manage subscription"

    -- Daily breakdown
    dailyUsageBreakdown_ isFree d.cycleStart d.dailyUsage

    -- Past cycles
    pastCyclesSection_ isFree basePriceNum d.pastCycles

  modalWith_ "pricing-modal" def{boxClass = "w-[1250px] max-w-[1300px] py-16 px-32", wrapperClass = "p-8"} Nothing do
    div_ [class_ "text-center text-sm text-textWeak w-full mx-auto max-w-96"] do
      span_ [class_ "text-textStrong text-2xl font-semibold"] "Compare Plans"
      p_ [class_ "mt-2 mb-4"] "Drag the slider to estimate costs at different usage levels."
    paymentPlanPicker d.pid d.lemonUrl d.critical d.paymentPlan d.enableFreetier d.basicAuthEnabled False d.provider


-- | Format a byte count with the largest unit it fits into. KB-step decimal
-- (matches what most cloud billing pages render), one decimal past KB.
humanBytes :: Int64 -> Text
humanBytes b
  | b < 1024 = show b <> " B"
  | b < 1_048_576 = fmtNum (fromIntegral b / 1024 :: Double) <> " KB"
  | b < 1_073_741_824 = fmtNum (fromIntegral b / 1_048_576 :: Double) <> " MB"
  | otherwise = fmtNum (fromIntegral b / 1_073_741_824 :: Double) <> " GB"
  where
    fmtNum n = toText (printf "%.1f" n :: String)


-- | Sticky table header cell: opaque background so scrolled rows can't show through.
stickyTh_ :: Text -> Html () -> Html ()
stickyTh_ cls = th_ [class_ ("font-medium px-3 py-2 sticky top-0 z-10 bg-fillWeak border-b border-strokeWeak " <> cls)]


-- | Per-day usage table for the last 30 days. Cost shown is the marginal
-- contribution past the 20M-included tier ($1 per 1M), assuming chronological
-- accumulation across the cycle. Days entirely below the threshold show "—".
dailyUsageBreakdown_ :: Bool -> Day -> [(Day, Int64, Int64, Int64, Int64)] -> Html ()
dailyUsageBreakdown_ isFree cycleStartDay rows = div_ [class_ "border-t border-strokeWeak pt-6 space-y-3"] do
  -- Header total is scoped to current cycle so it matches the headline
  -- "Estimated this cycle" figure. Pre-cycle rows are still rendered (dimmed)
  -- below for context, but excluded from the totals.
  let cycleRows = filter (\(d, _, _, _, _) -> d >= cycleStartDay) rows
      totalReqs = sum [n | (_, n, _, _, _) <- cycleRows]
      totalBytes = sum [eb + mb | (_, _, _, eb, mb) <- cycleRows]
      summaryRight =
        if totalBytes > 0
          then fmt (commaizeF totalReqs) <> " rows · " <> humanBytes totalBytes
          else fmt (commaizeF totalReqs) <> " rows"
  div_ [class_ "flex items-baseline justify-between"] do
    sectionLabel_ "Daily breakdown"
    span_ [class_ "text-xs text-textWeak tabular-nums"] $ toHtml @Text summaryRight
  if null rows
    then div_ [class_ "text-sm text-textWeak py-4"] "No usage recorded yet this cycle."
    else do
      let activeDays = length rows
          included = 20_000_000 :: Int64
          maxDay = foldr (\(_, n, _, _, _) acc -> max n acc) 1 rows
          hasMetrics = any (\(_, _, m, _, _) -> m > 0) rows
          ascending = sortWith (\(d, _, _, _, _) -> d) rows
          -- Running cumulative resets at cycleStartDay so pre-cycle rows (shown
          -- for context) don't inflate the included-tier counter and produce
          -- incorrect "Est. cost" for current-cycle days.
          withRunning =
            fst
              $ foldl'
                ( \(xs, acc) (day, n, m, eb, mb) ->
                    let acc' = (if day < cycleStartDay then 0 else acc) + n
                     in ((day, n, m, eb, mb, acc' - n, acc') : xs, acc')
                )
                ([], 0 :: Int64)
                ascending
          dayCostText prev cur
            | isFree = "—"
            | dayOverage <= 0 = "—"
            | otherwise = "$" <> toText (printf "%.2f" (fromIntegral dayOverage / 1_000_000 :: Double) :: String)
            where
              dayOverage = max 0 (cur - included) - max 0 (prev - included)
      unless hasMetrics
        $ div_ [class_ "flex items-center gap-2 text-xs text-textWeak bg-fillWeak/40 border border-strokeWeak rounded-md px-3 py-2"] do
          span_ [class_ "text-textStrong"] "No metric ingestion this cycle."
          span_ [] "Only logs and traces have been ingested."
      div_ [class_ "border border-strokeWeak rounded-md overflow-hidden max-h-96 overflow-y-auto"] do
        table_ [class_ "w-full text-sm tabular-nums border-separate border-spacing-0"] do
          -- `border-separate` keeps the header's border-bottom from being clipped
          -- by sticky positioning.
          thead_ [class_ "text-textWeak text-xs uppercase tracking-wide"] do
            tr_ do
              stickyTh_ "text-left" "Date"
              stickyTh_ "text-right" "Events"
              stickyTh_ "text-right" "Metrics"
              stickyTh_ "text-left w-1/4" ""
              stickyTh_ "text-right" "Est. cost"
          let countCell :: Int64 -> Int64 -> Bool -> Html ()
              countCell bytes n0 strong = td_ [class_ "px-3 py-2 text-right whitespace-nowrap"] do
                div_ [class_ (if strong then "text-textStrong" else "text-textWeak")]
                  $ toHtml @Text
                  $ if n0 <= 0 then "—" else fmt (commaizeF n0)
                when (bytes > 0)
                  $ div_ [class_ "text-2xs text-textWeak/80 leading-tight"]
                  $ toHtml @Text (humanBytes bytes)
          tbody_ do
            forM_ withRunning \(day, n, metrics, eb, mb, prev, cur) -> do
              let pct = max 1 $ min 100 $ (n * 100) `div` maxDay
                  events = max 0 (n - metrics)
                  preCycle = day < cycleStartDay
                  rowCls = "border-t border-strokeWeak align-top" <> (if preCycle then " opacity-50" else "")
                  dayCls = "px-3 py-2 " <> (if preCycle then "text-textWeak" else "text-textStrong")
              tr_ [class_ rowCls, title_ (if preCycle then "Previous cycle — shown for context" else "")] do
                td_ [class_ dayCls] $ toHtml $ toText (formatTime defaultTimeLocale "%a %b %e" day)
                countCell eb events True
                countCell mb metrics False
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


-- | Past billing cycles, newest first. Estimated cost is computed from the
-- current plan's pricing (we don't snapshot the plan/price at cycle close), so
-- it's an approximation when the plan has changed.
pastCyclesSection_ :: Bool -> Int64 -> [(Day, Day, Int64, Int64)] -> Html ()
pastCyclesSection_ _ _ [] = mempty
pastCyclesSection_ isFree basePrice cycles = div_ [class_ "border-t border-strokeWeak pt-6 space-y-3"] do
  div_ [class_ "flex items-baseline justify-between"] do
    sectionLabel_ "Past cycles"
    span_ [class_ "text-xs text-textWeak"] $ toHtml @Text (show (length cycles) <> " cycle" <> (if length cycles == 1 then "" else "s"))
  div_ [class_ "border border-strokeWeak rounded-md overflow-hidden"] do
    table_ [class_ "w-full text-sm tabular-nums border-separate border-spacing-0"] do
      thead_ [class_ "text-textWeak text-xs uppercase tracking-wide"] do
        tr_ do
          stickyTh_ "text-left" "Cycle"
          stickyTh_ "text-right" "Requests"
          stickyTh_ "text-right" "Volume"
          stickyTh_ "text-right" "Est. cost"
      tbody_ do
        forM_ cycles \(cs, ce, reqs, bytes) -> do
          -- ce is exclusive end; subtract 1 day for the human-facing label.
          let endLabel = toText (formatTime defaultTimeLocale "%b %-d" (addDays (-1) ce))
              startLabel = toText (formatTime defaultTimeLocale "%b %-d, %Y" cs)
              overage = max 0 (reqs - 20_000_000)
              cost = fromIntegral basePrice + (fromIntegral overage / 1_000_000 :: Double)
              costText
                | isFree = "—"
                | otherwise = "$" <> toText (printf "%.2f" cost :: String)
          tr_ [class_ "border-t border-strokeWeak"] do
            td_ [class_ "px-3 py-2 text-textStrong"] $ toHtml @Text (startLabel <> " – " <> endLabel)
            td_ [class_ "px-3 py-2 text-right text-textStrong"] $ toHtml @Text (fmt (commaizeF reqs))
            td_ [class_ "px-3 py-2 text-right text-textWeak"] $ toHtml @Text (if bytes > 0 then humanBytes bytes else "—")
            td_ [class_ "px-3 py-2 text-right text-textWeak"] $ toHtml costText
  unless isFree
    $ div_ [class_ "text-xs text-textWeak"] "Estimated cost uses the current plan's pricing; actual invoiced amounts may differ if your plan changed."


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
stripeOpts apiKey =
  Wreq.defaults
    & (Wreq.header "Authorization" .~ ["Bearer " <> encodeUtf8 apiKey])
    -- Managed Payments requires API version 2025-03-31.basil or later.
    & (Wreq.header "Stripe-Version" .~ ["2025-03-31.basil"])


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
      -- Stripe Managed Payments (MoR): Stripe becomes merchant of record and handles
      -- tax/address collection itself, so the automatic_tax + tax_id params are disallowed.
      managedPaymentsParams = [("managed_payments[enabled]", "true")]
      params =
        prices
          <> trialParams
          <> managedPaymentsParams
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
          (Just "customer.subscription.deleted", Just obj) -> handleStripeDowngrade "subscription.deleted" "was cancelled" obj notifyMembers billingUrl
          (Just "customer.subscription.updated", Just obj) -> handleStripeSubUpdated obj
          (Just "customer.subscription.paused", Just obj) -> handleStripeDowngrade "subscription.paused" "was paused" obj notifyMembers billingUrl
          (Just "customer.subscription.resumed", Just obj) -> handleStripeSubResumed envConfig obj notifyMembers billingUrl
          (Just "invoice.payment_failed", Just obj) -> handleStripePaymentFailed obj notifyMembers billingUrl
          _ -> do
            Log.logInfo "Stripe webhook unhandled event" (eventType, isJust sessionObj)
            pure ""


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
            case Projects.projectProvider project of
              Projects.LemonSqueezyProvider ->
                whenJust project.subId
                  $ liftIO
                  . cancelLemonSqueezySubscription envConfig.lemonSqueezyApiKey
              _ -> pass
          subDetails <- liftIO $ BJ.getStripeSubDetails envConfig.stripeSecretKey subId
          when (isNothing subDetails) $ Log.logAttention "Stripe sub fetch failed after checkout" (pid.toText, subId)
          let subItemId = maybe "" (.subItemId) subDetails
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


-- | Shared body of subscription.deleted / .paused: downgrade by sub_id, then notify.
-- @event@ names the Stripe event in logs, @reason@ is the user-facing wording.
handleStripeDowngrade :: Text -> Text -> AE.Value -> (Projects.ProjectId -> (Text, Html ()) -> ATBaseCtx ()) -> (Projects.ProjectId -> Text) -> ATBaseCtx (Html ())
handleStripeDowngrade event reason obj notifyMembers billingUrl =
  case jsonField "id" obj :: Maybe Text of
    Just subId -> do
      rows <- Projects.downgradeToFreeBySubId subId
      when (rows == 0) $ Log.logAttention ("Stripe " <> event <> ": no project for sub_id") subId
      when (rows > 1) $ Log.logAttention ("Stripe " <> event <> " touched multiple rows") (subId, rows)
      whenJustM (Projects.projectBySubId subId) \project ->
        notifyMembers project.id $ ET.planDowngradedEmail project.title reason (billingUrl project.id)
      pure $ toHtml $ T.replace "." " " event
    Nothing -> do
      Log.logAttention ("Stripe " <> event <> ": missing sub id") ()
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
  let attemptCount = jsonField "attempt_count" obj :: Maybe Int
      nextAttempt = jsonField "next_payment_attempt" obj :: Maybe Int
  whenJust (jsonField "customer" obj :: Maybe Text) \customerId ->
    whenJustM (Projects.projectByCustomerId customerId) \project -> do
      Log.logAttention "Stripe invoice.payment_failed" (project.id, attemptCount, nextAttempt)
      notifyMembers project.id $ ET.planDowngradedEmail project.title "payment failed" (billingUrl project.id)
  pure "payment failed handled"


-- Look up the sub's price via the Stripe API, reverse-map to our plan name, and re-upgrade.
-- (Pause preserved the IDs, so resume never needs a re-checkout.)
handleStripeSubResumed :: EnvConfig -> AE.Value -> (Projects.ProjectId -> (Text, Html ()) -> ATBaseCtx ()) -> (Projects.ProjectId -> Text) -> ATBaseCtx (Html ())
handleStripeSubResumed envConfig obj notifyMembers billingUrl =
  case jsonField "id" obj :: Maybe Text of
    Just subId -> do
      subDetailsM <- liftIO $ BJ.getStripeSubDetails envConfig.stripeSecretKey subId
      case subDetailsM of
        -- the price id reverse-maps to our internal plan name
        Just BJ.StripeSubDetails{subItemId = itemId, priceId} -> do
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
