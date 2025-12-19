module Pages.S3 (bringS3GetH, brings3PostH, getMinioConnectInfo, brings3RemoveH) where

import Data.Default (Default (def))
import Data.Text qualified as T
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxDelete_, hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Network.Minio qualified as Minio
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (ask)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Utils (faSprite_)


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
      addRespHeaders notConnected
    Right bExists ->
      if bExists
        then do
          _ <- Projects.updateProjectS3Bucket pid $ Just s3Form
          addSuccessToast "Connected succesfully" Nothing
          addRespHeaders connected
        else do
          addErrorToast "Bucket does not exist" Nothing
          addRespHeaders notConnected


brings3RemoveH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
brings3RemoveH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  _ <- Projects.updateProjectS3Bucket pid Nothing

  addSuccessToast "Removed S3 bucket" Nothing
  addRespHeaders notConnected


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
            div_ [class_ "p-2 rounded-full bg-fillBrand-weak"] $ faSprite_ "bucket" "regular" "h-4 w-4 text-textBrand"
            div_ do
              h3_ [class_ "text-sm font-medium text-textStrong"] "Connection Status"
              p_ [class_ "text-xs text-textWeak"] "Your bucket connection state"
          div_ [id_ "connectedInd"] $ maybe notConnected (const connected) s3BucketM

      -- Credentials card
      div_ [class_ "surface-raised rounded-2xl p-4 space-y-4"] do
        label_ [class_ "text-sm font-medium text-textStrong block"] "Bucket Credentials"
        div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] do
          connectionField "Access Key ID" "accessKey" True (maybe "" (.accessKey) s3BucketM) False
          connectionField "Secret Access Key" "secretKey" True (maybe "" (.secretKey) s3BucketM) True
          connectionField "Region" "region" True (maybe "" (.region) s3BucketM) False
          connectionField "Bucket Name" "bucket" True (maybe "" (.bucket) s3BucketM) False
        connectionField "Custom Endpoint" "endpointUrl" False (maybe "" (.endpointUrl) s3BucketM) False
        p_ [class_ "text-xs text-textWeak"] "Optional: For S3-compatible providers like MinIO, DigitalOcean Spaces, etc."

      -- Actions
      div_ [class_ "flex items-center justify-between"] do
        div_ [class_ "flex items-center gap-3"] do
          button_ [class_ "btn btn-sm btn-outline gap-1"] do
            "Validate & Save"
            span_ [class_ "htmx-indicator loading loading-dots loading-xs", id_ "indicator"] ""
          span_ [class_ "text-xs text-textWeak"] "Auto-saves on success"
        when (isJust s3BucketM) $ label_ [class_ "btn btn-sm btn-ghost text-textError hover:bg-fillError-weak", Lucid.for_ "remove-modal"] do
          faSprite_ "trash" "regular" "w-3 h-3"
          span_ "Remove"

    -- Remove modal
    input_ [type_ "checkbox", id_ "remove-modal", class_ "modal-toggle"]
    div_ [class_ "modal", role_ "dialog"] do
      div_ [class_ "modal-box p-6"] do
        div_ [class_ "flex items-start gap-3 mb-4"] do
          div_ [class_ "p-2 bg-fillError-weak rounded-full"] $ faSprite_ "triangle-alert" "regular" "h-5 w-5 text-textError"
          div_ do
            h3_ [class_ "text-lg font-semibold text-textStrong"] "Remove bucket?"
            p_ [class_ "text-sm text-textWeak mt-1"] "This will disconnect your S3 bucket. Data already stored will remain in your bucket."
        div_ [class_ "flex justify-end gap-2 mt-6"] do
          label_ [class_ "btn btn-sm btn-ghost", Lucid.for_ "remove-modal"] "Cancel"
          button_ [class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90", hxDelete_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd"] "Remove bucket"
      label_ [class_ "modal-backdrop", Lucid.for_ "remove-modal"] ""


connectionField :: Text -> Text -> Bool -> Text -> Bool -> Html ()
connectionField lbl name required defVal isPass =
  div_ [class_ "space-y-1.5"] do
    label_ [class_ "flex items-center gap-1 text-xs font-medium text-textWeak"] do
      toHtml lbl
      when required $ span_ [class_ "text-textError"] "*"
    input_ ([class_ "input input-bordered input-sm w-full", value_ defVal, name_ name, type_ $ if isPass then "password" else "text", placeholder_ lbl] <> [required_ "true" | required])


connected :: Html ()
connected = span_ [class_ "inline-flex items-center gap-1.5 rounded-full px-2.5 py-1 text-xs font-medium bg-fillSuccess-weak text-textSuccess"] do
  faSprite_ "circle-check" "regular" "w-3 h-3"
  "Connected"


notConnected :: Html ()
notConnected = span_ [class_ "inline-flex items-center gap-1.5 rounded-full px-2.5 py-1 text-xs font-medium bg-fillWeak text-textWeak"] do
  faSprite_ "circle-info" "regular" "w-3 h-3"
  "Not connected"
