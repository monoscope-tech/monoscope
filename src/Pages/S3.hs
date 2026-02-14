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
import Pages.Components (FieldCfg (..), FieldSize (..), confirmModal_, connectionBadge_, formField_)
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
            div_ [class_ "p-2 rounded-full bg-fillBrand-weak"] $ faSprite_ "bucket" "regular" "h-4 w-4 text-textBrand"
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
            span_ [class_ "htmx-indicator loading loading-dots loading-xs", id_ "indicator"] ""
          span_ [class_ "text-xs text-textWeak"] "Auto-saves on success"
        when (isJust s3BucketM) $ label_ [class_ "btn btn-sm btn-ghost text-textError hover:bg-fillError-weak", Lucid.for_ "remove-modal"] do
          faSprite_ "trash" "regular" "w-3 h-3"
          span_ "Remove"

    confirmModal_ "remove-modal" "Remove bucket?" "This will disconnect your S3 bucket. Data already stored will remain in your bucket." [hxDelete_ "", hxSwap_ "innerHTML", hxTarget_ "#connectedInd"] "Remove bucket"
