module Pages.S3 (bringS3GetH, brings3PostH, getMinioConnectInfo,brings3RemoveH) where

import Data.Default (Default (def))
import Data.Text qualified as T
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxIndicator_, hxPost_, hxSwap_, hxTarget_, hxDelete_)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Network.Minio qualified as Minio
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (ask)
import System.Config (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Utils (faSprite_)
import Lucid.Hyperscript (__)


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
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Bring your own s3", isSettingsPage = True}
  addRespHeaders $ bodyWrapper bwconf $ bringS3Page pid project.s3Bucket


bringS3Page :: Projects.ProjectId -> Maybe Projects.ProjectS3Bucket -> Html ()
bringS3Page pid s3BucketM = div_ [class_ "space-y-6 mx-auto w-full max-w-5xl px-4 py-10 md:py-14"] $ do
  div_ [class_ "space-y-2"] $ do
    div_ [class_ "flex items-center gap-2"] $ do
      faSprite_ "cloud" "regular" "h-8 w-8"
      h1_ [class_ "text-2xl font-semibold tracking-tight"] "Bring Your Own S3 Bucket"
    p_ [class_ "text-textWeak text-sm max-w-3xl leading-tight"] "Connect your own S3 or S3-compatible bucket, validate access. Your project's data opentelemetry and session replay events will be store in this bucket"

  form_ [class_ "rounded-lg border bg-bgBase w-full", hxPost_ "", hxSwap_ "innerHtml", hxTarget_ "#connectedInd", hxIndicator_ "#indicator"] $ do
    div_ [class_ "flex flex-wrap items-center w-full justify-between gap-3 border-b p-5"] $ do
      div_ [class_ "flex items-center w-full justify-between"] $ do
        div_ [class_ "space-y-1"] $ do
          h2_ [class_ "flex items-center gap-2 font-semibold"] $ do
            faSprite_ "shield-check" "regular" "h-5 w-5"
            "Connection"
          p_ [class_ "text-sm text-textWeak"] "Enter credentials to connect to your S3 bucket."
        div_ [id_ "connectedInd"] do
          case s3BucketM of
            Just _ -> connected
            Nothing -> notConnected

    div_ [class_ "p-5"] $ do
      div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] $ do
        connectionField "Access Key ID" "accessKey" True (maybe "" (.accessKey) s3BucketM) False
        connectionField "Secret Access Key" "secretKey" True (maybe "" (.secretKey) s3BucketM) True
        connectionField "Region" "region" True (maybe "" (.region) s3BucketM) False
        connectionField "Bucket" "bucket" True (maybe "" (.bucket) s3BucketM) False
        div_ [class_ "space-y-2 md:col-span-2"] $ do
          connectionField "Custom Endpoint (optional, for S3-compatible providers)" "endpointUrl" False (maybe "" (.endpointUrl) s3BucketM) False
      div_ [class_ "mt-10 flex flex-wrap justify-between items-center gap-3"] $ do
        div_ [class_ "flex gap-2 items-center"] do
          button_ [class_ "btn btn-sm btn-primary"] do
            "Validate Connection"
            span_ [class_ "htmx-indicator query-indicator loading loading-dots loading-sm", id_ "indicator"] ""
          span_ [class_ "text-sm text-textWeak"] "Auto saves if credentials are valid"
        label_ [class_ "btn bg-fillWeak text-textWeak", Lucid.for_ "remove-modal"] "Remove bucket"

    input_ [type_ "checkbox", id_ "remove-modal", class_ "modal-toggle"]
    div_ [class_ "modal ", role_ "dialog", id_ "remove-modal"] do
      div_ [class_ "modal-box flex flex-col gap-2 p-8"] $ do
        div_ [class_ "flex w-full mb-2 justify-between items-start"] do
          div_ [class_ "p-3 bg-fillError-weak rounded-full w-max border-[#067a57]/20 gap-2 inline-flex"]
            $ faSprite_ "circle-info" "regular" "h-6 w-6 text-textError"
          button_
            [ class_ "btn btn-ghost btn-sm btn-circle"
            , [__|on click set #remove-modal.checked to false |]
            ]
            do
              faSprite_ "circle-xmark" "regular" "h-6 w-6 text-textWeak"
        span_ [class_ "text-textStrong text-2xl font-semibold"] "Remove bucket?"
        span_ [class_ "text-textWeak text-sm font-semibold"] "Removing bucket will result in the loss of all data associated with it on your dashboard"
        button_ [class_ "btn mt-4 bg-fillError-strong text-white", hxDelete_ $ "", hxSwap_ "innerHtml", hxTarget_ "#connectedInd"] "Remove"
      label_ [class_ "modal-backdrop", Lucid.for_ "remove-modal"] "Close"


-- div_ [class_ "rounded-lg border bg-bgBase w-full"] $ do
--   div_ [class_ "flex flex-wrap items-center w-full justify-between gap-3 border-b p-5"] $ do
--     div_ [class_ "flex items-center w-full justify-between"] $ do
--       div_ [class_ "space-y-1"] $ do
--         h2_ [class_ "flex items-center font-semibold"] $ do
--           "Upload a Test File"
--         p_ [class_ "text-sm text-textWeak"] "Uploads server-side with your provided credentials."
--       span_ [class_ "inline-flex items-center gap-2 rounded-full px-3 py-1 text-xs font-medium bg-fillWeaker"] "Connected"
--   div_ [class_ "p-5"] $ do
--     div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] $ do
--       div_ [class_ "space-y-2"] $ do
--         label_ [class_ "flex items-center gap-2 text-sm font-medium"] "File"
--         input_ [class_ "input rounded-lg w-full border border-strokeStrong", type_ "file"]
--     div_ [class_ "mt-4 flex flex-wrap items-center gap-3"] $ do
--       button_ [class_ "btn btn-sm btn-primary"] "Upload"
--       span_ [class_ "text-sm text-textWeak"] "Make sure to enter credentials first"

connectionField :: Text -> Text -> Bool -> Text -> Bool -> Html ()
connectionField lbl name required defVal isPass =
  div_ [class_ "space-y-2"] $ do
    label_ [class_ "flex items-center gap-2 text-sm font-medium"] do
      toHtml lbl
      when required $ span_ [class_ "text-textError"] "*"
    input_ ([class_ "input rounded-lg w-full border border-strokeStrong", value_ defVal, name_ name, type_ $ if isPass then "password" else "text", placeholder_ lbl] <> [required_ "true" | required])


connected :: Html ()
connected = span_ [class_ "inline-flex items-center gap-2 rounded-full px-3 py-1 text-xs text-textSuccess bg-green-50 font-medium bg-fillWeaker"] do
  faSprite_ "circle-check" "regular" "w-3 h-3"
  "Connected"


notConnected :: Html ()
notConnected = span_ [class_ "inline-flex items-center gap-2 rounded-full px-3 py-1 text-xs font-medium bg-fillWeaker"] do
  faSprite_ "circle-info" "regular" "w-3 h-3"
  "Not Connected"
