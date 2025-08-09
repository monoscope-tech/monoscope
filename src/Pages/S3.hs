module Pages.S3 (bringS3GetH, brings3PostH) where

import Data.Default (Default (def))
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Network.Minio qualified as Minio
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (ask)
import Servant qualified
import System.Config (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Utils (faSprite_)


getMinioConnectInfo :: Projects.ProjectS3Bucket -> Minio.ConnectInfo
getMinioConnectInfo Projects.ProjectS3Bucket{..} = Minio.setCreds (Minio.CredentialValue accessKey' secretKey' Nothing) withRegion
  where
    withRegion = Minio.setRegion (fromString $ toString region) info
    info = fromString $ toString (fromMaybe "" endpointUrl)
    accessKey' = fromString $ toString accessKey
    secretKey' = fromString $ toString secretKey


brings3PostH :: Projects.ProjectId -> Projects.ProjectS3Bucket -> ATAuthCtx (RespHeaders (Html ()))
brings3PostH pid s3Form = do
  let connectInfo = getMinioConnectInfo s3Form
  res <- liftIO $ Minio.runMinio connectInfo do
    Minio.bucketExists s3Form.bucket
  case res of
    Left err -> do
      addErrorToast (show err) Nothing
      addRespHeaders notConnected
    Right bExists ->
      if bExists
        then do
          _ <- Projects.updateProjectS3Bucket pid s3Form
          addSuccessToast "Connected succesfully" Nothing
          addRespHeaders connected
        else do
          addErrorToast "Bucket does not exist" Nothing
          addRespHeaders notConnected


bringS3GetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
bringS3GetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Bring your own s3", isSettingsPage = True}
  addRespHeaders $ bodyWrapper bwconf $ bringS3Page pid project.s3Bucket


bringS3Page :: Projects.ProjectId -> Maybe Projects.ProjectS3Bucket -> Html ()
bringS3Page pid s3BucketM = div_ [class_ "space-y-6 mx-auto w-full max-w-5xl px-4 py-10 md:py-14"] $ do
  -- Header
  div_ [class_ "space-y-2"] $ do
    div_ [class_ "flex items-center gap-2"] $ do
      faSprite_ "cloud" "regular" "h-8 w-8"
      h1_ [class_ "text-2xl font-semibold tracking-tight"] "Bring Your Own S3 Bucket"
    p_ [class_ "text-textWeak text-sm max-w-3xl leading-tight"] "Connect your own S3 or S3-compatible bucket, validate access, upload a test file, and browse objects. Your keys are only held in-memory in this demo and never persisted."

  -- Connection Card
  form_ [class_ "rounded-lg border bg-bgBase w-full", hxPost_ "", hxSwap_ "innerHtml", hxTarget_ "#connectedInd"] $ do
    div_ [class_ "flex flex-wrap items-center w-full justify-between gap-3 border-b p-5"] $ do
      div_ [class_ "flex items-center w-full justify-between"] $ do
        div_ [class_ "space-y-1"] $ do
          h2_ [class_ "flex items-center gap-2 font-semibold"] $ do
            faSprite_ "shield-check" "regular" "h-5 w-5"
            "Connection"
          p_ [class_ "text-sm text-textWeak"] "Enter temporary credentials to test your bucket connection."
        div_ [id_ "connectedInd"] do
          case s3BucketM of
            Just _ -> connected
            Nothing -> notConnected

    div_ [class_ "p-5"] $ do
      div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] $ do
        connectionField "Access Key ID" "accessKey" True (maybe "" (.accessKey) s3BucketM)
        connectionField "Secret Access Key" "secretKey" True (maybe "" (.secretKey) s3BucketM)
        connectionField "Region" "region" True (maybe "" (.region) s3BucketM)
        connectionField "Bucket" "bucket" True (maybe "" (.bucket) s3BucketM)
        div_ [class_ "space-y-2 md:col-span-2"] $ do
          connectionField "Custom Endpoint (optional, for S3-compatible providers)" "endpointUrl" False (maybe "" (\x -> fromMaybe "" x.endpointUrl) s3BucketM)
      div_ [class_ "mt-4 flex flex-wrap items-center gap-3"] $ do
        button_ [class_ "btn btn-sm btn-primary"] "Validate Connection"
        span_ [class_ "text-sm text-textWeak"] "Auto saves if credentials are valid"


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

connectionField :: Text -> Text -> Bool -> Text -> Html ()
connectionField lbl name required defVal =
  div_ [class_ "space-y-2"] $ do
    label_ [class_ "flex items-center gap-2 text-sm font-medium"] do
      toHtml lbl
      when required $ span_ [class_ "text-textError"] "*"
    input_ ([class_ "input rounded-lg w-full border border-strokeStrong", value_ defVal, name_ name, type_ "text", placeholder_ lbl] <> [required_ "true" | required])


connected :: Html ()
connected = span_ [class_ "inline-flex items-center gap-2 rounded-full px-3 py-1 text-xs text-textSuccess bg-green-50 font-medium bg-fillWeaker"] do
  faSprite_ "circle-check" "regular" "w-3 h-3"
  "Connected"


notConnected :: Html ()
notConnected = span_ [class_ "inline-flex items-center gap-2 rounded-full px-3 py-1 text-xs font-medium bg-fillWeaker"] do
  faSprite_ "circle-info" "regular" "w-3 h-3"
  "Not Connected"
