module Pages.S3 (bringS3GetH) where

import Data.Default (Default (def))
import Effectful.Reader.Static (ask)
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (ask)
import System.Config (AuthContext)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)


bringS3GetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
bringS3GetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "Bring your own s3", isSettingsPage = True}
  addRespHeaders $ bodyWrapper bwconf $ bringS3Page pid


bringS3Page :: Projects.ProjectId -> Html ()
bringS3Page pid = div_ [class_ "space-y-6 mx-auto w-full max-w-5xl px-4 py-10 md:py-14"] $ do
  -- Header
  div_ [class_ "space-y-2"] $ do
    div_ [class_ "flex items-center gap-2"] $ do
      span_ [class_ "h-6 w-6 bg-gray-300 inline-block"] "" -- placeholder for Cloud icon
      h1_ [class_ "text-2xl font-semibold tracking-tight"] "Bring Your Own S3 Bucket"
    p_ [class_ "text-textWeak text-sm max-w-3xl leading-tight"] "Connect your own S3 or S3-compatible bucket, validate access, upload a test file, and browse objects. Your keys are only held in-memory in this demo and never persisted."

  -- Connection Card
  div_ [class_ "rounded-lg border bg-bgBase w-full"] $ do
    div_ [class_ "flex flex-wrap items-center w-full justify-between gap-3 border-b p-5"] $ do
      div_ [class_ "flex items-center w-full justify-between"] $ do
        div_ [class_ "space-y-1"] $ do
          h2_ [class_ "flex items-center gap-2 font-semibold"] $ do
            span_ [class_ ""] "" -- ShieldCheck icon placeholder
            "Connection"
          p_ [class_ "text-sm text-textWeak"] "Enter temporary credentials to test your bucket connection."
        span_ [class_ "inline-flex items-center gap-2 rounded-full px-3 py-1 text-xs font-medium bg-fillWeaker"] "Connected"
    div_ [class_ "p-5"] $ do
      div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] $ do
        connectionField "Access Key ID"
        connectionField "Secret Access Key"
        connectionField "Region"
        connectionField "Bucket"
        div_ [class_ "space-y-2 md:col-span-2"] $ do
          connectionField "Custom Endpoint (optional, for S3-compatible providers)"
      div_ [class_ "mt-4 flex flex-wrap items-center gap-3"] $ do
        button_ [class_ "btn btn-sm btn-primary"] "Validate Connection"
        span_ [class_ "text-sm text-textWeak"] "Validation calls HeadBucket on your target."
      div_ [class_ "mt-4 alert border-emerald-300"] $ do
        strong_ "Success"
        p_ "Bucket connection validated."

  div_ [class_ "rounded-lg border bg-bgBase w-full"] $ do
    div_ [class_ "flex flex-wrap items-center w-full justify-between gap-3 border-b p-5"] $ do
      div_ [class_ "flex items-center w-full justify-between"] $ do
        div_ [class_ "space-y-1"] $ do
          h2_ [class_ "flex items-center font-semibold"] $ do
            "Upload a Test File"
          p_ [class_ "text-sm text-textWeak"] "Uploads server-side with your provided credentials."
        span_ [class_ "inline-flex items-center gap-2 rounded-full px-3 py-1 text-xs font-medium bg-fillWeaker"] "Connected"
    div_ [class_ "p-5"] $ do
      div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] $ do
        div_ [class_ "space-y-2"] $ do
          label_ [class_ "flex items-center gap-2 text-sm font-medium"] "File"
          input_ [class_ "input rounded-lg w-full border border-strokeStrong", type_ "file"]
      div_ [class_ "mt-4 flex flex-wrap items-center gap-3"] $ do
        button_ [class_ "btn btn-sm btn-primary"] "Upload"
        span_ [class_ "text-sm text-textWeak"] "Make sure to enter credentials first"
      div_ [class_ "mt-4 alert border-emerald-300"] $ do
        strong_ "Success"
        p_ "Bucket connection validated."


-- Tabs
--   div_ [class_ "tabs w-full"] $ do
--     div_ [class_ "tabs-list grid w-full grid-cols-3 md:w-auto md:grid-cols-3"] $ do
--       tabButton "upload" "Upload"
--       tabButton "browse" "Browse"
--       tabButton "help" "CORS & Setup"

--     -- Upload Tab
--     div_ [class_ "tabs-content mt-4"] $ do
--       card_ "Upload a Test File" "Uploads server-side with your provided credentials." $ do
--         div_ [class_ "grid gap-4 md:grid-cols-[1fr,1fr]"] $ do
--           field "File"
--           field "Object Key (optional)"
--         div_ [class_ "mt-4 flex items-center gap-3"]
--           $ button_ [class_ "btn"] "Upload"

--     -- Browse Tab
--     div_ [class_ "tabs-content mt-4"] $ do
--       card_ "Browse Objects" "List objects by prefix." $ do
--         div_ [class_ "grid gap-3 md:grid-cols-[1fr,150px,auto]"] $ do
--           field "Prefix"
--           field "Max Keys"
--           div_ [class_ "flex items-end"] $ button_ [class_ "btn"] "List Objects"
--         div_ [class_ "mt-4 rounded-md border"] $ table_ $ do
--           thead_ $ tr_ $ do
--             th_ "Key"
--             th_ [class_ "w-28"] "Size"
--             th_ [class_ "w-56"] "Last Modified"
--           tbody_ $ tr_ $ td_ [colspan_ "3", class_ "text-muted-foreground"] "No results."

--     -- Help Tab
--     div_ [class_ "tabs-content mt-4"] $ do
--       card_ "CORS & Setup Help" "Bucket configuration tips and a CORS policy example for browser uploads." $ do
--         sectionTitle "Recommended Bucket Settings"
--         ul_ [class_ "list-disc pl-5 text-sm text-muted-foreground space-y-1"] $ do
--           li_ "Object Ownership: ACLs enabled, Bucket owner preferred"
--           li_ "Block Public Access: Uncheck “Block all public access” if serving public files"
--           li_ "IAM policy must allow s3:DeleteObject, s3:GetObject, s3:ListBucket, s3:PutObject, s3:PutObjectAcl"
--         hr_ []
--         sectionTitle "Example CORS Policy"
--         textarea_ [class_ "font-mono text-xs min-h-36", readonly_ "true"] "[CORS JSON here]"
--         p_ [class_ "text-xs text-muted-foreground"] "Add this under your bucket's Permissions → CORS."
--         hr_ []
--         sectionTitle "Alternative: Vercel Blob"
--         pre_ [class_ "p-3 text-xs"] "import { put } from \"@vercel/blob\" ..."

connectionField :: Text -> Html ()
connectionField lbl =
  div_ [class_ "space-y-2"] $ do
    label_ [class_ "flex items-center gap-2 text-sm font-medium"] $ toHtml lbl
    input_ [class_ "input rounded-lg w-full border border-strokeStrong", type_ "text", placeholder_ lbl]
