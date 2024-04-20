module Pages.Monitors.TestCollectionEditor (collectionGetH, collectionPage) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.NonMember (userNotMemeberPage)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext)
import System.Types (ATAuthCtx)
import Utils (faIcon_, scheduleIntervals, userIsProjectMember)
import Utils qualified


collectionGetH :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (Html ())
collectionGetH pid col_id = do
  appConf <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  collectionM <- dbtToEff $ Testing.getCollectionById col_id
  project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)
  collection_steps <- dbtToEff $ Testing.getCollectionSteps col_id
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Testing"
          }
  pure $ bodyWrapper bwconf $ collectionPage pid (Unsafe.fromJust collectionM) collection_steps


collectionPage :: Projects.ProjectId -> Testing.Collection -> V.Vector Testing.CollectionStep -> Html ()
collectionPage pid col steps = do
  -- let col_json = decodeUtf8 $ AE.encode col
  -- let steps_json = decodeUtf8 $ AE.encode steps
  section_ [id_ "test-data", class_ "grid grid-cols-3 h-full"] do
    div_ [class_ "col-span-2 h-full divide-x divide-gray-200"] do
      div_ [class_ "flex justify-between"] do
        div_ [class_ " pb-5 p-5"] do
          h2_ [class_ "text-base font-semibold leading-6 text-gray-900"] do
            toHtml col.title
            small_ [class_ "inline-block ml-2 truncate text-sm text-gray-500"] "created 20th June"
          p_ [] $ toHtml col.description
        div_ [] do
          span_ [class_ "badge badge-success"] "Active"
          Utils.faSprite_ "ellipsis-vertical" "solid" "h-3"
    div_ [class_ "col-span-1 h-full border-r border-gray-200"] do
      div_ do
        div_ [] $ Utils.faSprite_ "inbox-full" "solid" "w-6 h-6"
        p_ [] "Run a test to view the results here."

-- script_ [type_ "module", src_ "/assets/testeditor.js"] ("" :: Text)
-- script_ [src_ "/assets/js/thirdparty/jsyaml.min.js", crossorigin_ "true"] ("" :: Text)
-- script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.js"] ("" :: Text)
-- script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/mode/yaml/yaml.js"] ("" :: Text)
-- style_
--   [text|
--       .CodeMirror {
--           height: 100%;
--       }
--   |]
