module Pages.Bots.Github (setupGetH) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask, asks)
import Lucid
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Relude hiding (ask, asks)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_)


setupGetH :: Maybe Text -> Maybe Projects.ProjectId -> Maybe Text -> ATBaseCtx (PageCtx (Html ()))
setupGetH pid installationId action = do
  appCtx <- asks env
  let bwconf = (def :: BWConfig){pageTitle = "GitHub Bot Setup", isSettingsPage = True}
  pure $ PageCtx bwconf setupPage


setupPage :: Html ()
setupPage = div_ [class_ "w-full h-full flex flex-col pt-16"] do
  div_ [class_ "max-w-2xl mx-auto mt-16 p-6"] $ do
    div_ [class_ "flex justify-center mb-4"] $ do
      faSprite_ "circle-check" "regular" "fill-green-400 h-16 w-16"

    h1_ [class_ "text-3xl font-bold text-center mb-2"] "Installation Successful!"

    p_ [class_ "text-center text-textWeak max-w-lg text-sm mb-6"] "Monoscope GitHub app has been installed you can now configure a repo for syncing dashboards"

    div_ [class_ "bg-fillWeak p-4 rounded border border-strokeWeak"] $ do
      p_ [class_ "font-semibold mb-2"] "What's Next?"
      ul_ [class_ "list-disc list-inside text-sm space-y-1 ml-4"] $ do
        li_ [] "You can now **close this window** and return to your application."
        li_ [] "Check your repository for initial configuration changes or first runs."

    -- Optional: Link back to main app page
    div_ [class_ "mt-6 text-center"]
      $ a_ [href_ "/", class_ "text-blue-500 hover:text-blue-700 font-medium"]
      $ "Return to the Application Dashboard"
