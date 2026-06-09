module Pages.Hardware (hardwareGetH, HardwareGet (..)) where

import Lucid
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)


-- | Hardware / digital-twin viewer page. Mounts the <hardware-monitor>
-- custom element which owns the 3D scene, 2D fallback, scrubber, and rail.
newtype HardwareGet = HardwareGet {project :: Projects.Project}
  deriving stock (Generic, Show)


instance ToHtml HardwareGet where
  toHtmlRaw = toHtml
  toHtml (HardwareGet project) =
    div_ [class_ "w-full bg-bgBase", style_ "height:calc(100vh - 56px)"]
      $ term "hardware-monitor"
        [ data_ "project-id" project.id.toText
        , class_ "block w-full h-full"
        ]
        ""


hardwareGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx HardwareGet))
hardwareGetH pid = do
  (_, project, bw) <- mkPageCtx pid
  addRespHeaders $ PageCtx bw{pageTitle = "Hardware", menuItem = Just "Hardware"} (HardwareGet project)
