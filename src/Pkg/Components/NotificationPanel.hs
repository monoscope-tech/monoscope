module Pkg.Components.NotificationPanel (
  -- * Types
  IntegrationCard (..),
  IntegrationStatus (..),

  -- * Builders
  integrationCard,
  withTestButton,
  withExtraContent,

  -- * Rendering
  renderIntegrationCard,
  renderIntegrationCards,
  renderStatusBadge,
) where

import Lucid
import Lucid.Aria qualified as Aria
import Relude
import Utils (faSprite_)


-- | Status of an integration channel
data IntegrationStatus
  = Active -- Enabled and configured
  | Connected -- Configured but not enabled
  | Unconfigured -- Not yet set up
  deriving stock (Eq, Show)


-- | A single integration/notification channel card
data IntegrationCard = IntegrationCard
  { title :: Text
  , description :: Text
  , value :: Text -- Form value for the checkbox
  , icon :: Html () -- Icon content
  , status :: IntegrationStatus
  , isChecked :: Bool
  , testButton :: Maybe (Html ()) -- Optional test button
  , extraContent :: Maybe (Html ()) -- Configuration content shown when expanded
  }


-- | Create a basic integration card
integrationCard :: Text -> Text -> Text -> Html () -> IntegrationStatus -> Bool -> IntegrationCard
integrationCard title description value icon status isChecked =
  IntegrationCard
    { title
    , description
    , value
    , icon
    , status
    , isChecked
    , testButton = Nothing
    , extraContent = Nothing
    }


withTestButton :: Html () -> IntegrationCard -> IntegrationCard
withTestButton btn ic = ic{testButton = Just btn}


withExtraContent :: Html () -> IntegrationCard -> IntegrationCard
withExtraContent content ic = ic{extraContent = Just content}


-- | Render a list of integration cards
renderIntegrationCards :: [IntegrationCard] -> Html ()
renderIntegrationCards = div_ [class_ "space-y-4"] . mapM_ renderIntegrationCard


-- | Render a single integration card
renderIntegrationCard :: IntegrationCard -> Html ()
renderIntegrationCard ic = do
  let isActive = ic.status == Active
      borderClass = if isActive then "border-fillBrand-weak" else "border-strokeWeak"
      iconBgClass = if isActive then "bg-fillBrand-weak" else "bg-fillWeak"

  div_ [class_ $ "bg-bgRaised rounded-lg border shadow-xs hover-only:hover:shadow-md transition-shadow duration-200 " <> borderClass] do
    div_ [class_ "p-4"] do
      -- Header: icon, title/description, status badge
      div_ [class_ "flex items-start justify-between gap-3 mb-3"] do
        div_ [class_ "flex items-start gap-3 flex-1 min-w-0"] do
          div_ [class_ $ "flex h-10 w-10 items-center justify-center rounded-lg shrink-0 " <> iconBgClass] ic.icon
          div_ [class_ "flex-1 min-w-0"] do
            h3_ [class_ "text-base font-semibold text-textStrong"] $ toHtml ic.title
            p_ [class_ "text-sm text-textWeak mt-0.5"] $ toHtml ic.description
        div_ $ renderStatusBadge ic.status

      -- Test button and toggle
      div_ [class_ "flex items-center justify-between gap-2 mt-4 pt-3 border-t border-strokeWeak"] do
        div_ [class_ "flex-1", id_ $ ic.value <> "-test-button"] do
          case ic.testButton of
            Just btn | isActive -> btn
            _ ->
              button_ [type_ "button", disabled_ "", class_ "btn btn-xs btn-outline btn-neutral gap-1 cursor-not-allowed", Aria.label_ "Test unavailable - enable integration first"] do
                faSprite_ "flask-vial" "regular" "h-3 w-3"
                "Test"
        -- Toggle
        label_ [class_ "relative inline-flex items-center cursor-pointer tap-target", Aria.label_ $ if ic.isChecked then "Disable " <> ic.title else "Enable " <> ic.title] do
          input_
            $ [type_ "checkbox", value_ ic.value, name_ "notifChannel", class_ "toggle toggle-primary"]
            <> [checked_ | ic.isChecked]

    -- Extra configuration content
    whenJust ic.extraContent \content ->
      div_ [class_ "px-4 pb-4 border-t border-strokeWeak pt-4"] content


-- | Render a status badge for an integration
renderStatusBadge :: IntegrationStatus -> Html ()
renderStatusBadge = \case
  Active -> span_ [class_ "badge badge-success badge-sm gap-1 shrink-0"] do
    faSprite_ "circle-check" "solid" "h-3 w-3"
    "Active"
  Connected -> span_ [class_ "badge badge-ghost badge-sm gap-1 shrink-0"] do
    faSprite_ "circle" "regular" "h-3 w-3"
    "Connected"
  Unconfigured -> span_ [class_ "badge badge-ghost badge-sm shrink-0"] "Configure"
