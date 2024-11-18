module Pages.Onboarding.Components (
  renderOnboardingWrapper,
  renderOnboardingHeaderImage,
  renderFormQuestion,
  renderBackButton,
  renderPrimaryButton,
  renderSecondaryButton,
  renderInfoMessage,
  renderInput,
  renderSelect,
) where

import Data.Bool
import Data.Maybe (Maybe)
import Data.Monoid (Monoid (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Base (Maybe (..))
import Lucid
import Lucid.Htmx (hxDelete_, hxGet_, hxInclude_, hxPost_, hxPut_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Pages.Onboarding.Helpers
import Pages.Onboarding.Types (HTTPMethod)
import Relude hiding (ask, asks)
import Utils (faSprite_)


renderOnboardingWrapper :: Maybe Text -> Html () -> Html ()
renderOnboardingWrapper mStyle content = div_ [] $ do
  -- Logo section
  renderOnboardingHeaderImage

  let pageWidth = fromMaybe "max-w-md" mStyle

  -- Main Content
  main_ [role_ "main"] $ do
    div_ [class_ $ pageWidth <> " mx-auto p-6 pt-32 pb-32"] $ do
      -- Back Button
      renderBackButton "Back"
      content


-- | Logo header component
renderOnboardingHeaderImage :: Html ()
renderOnboardingHeaderImage =
  header_ [role_ "banner"] $ do
    div_ [class_ " mt-10 ml-11"]
      $ img_ [src_ "/public/assets/svgs/logo.svg", alt_ "APIToolkit Logo"]


renderBackButton :: Text -> Html ()
renderBackButton label = do
  -- Back button
  nav_
    [ role_ "navigation"
    -- , aria_label_ "Back navigation"
    ]
    $ button_ [class_ "flex items-center gap-2 mb-3 group", onclick_ "history.back()"]
    $ do
      div_ [class_ "w-8 h-8 flex items-center justify-center bg-blue-500 rounded-full"]
        $ faSprite_ "chevron-left" "regular" "w-3 h-3 text-white"
      span_ [class_ "text-slate-600 text-lg"] (toHtml label)


renderFormQuestion :: Text -> Maybe Text -> Maybe Text -> Html ()
renderFormQuestion mainText optionalText1 optionalText2 =
  h1_ [id_ "form-title", class_ "text-slate-950 text-4xl font-semibold font-['Inter'] leading-10"] $ do
    toHtml mainText
    case optionalText1 of
      Just text -> do
        br_ []
        toHtml text
      Nothing -> pass
    case optionalText2 of
      Just text -> do
        br_ []
        toHtml text
      Nothing -> pass


-- | Primary button component
renderPrimaryButton :: HTTPMethod -> Text -> Text -> Html ()
renderPrimaryButton method url label =
  button_
    [ type_ "button"
    , class_ primaryButtonClasses
    , customMethodAttribute method url
    ]
    $ do
      span_ [class_ "text-white text-sm font-medium font-['Inter'] leading-snug"]
        $ toHtml label
  where
    primaryButtonClasses =
      mconcat
        [ "w-full px-6 py-4 border-2 border-transparent "
        , "bg-gradient-to-b from-[#256bf6] to-[#1055de] "
        , "rounded-2xl shadow-inner justify-center items-center gap-2.5 inline-flex "
        , "hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 mt-8"
        ]


-- | Secondary button (text link style)
renderSecondaryButton :: Text -> Text -> Html ()
renderSecondaryButton url label =
  a_ [href_ url, class_ "block text-center text-slate-600 hover:text-slate-500 underline"]
    $ toHtml label


-- | Input field component
renderInput :: Text -> Text -> Text -> Text -> Bool -> Html ()
renderInput label name placeholder iconName showIcon =
  div_ [class_ "space-y-2"] $ do
    unless (T.null label)
      $ label_ [class_ "text-[#475467] text-sm font-medium font-['Inter'] leading-snug"]
      $ toHtml label
    div_ [class_ "relative"] $ do
      when showIcon
        $ div_ [class_ "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none"]
        $ faSprite_ iconName "regular" "h-5 w-5 text-slate-500"
      input_
        [ type_ "text"
        , name_ name
        , placeholder_ placeholder
        , class_
            $ mconcat
              [ "w-full px-3 py-3 bg-slate-50 text-slate-500 text-sm font-normal "
              , "font-['Inter'] leading-snug border border-slate-300 rounded-xl "
              , "placeholder-slate-400 focus:outline-none focus:ring-blue-500 "
              , "focus:border-blue-500"
              , if showIcon then " pl-10" else ""
              ]
        ]


-- | Select dropdown component
renderSelect :: Text -> Text -> [(Text, Text)] -> Html ()
renderSelect label name options =
  div_ [class_ "space-y-2"] $ do
    label_ [class_ "text-[#475467] text-sm font-medium font-['Inter'] leading-snug"]
      $ toHtml label
    div_ [class_ "relative bg-slate-50"] $ do
      select_
        [ class_ "w-full px-3 py-2 border border-slate-300 bg-slate-50 rounded-xl shadow-sm appearance-none focus:outline-none focus:ring-blue-500 focus:border-blue-500"
        , name_ name
        ]
        $ do
          forM_ options $ \(value, label') ->
            option_ [value_ value] $ toHtml label'
      div_ [class_ "absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none"]
        $ faSprite_ "chevron-down" "regular" "h-4 w-4 text-slate-400"


-- renderSelect :: Text -> Text -> [(Text, Text)] -> Maybe Text -> Html ()
-- renderSelect label name options selectedValue =
--   div_ [class_ "space-y-2"] $ do
--     label_ [class_ "text-[#475467] text-sm font-medium font-['Inter'] leading-snug"]
--       $ toHtml label
--     div_ [class_ "relative bg-slate-50"] $ do
--       select_
--         [ class_ "w-full px-3 py-2 border border-slate-300 bg-slate-50 rounded-xl shadow-sm appearance-none focus:outline-none focus:ring-blue-500 focus:border-blue-500"
--         , name_ name
--         ]
--         $ do
--           forM_ options $ \(value, label') ->
--             option_
--               [ value_ value
--               , selected_ (Just value == selectedValue)
--               ]
--               $ toHtml label'
--       div_ [class_ "absolute inset-y-0 right-0 flex items-center pr-3 pointer-events-none"]
--         $ faSprite_ "chevron-down" "regular" "h-4 w-4 text-slate-400"

-- | Divider with optional label
renderDivider :: Maybe Text -> Html ()
renderDivider mbLabel =
  div_ [class_ "relative w-full py-1"] $ do
    div_ [class_ "absolute inset-0 flex items-center"]
      $ div_ [class_ "w-full border-t border-slate-300"] mempty
    forM_ mbLabel $ \label ->
      div_ [class_ "relative flex justify-center"]
        $ span_ [class_ "bg-white px-2 py-2 bg-slate-50 text-sm text-slate-500 rounded-full border border-slate-300"]
        $ toHtml label


-- | Card container component
renderCard :: Text -> Html () -> Html ()
renderCard additionalClasses = div_ [class_ $ "rounded-xl border p-6 space-y-6 " <> additionalClasses]


-- | Feature list item component
renderFeatureItem :: Text -> Html ()
renderFeatureItem text =
  li_ [class_ "flex items-center text-slate-700"] $ do
    faSprite_ "circle-check" "solid" "w-5 h-5 mr-3 text-blue-500"
    toHtml text


-- | Info message component
renderInfoMessage :: Text -> Html ()
renderInfoMessage message =
  div_ [class_ "flex items-start space-x-2 text-sm text-slate-600 p-5 bg-slate-100 rounded-2xl justify-start gap-2"] $ do
    faSprite_ "circle-info" "regular" "w-5 h-5 mt-0.5 text-[#067cff]"
    div_ [class_ "text-slate-500 text-sm font-medium font-['Inter'] leading-snug"]
      $ toHtml message


-- | Social login button component
renderSocialLoginButton :: Text -> Text -> Text -> Html ()
renderSocialLoginButton provider iconPath url =
  button_
    [ type_ "button"
    , class_ "w-full py-2 px-4 py-3 border border-slate-300 shadow-sm bg-slate-50 rounded-2xl text-sm font-medium text-slate-700 hover:bg-slate-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 flex items-center justify-center space-x-2"
    ]
    $ do
      img_ [src_ iconPath, alt_ provider, class_ "w-5 h-5"]
      span_ [] $ "Continue with " <> toHtml provider
