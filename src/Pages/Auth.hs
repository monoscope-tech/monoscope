{-# LANGUAGE OverloadedStrings #-}

module Pages.Auth
  ( RegisterDefaults (..)
  , loginPage
  , registerPage
  )
where

import Lucid
import Relude
import System.Config (EnvConfig (..))
import Utils (escapedQueryPartial)
import Web.I18n (Language, t)


data RegisterDefaults = RegisterDefaults
  { firstName :: Text
  , lastName :: Text
  , email :: Text
  , companyName :: Text
  , companySize :: Text
  , foundUsFrom :: Text
  , redirectTo :: Maybe Text
  }


loginPage :: EnvConfig -> Language -> Maybe Text -> Maybe Text -> Html ()
loginPage envCfg lang errorMsg redirectTo =
  authShell lang (Just $ "/login?redirect_to=" <> fromMaybe "/" redirectTo) (t lang "auth.login.title") do
    p_ [class_ "text-sm text-textWeak text-center mb-5"] $ toHtml $ t lang "auth.login.welcome"
    errorAlert errorMsg
    form_ [method_ "post", action_ "/login", class_ "flex flex-col gap-3"] do
      hiddenRedirect redirectTo
      field_ (t lang "auth.login.email") "email" "email" "" True
      passwordField_ (t lang "auth.login.password") "password"
      button_ [type_ "submit", class_ "btn btn-primary w-full mt-2"] $ toHtml $ t lang "auth.login.submit"
    when envCfg.allowRegistration do
      p_ [class_ "text-sm text-textWeak text-center mt-5"] do
        toHtml $ t lang "auth.login.no_account"
        " "
        a_ [href_ $ "/register?redirect_to=" <> escapedQueryPartial (fromMaybe "/" redirectTo), class_ "link link-primary"] $ toHtml $ t lang "auth.login.register_link"


registerPage :: EnvConfig -> Language -> Bool -> Maybe Text -> RegisterDefaults -> Html ()
registerPage _envCfg lang firstUser errorMsg defaults =
  authShell lang (Just $ "/register?redirect_to=" <> fromMaybe "/" defaults.redirectTo) (t lang "auth.register.title") do
    when firstUser do
      div_ [class_ "alert alert-info text-sm mb-4"] $ toHtml $ t lang "auth.register.first_user_hint"
    errorAlert errorMsg
    form_ [method_ "post", action_ "/register", class_ "flex flex-col gap-3"] do
      hiddenRedirect defaults.redirectTo
      div_ [class_ "grid grid-cols-2 gap-3"] do
        field_ (t lang "auth.register.first_name") "firstName" "text" defaults.firstName True
        field_ (t lang "auth.register.last_name") "lastName" "text" defaults.lastName True
      field_ (t lang "auth.register.email") "email" "email" defaults.email True
      passwordField_ (t lang "auth.register.password") "password"
      passwordField_ (t lang "auth.register.password_confirm") "passwordConfirm"
      div_ [class_ "border-t border-strokeWeak my-2"] mempty
      p_ [class_ "text-xs text-textWeak"] $ toHtml $ t lang "auth.register.optional_section"
      field_ (t lang "auth.register.company_name") "companyName" "text" defaults.companyName False
      selectField_ (t lang "auth.register.company_size") "companySize" defaults.companySize
        [ ("", "—")
        , ("1-4", "1 – 4")
        , ("5-10", "5 – 10")
        , ("11-25", "11 – 25")
        , ("26+", "26+")
        ]
      selectField_ (t lang "auth.register.found_us") "foundUsFrom" defaults.foundUsFrom
        [ ("", "—")
        , ("google", t lang "auth.register.found_us.google")
        , ("github", t lang "auth.register.found_us.github")
        , ("twitter", t lang "auth.register.found_us.twitter")
        , ("linkedin", t lang "auth.register.found_us.linkedin")
        , ("friend", t lang "auth.register.found_us.friend")
        , ("other", t lang "auth.register.found_us.other")
        ]
      button_ [type_ "submit", class_ "btn btn-primary w-full mt-2"] $ toHtml $ t lang "auth.register.submit"
    p_ [class_ "text-sm text-textWeak text-center mt-5"] do
      toHtml $ t lang "auth.register.have_account"
      " "
      a_ [href_ $ "/login?redirect_to=" <> escapedQueryPartial (fromMaybe "/" defaults.redirectTo), class_ "link link-primary"] $ toHtml $ t lang "auth.register.login_link"


authShell :: Language -> Maybe Text -> Text -> Html () -> Html ()
authShell lang redirectTo title inner =
  doctypehtml_ do
    head_ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ $ toHtml $ title <> " - Monoscope"
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/public/assets/css/tailwind.min.css"]
    body_ [class_ "min-h-screen bg-bgBase text-textStrong"] do
      div_ [class_ "absolute top-4 right-4 flex items-center gap-2 text-sm"] do
        langLink "EN" "en" redirectTo
        span_ [class_ "text-textWeak"] "/"
        langLink "ES" "es" redirectTo
      main_ [class_ "min-h-screen flex items-center justify-center p-4"] do
        section_ [class_ "w-full max-w-md bg-bgRaised border border-strokeWeak rounded-lg shadow-sm p-8"] do
          div_ [class_ "flex justify-center mb-5"] do
            img_ [src_ "/public/assets/logo-mini.png", alt_ "Monoscope", class_ "h-10 w-10"]
          h1_ [class_ "text-2xl font-semibold text-center mb-2"] $ toHtml title
          inner


langLink :: Text -> Text -> Maybe Text -> Html ()
langLink label code redirectTo =
  a_ [href_ $ "/set_language/" <> code <> "?redirect_to=" <> escapedQueryPartial (fromMaybe "/" redirectTo), class_ "link link-hover font-medium"] $ toHtml label


errorAlert :: Maybe Text -> Html ()
errorAlert = \case
  Nothing -> mempty
  Just msg -> div_ [class_ "alert alert-error text-sm mb-4"] $ toHtml msg


hiddenRedirect :: Maybe Text -> Html ()
hiddenRedirect redirectTo =
  whenJust redirectTo \to -> input_ [type_ "hidden", name_ "redirectTo", value_ to]


field_ :: Text -> Text -> Text -> Text -> Bool -> Html ()
field_ labelTxt fieldName inputType val required =
  label_ [class_ "form-control w-full"] do
    div_ [class_ "label"] $ span_ [class_ "label-text"] $ toHtml labelTxt
    input_ $ [type_ inputType, name_ fieldName, value_ val, class_ "input input-bordered w-full"] <> [required_ "required" | required]


passwordField_ :: Text -> Text -> Html ()
passwordField_ labelTxt fieldName =
  label_ [class_ "form-control w-full"] do
    div_ [class_ "label"] $ span_ [class_ "label-text"] $ toHtml labelTxt
    input_ [type_ "password", name_ fieldName, class_ "input input-bordered w-full", required_ "required"]


selectField_ :: Text -> Text -> Text -> [(Text, Text)] -> Html ()
selectField_ labelTxt fieldName selectedVal opts =
  label_ [class_ "form-control w-full"] do
    div_ [class_ "label"] $ span_ [class_ "label-text"] $ toHtml labelTxt
    select_ [name_ fieldName, class_ "select select-bordered w-full"] do
      forM_ opts $ \(val, displ) ->
        option_ (value_ val : [selected_ "selected" | val == selectedVal]) (toHtml displ)
