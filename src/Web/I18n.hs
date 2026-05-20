{-# LANGUAGE OverloadedStrings #-}

module Web.I18n
  ( Language (..)
  , parseLanguage
  , languageCode
  , t
  , languageFromCookies
  , languageSetCookieBS
  )
where

import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Time.Clock (secondsToDiffTime)
import Relude
import Web.Cookie (Cookies, SetCookie (..), defaultSetCookie)


data Language = En | Es deriving stock (Eq, Generic, Read, Show)
  deriving anyclass (NFData)


parseLanguage :: Text -> Language
parseLanguage "es" = Es
parseLanguage _ = En


languageCode :: Language -> Text
languageCode En = "en"
languageCode Es = "es"


t :: Language -> Text -> Text
t lang key = case Map.lookup key (dict lang) of
  Just v -> v
  Nothing -> key


dict :: Language -> Map.Map Text Text
dict En = en
dict Es = es


en :: Map.Map Text Text
en =
  Map.fromList
    [ ("auth.login.title", "Sign in")
    , ("auth.login.email", "Email")
    , ("auth.login.password", "Password")
    , ("auth.login.submit", "Sign in")
    , ("auth.login.no_account", "Don't have an account?")
    , ("auth.login.register_link", "Register")
    , ("auth.login.error_invalid", "Invalid email or password")
    , ("auth.login.welcome", "Welcome back to Monoscope")
    , ("auth.register.title", "Create your account")
    , ("auth.register.first_name", "First name")
    , ("auth.register.last_name", "Last name")
    , ("auth.register.email", "Email")
    , ("auth.register.password", "Password")
    , ("auth.register.password_confirm", "Confirm password")
    , ("auth.register.submit", "Create account")
    , ("auth.register.have_account", "Already have an account?")
    , ("auth.register.login_link", "Sign in")
    , ("auth.register.error_mismatch", "Passwords do not match")
    , ("auth.register.error_short", "Password must be at least 8 characters")
    , ("auth.register.error_exists", "An account with that email already exists")
    , ("auth.register.error_closed", "Registration is closed")
    , ("auth.register.first_user_hint", "You're creating the first admin account for this Monoscope instance.")
    , ("auth.register.optional_section", "Optional — helps us understand who's using Monoscope")
    , ("auth.register.company_name", "Company / Organization")
    , ("auth.register.company_size", "Company size")
    , ("auth.register.found_us", "How did you hear about us?")
    , ("auth.register.found_us.google", "Google")
    , ("auth.register.found_us.github", "GitHub")
    , ("auth.register.found_us.twitter", "Twitter / X")
    , ("auth.register.found_us.linkedin", "LinkedIn")
    , ("auth.register.found_us.friend", "A friend")
    , ("auth.register.found_us.other", "Other")
    , ("onboarding.project.title", "Name your project")
    , ("onboarding.project.subtitle", "This is how it'll appear in the sidebar and dashboards. You can change it later in Settings.")
    , ("onboarding.project.name_label", "Project name")
    , ("onboarding.project.proceed", "Continue")
    , ("nav.logout", "Logout")
    , ("nav.language", "Language")
    , ("nav.language.english", "English")
    , ("nav.language.spanish", "Spanish")
    , ("error.401.title", "Unauthorized")
    , ("error.401.desc", "You need to be logged in to access this page.")
    , ("error.404.title", "Not Found")
    , ("error.404.desc", "The page you're looking for doesn't exist or has been moved.")
    , ("error.403.title", "Forbidden")
    , ("error.403.desc", "You don't have permission to access this resource.")
    , ("error.500.title", "Internal Server Error")
    , ("error.500.desc", "Something went wrong on our end. Please try again later.")
    , ("error.go_back", "Go Back")
    , ("error.home", "Home")
    ]


es :: Map.Map Text Text
es =
  Map.fromList
    [ ("auth.login.title", "Iniciar sesión")
    , ("auth.login.email", "Correo electrónico")
    , ("auth.login.password", "Contraseña")
    , ("auth.login.submit", "Iniciar sesión")
    , ("auth.login.no_account", "¿No tienes una cuenta?")
    , ("auth.login.register_link", "Regístrate")
    , ("auth.login.error_invalid", "Correo o contraseña incorrectos")
    , ("auth.login.welcome", "Bienvenido de vuelta a Monoscope")
    , ("auth.register.title", "Crea tu cuenta")
    , ("auth.register.first_name", "Nombre")
    , ("auth.register.last_name", "Apellido")
    , ("auth.register.email", "Correo electrónico")
    , ("auth.register.password", "Contraseña")
    , ("auth.register.password_confirm", "Confirmar contraseña")
    , ("auth.register.submit", "Crear cuenta")
    , ("auth.register.have_account", "¿Ya tienes una cuenta?")
    , ("auth.register.login_link", "Inicia sesión")
    , ("auth.register.error_mismatch", "Las contraseñas no coinciden")
    , ("auth.register.error_short", "La contraseña debe tener al menos 8 caracteres")
    , ("auth.register.error_exists", "Ya existe una cuenta con ese correo")
    , ("auth.register.error_closed", "El registro está cerrado")
    , ("auth.register.first_user_hint", "Estás creando la primera cuenta de administrador para esta instancia de Monoscope.")
    , ("auth.register.optional_section", "Opcional — nos ayuda a entender quién está usando Monoscope")
    , ("auth.register.company_name", "Empresa / Organización")
    , ("auth.register.company_size", "Tamaño de la empresa")
    , ("auth.register.found_us", "¿Cómo nos conociste?")
    , ("auth.register.found_us.google", "Google")
    , ("auth.register.found_us.github", "GitHub")
    , ("auth.register.found_us.twitter", "Twitter / X")
    , ("auth.register.found_us.linkedin", "LinkedIn")
    , ("auth.register.found_us.friend", "Un amigo")
    , ("auth.register.found_us.other", "Otro")
    , ("onboarding.project.title", "Nombra tu proyecto")
    , ("onboarding.project.subtitle", "Así aparecerá en el menú lateral y los dashboards. Lo puedes cambiar después en Configuración.")
    , ("onboarding.project.name_label", "Nombre del proyecto")
    , ("onboarding.project.proceed", "Continuar")
    , ("nav.logout", "Cerrar sesión")
    , ("nav.language", "Idioma")
    , ("nav.language.english", "Inglés")
    , ("nav.language.spanish", "Español")
    , ("error.401.title", "No autorizado")
    , ("error.401.desc", "Necesitas iniciar sesión para acceder a esta página.")
    , ("error.404.title", "No encontrado")
    , ("error.404.desc", "La página que buscas no existe o se ha movido.")
    , ("error.403.title", "Prohibido")
    , ("error.403.desc", "No tienes permiso para acceder a este recurso.")
    , ("error.500.title", "Error interno del servidor")
    , ("error.500.desc", "Algo salió mal de nuestro lado. Intenta de nuevo más tarde.")
    , ("error.go_back", "Volver")
    , ("error.home", "Inicio")
    ]


languageFromCookies :: Cookies -> Language
languageFromCookies cs = case L.lookup "lang" cs of
  Just "es" -> Es
  Just "en" -> En
  _ -> En


languageSetCookieBS :: Language -> SetCookie
languageSetCookieBS lang =
  defaultSetCookie
    { setCookieName = "lang"
    , setCookieValue = encodeUtf8 (languageCode lang)
    , setCookiePath = Just "/"
    , setCookieMaxAge = Just (secondsToDiffTime $ 365 * 24 * 60 * 60)
    , setCookieHttpOnly = False
    , setCookieSecure = False
    }
