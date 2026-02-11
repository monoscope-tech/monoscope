{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pkg.Components.Form (
  -- * Form DSL types
  Form (..),
  FormConfig (..),
  Field (..),
  SelectOption (..),
  TagifyConfig (..),

  -- * Field builders
  textField,
  textareaField,
  selectField,
  tagifyField,
  checkboxField,
  numberField,
  hiddenField,

  -- * Field modifiers
  withPlaceholder,
  withValue,
  withRequired,
  withDisabled,
  withPattern,
  withIcon,
  withHelpText,
  withFieldAttrs,

  -- * Rendering
  renderForm,
  renderField,
  renderFieldGroup,
  renderModalForm,

  -- * Section helpers
  formSection,
  formActions,
) where

import Data.Default (Default (..))
import Data.Text qualified as T
import Lucid
import Lucid.Htmx (hxPatch_, hxPost_, hxPut_, hxSwap_, hxTarget_, hxTrigger_)
import Relude
import Utils (faSprite_)


-- | A complete form with HTMX submission
data Form = Form
  { config :: FormConfig
  , fields :: [Field]
  , sections :: [(Text, [Field])] -- Named sections with fields
  }


-- | Form configuration for submission and styling
data FormConfig = FormConfig
  { formId :: Text
  , method :: Text -- "post", "patch", "put"
  , action :: Text -- HTMX target URL
  , targetId :: Maybe Text -- hx-target
  , swapMode :: Text -- hx-swap value
  , extraAttrs :: [Attribute] -- Additional form attributes (hxExt_, hxVals_, etc.)
  , containerClass :: Text
  }


instance Default FormConfig where
  def =
    FormConfig
      { formId = "form"
      , method = "post"
      , action = ""
      , targetId = Nothing
      , swapMode = "none"
      , extraAttrs = []
      , containerClass = "flex flex-col gap-4"
      }


-- | A single form field
data Field = Field
  { fieldType :: FieldType
  , fieldId :: Text
  , name :: Text
  , label :: Text
  , placeholder :: Text
  , value :: Text
  , required :: Bool
  , disabled :: Bool
  , fieldPattern :: Maybe Text -- HTML pattern attribute
  , icon :: Maybe (Text, Text, Text) -- (iconName, iconKind, iconClasses)
  , helpText :: Maybe Text
  , extraAttrs :: [Attribute]
  , options :: [SelectOption] -- For select fields
  , tagifyConfig :: Maybe TagifyConfig -- For tagify fields
  , rows :: Int -- For textarea
  }


data FieldType
  = TextField
  | TextareaField
  | SelectField
  | TagifyField
  | CheckboxField
  | NumberField
  | HiddenField


-- | Option for select fields
data SelectOption = SelectOption
  { optLabel :: Text
  , optValue :: Text
  , optSelected :: Bool
  , optDisabled :: Bool
  }


-- | Configuration for Tagify-enhanced inputs
data TagifyConfig = TagifyConfig
  { enforceWhitelist :: Bool
  , tagTextProp :: Text
  }


instance Default TagifyConfig where
  def = TagifyConfig{enforceWhitelist = False, tagTextProp = "name"}


-- | Smart constructors for fields

textField :: Text -> Text -> Field
textField fId lbl = baseField TextField fId lbl


textareaField :: Text -> Text -> Field
textareaField fId lbl = (baseField TextareaField fId lbl){rows = 3}


selectField :: Text -> Text -> [SelectOption] -> Field
selectField fId lbl opts = (baseField SelectField fId lbl){options = opts}


tagifyField :: Text -> Text -> Field
tagifyField fId lbl =
  (baseField TagifyField fId lbl)
    { tagifyConfig = Just def
    }


checkboxField :: Text -> Text -> Field
checkboxField fId lbl = baseField CheckboxField fId lbl


numberField :: Text -> Text -> Field
numberField fId lbl = baseField NumberField fId lbl


hiddenField :: Text -> Text -> Text -> Field
hiddenField fId nm val = (baseField HiddenField fId ""){name = nm, value = val}


baseField :: FieldType -> Text -> Text -> Field
baseField ft fId lbl =
  Field
    { fieldType = ft
    , fieldId = fId
    , name = fId
    , label = lbl
    , placeholder = ""
    , value = ""
    , required = False
    , disabled = False
    , fieldPattern = Nothing
    , icon = Nothing
    , helpText = Nothing
    , extraAttrs = []
    , options = []
    , tagifyConfig = Nothing
    , rows = 3
    }


-- | Field modifiers

withPlaceholder :: Text -> Field -> Field
withPlaceholder ph f = f{placeholder = ph}


withValue :: Text -> Field -> Field
withValue v f = f{value = v}


withRequired :: Field -> Field
withRequired f = f{required = True}


withDisabled :: Field -> Field
withDisabled f = f{disabled = True}


withPattern :: Text -> Field -> Field
withPattern p f = f{fieldPattern = Just p}


withIcon :: Text -> Text -> Text -> Field -> Field
withIcon iconName iconKind iconCls f = f{icon = Just (iconName, iconKind, iconCls)}


withHelpText :: Text -> Field -> Field
withHelpText t f = f{helpText = Just t}


withFieldAttrs :: [Attribute] -> Field -> Field
withFieldAttrs attrs f = f{extraAttrs = attrs}


-- | Render a complete form with HTMX attributes
renderForm :: Form -> Html ()
renderForm frm = do
  let cfg = frm.config
      methodAttr = case T.toLower cfg.method of
        "patch" -> [hxPatch_ cfg.action]
        "put" -> [hxPut_ cfg.action]
        _ -> [hxPost_ cfg.action]
      targetAttr = maybe [] (\t -> [hxTarget_ $ "#" <> t]) cfg.targetId
      baseAttrs =
        [class_ cfg.containerClass, id_ cfg.formId, hxSwap_ cfg.swapMode, hxTrigger_ "submit"]
          <> methodAttr
          <> targetAttr
          <> cfg.extraAttrs
  form_ baseAttrs do
    forM_ frm.fields renderField
    forM_ frm.sections \(title, fields) -> do
      formSection title $ forM_ fields renderField


-- | Render a single form field with label and wrapper
renderField :: Field -> Html ()
renderField f = case f.fieldType of
  HiddenField ->
    input_ $ [type_ "hidden", name_ f.name, value_ f.value] <> f.extraAttrs
  CheckboxField ->
    div_ [class_ "flex items-center gap-2"] do
      label_ [class_ "label cursor-pointer flex items-center gap-2"] do
        input_
          $ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ f.name, value_ f.value]
          <> [checked_ | f.value == "true"]
          <> f.extraAttrs
        span_ [class_ "text-sm"] $ toHtml f.label
  _ -> case f.icon of
    Just (iconName, iconKind, iconCls) ->
      fieldset_ [class_ "fieldset p-4"] do
        label_ [class_ "label text-sm font-medium flex items-center gap-2 mb-2", Lucid.for_ f.fieldId] do
          faSprite_ iconName iconKind $ "w-4 h-4 " <> iconCls
          toHtml f.label
          renderRequiredStar f
        renderFieldInput f
        renderHelpText f
    Nothing ->
      fieldset_ [class_ "fieldset"] do
        label_ [class_ "label text-sm font-medium", Lucid.for_ f.fieldId] do
          toHtml f.label
          renderRequiredStar f
        renderFieldInput f
        renderHelpText f


renderRequiredStar :: Field -> Html ()
renderRequiredStar f = when f.required $ span_ [class_ "text-textWeak ml-1"] "*"


renderHelpText :: Field -> Html ()
renderHelpText f = whenJust f.helpText \t ->
  p_ [class_ "text-xs text-textWeak mt-1"] $ toHtml t


renderFieldInput :: Field -> Html ()
renderFieldInput f = case f.fieldType of
  TextField ->
    input_
      $ [class_ "input w-full", type_ "text", id_ f.fieldId, name_ f.name, placeholder_ f.placeholder]
      <> [value_ f.value | not (T.null f.value)]
      <> [required_ "true" | f.required]
      <> [disabled_ "" | f.disabled]
      <> maybe [] (\p -> [pattern_ p]) f.fieldPattern
      <> f.extraAttrs
  NumberField ->
    input_
      $ [class_ "input w-full", type_ "number", id_ f.fieldId, name_ f.name, placeholder_ f.placeholder]
      <> [value_ f.value | not (T.null f.value)]
      <> [required_ "true" | f.required]
      <> [disabled_ "" | f.disabled]
      <> f.extraAttrs
  TextareaField ->
    textarea_
      ( [class_ "textarea w-full min-h-20 resize-none", id_ f.fieldId, name_ f.name, placeholder_ f.placeholder, rows_ (show f.rows)]
          <> [required_ "true" | f.required]
          <> [disabled_ "" | f.disabled]
          <> f.extraAttrs
      )
      $ toHtml f.value
  SelectField ->
    select_
      ( [class_ "select w-full", id_ f.fieldId, name_ f.name]
          <> [disabled_ "" | f.disabled]
          <> f.extraAttrs
      )
      $ forM_ f.options \opt ->
        option_
          ( [value_ opt.optValue]
              <> [selected_ "" | opt.optSelected]
              <> [disabled_ "" | opt.optDisabled]
          )
          $ toHtml opt.optLabel
  TagifyField ->
    textarea_ ([class_ "textarea w-full min-h-12 resize-none", id_ f.fieldId, placeholder_ f.placeholder] <> f.extraAttrs) ""
  _ -> pass


-- | Render a group of fields side-by-side (e.g., in a grid)
renderFieldGroup :: Text -> [Field] -> Html ()
renderFieldGroup gridClass fields =
  div_ [class_ gridClass] $ forM_ fields renderField


-- | Render a form inside a modal (pairs with Components.modal_)
renderModalForm :: FormConfig -> [Field] -> Text -> Text -> Html ()
renderModalForm cfg fields cancelLabel submitLabel = do
  let methodAttr = case T.toLower cfg.method of
        "patch" -> [hxPatch_ cfg.action]
        "put" -> [hxPut_ cfg.action]
        _ -> [hxPost_ cfg.action]
      targetAttr = maybe [] (\t -> [hxTarget_ $ "#" <> t]) cfg.targetId
      baseAttrs =
        [class_ "flex flex-col p-3 gap-3", hxSwap_ cfg.swapMode, hxTrigger_ "submit"]
          <> methodAttr
          <> targetAttr
          <> cfg.extraAttrs
  form_ baseAttrs do
    forM_ fields renderField
    formActions cancelLabel submitLabel


-- | Render a titled form section
formSection :: Text -> Html () -> Html ()
formSection title content = div_ [class_ "space-y-4"] do
  h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wider"] $ toHtml title
  div_ [class_ "rounded-lg bg-bgRaised border border-strokeWeak p-4 space-y-4"] content


-- | Render form action buttons (Cancel + Submit)
formActions :: Text -> Text -> Html ()
formActions cancelLabel submitLabel =
  div_ [class_ "mt-3 flex justify-end gap-2"] do
    button_ [type_ "button", class_ "btn btn-outline"] $ toHtml cancelLabel
    button_ [type_ "submit", class_ "btn btn-primary"] $ toHtml submitLabel
