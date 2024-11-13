module Pages.Onboarding.Helpers (customMethodAttribute, customTagifyStyles, emailTagsScript) where

import Data.Text
import Lucid (Attribute)
import Lucid.Htmx (hxDelete_, hxGet_, hxPatch_, hxPost_, hxPut_)
import Pages.Onboarding.Types


-- import Data.Text qualified as T

-- Helper function to select the appropriate HTMX attribute based on HTTP method
customMethodAttribute :: HTTPMethod -> Text -> Attribute
customMethodAttribute GET url = hxGet_ url
customMethodAttribute POST url = hxPost_ url
customMethodAttribute PUT url = hxPut_ url
customMethodAttribute PATCH url = hxPatch_ url
customMethodAttribute DELETE url = hxDelete_ url


customTagifyStyles :: Text
customTagifyStyles =
  unlines
    [ ".tagify {"
    , -- Tag background color
      "  --tag-bg: rgb(226, 232, 240);"
    , -- No hover effect, matches background
      "  --tag-hover: #E2E8F0;"
    , -- Text color for tags
      "  --tag-text-color: #64748B;"
    , -- Text color in edit mode
      "  --tag-text-color--edit: #64748B;"
    , -- Padding within the tag
      "  --tag-pad: 0.125rem 0.125rem;"
    , -- Remove shadow effect
      "  --tag-inset-shadow-size: 0;"
    , -- Rounded tag corners
      "  --tag-border-radius: 9999px;"
    , -- Background for remove button
      "  --tag-remove-btn-bg: #ffffff;"
    , -- Border settings for tags container
      "  --tags-border-color: transparent;"
    , -- No border on hover
      "  --tags-hover-border-color: transparent;"
    , -- No border on focus
      "  --tags-focus-border-color: transparent;"
    , -- Font size for tag text
      "  font-size: 0.75rem;"
    , -- Border color for the entire tag component
      "  border: 1px solid #E5E7EB;"
    , -- Component border-radius
      "  border-radius: 0.75rem;"
    , -- Component padding
      "  padding: 0.250rem;"
    , "}"
    , ""
    , -- Margin between tags
      ".tagify__tag {"
    , "  margin: 0.125rem;"
    , "}"
    , ""
    , -- Padding inside individual tags and alignment
      ".tagify__tag > div {"
    , "  padding: 0.125rem 0.125rem;"
    , "  display: inline-flex;"
    , "  align-items: center;"
    , "  gap: 0rem;"
    , "}"
    , ""
    , -- Styling for remove button (X)
      ".tagify__tag__removeBtn {"
    , "  width: 1rem;"
    , "  height: 1rem;"
    , "  border-radius: 9999px;"
    , "  background: white;"
    , "  margin-left: 0.25rem;"
    , "  box-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05);"
    , "  display: inline-flex;"
    , "  align-items: center;"
    , "  justify-content: center;"
    , "}"
    , ""
    , -- Placeholder color in input
      ".tagify__input {"
    , "  color: #94A3B8;"
    , "}"
    , ""
    , -- Keep border on focus
      ".tagify--focus {"
    , "  border-color: #E5E7EB !important;"
    , "}"
    , ""
    , -- Styling for remove button's X mark
      ".tagify__tag__removeBtn .tagify__tag__removeBtn__inner::before,"
    , ".tagify__tag__removeBtn .tagify__tag__removeBtn__inner::after {"
    , "  background: #e2e8f0;"
    , "  width: 0.125rem;"
    , "  height: 0.125rem;"
    , "}"
    ]


emailTagsScript :: Text
emailTagsScript =
  unlines
    [ "var input = document.querySelector('.tagify-email');"
    , "var tagify = new Tagify(input, {"
    , "  pattern: /^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$/,"
    , "  delimiters: \",| \","
    , "  keepInvalidTags: false,"
    , "  editTags: {"
    , "    clicks: 2,"
    , "    keepInvalid: false"
    , "  },"
    , "  maxTags: 10,"
    , "  transformTag: function(tagData) {"
    , "    tagData.value = tagData.value.toLowerCase();"
    , "  },"
    , "  dropdown: {"
    , "    enabled: 0"
    , "  },"
    , "  callbacks: {"
    , "    invalid: function(e) {"
    , "      console.log(\"Invalid email:\", e.detail.value);"
    , "    }"
    , "  },"
    , "  validateTag: function(tagData) {"
    , "    const emailRegex = /^(([^<>()\\[\\]\\\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$/;"
    , "    return emailRegex.test(tagData.value);"
    , "  }"
    , "});"
    , "input.addEventListener('change', function() {"
    , "  const emails = JSON.parse(input.value).map(tag => tag.value);"
    , "  console.log(\"Valid emails:\", emails);"
    , "});"
    ]
