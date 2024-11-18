module Pages.Onboarding.Helpers (customMethodAttribute, customTagifyStyles, emailTagsScript, countrySelect, fullCountryCodes) where

import Data.Foldable (mapM_)
import Data.Text
import Data.Text qualified as T
import GHC.Base
import Gogol.PubSub (Schema (name))
import Lucid
import Lucid.Base
import Lucid.Html5
import Lucid.Htmx (hxDelete_, hxGet_, hxPatch_, hxPost_, hxPut_)
import Pages.Onboarding.Types
import Utils (faSprite_)


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
    , "  --tags-border-color: #d1d5db;"
    , "  --tags-hover-border-color: #9ca3af;"
    , "  --tags-focus-border-color: #3b82f6;"
    , "  --tag-bg: #e2e8f0;"
    , "  --tag-hover: #dbeafe;"
    , "  --tag-text-color: #475467;"
    , "  --tag-text-color--edit: #475467;"
    , "  --tag-pad: 0.125rem 0.125rem;"
    , "  --tag-inset-shadow-size: 1.1em;"
    , "  --tag-invalid-color: #991b1b;"
    , "  --tag-invalid-bg: #fee2e2;"
    , "  --tag-remove-bg: #991b1b;"
    , "  --tag-remove-btn-color: #94a3b8;"
    , "  --tag-remove-btn-bg: none;"
    , "  --tag-remove-btn-bg--hover: #dc2626;"
    , "  font-size: 0.75rem;"
    , "  padding: 0.5rem;"
    , "}"
    , ""
    , "/* Remove button styling */"
    , ".tagify__tag__removeBtn {"
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
    , "/* Input placeholder color */"
    , ".tagify__input {"
    , "  color: #94a3b8;"
    , "}"
    , ""
    , ".tagify--focus {"
    , "  border-color: #e5e7eb !important;"
    , "}"
    , ""
    , ".tagify__tag__removeBtn {"
    , "  width: 0.5rem;"
    , "  height: 0.5rem;"
    , "  border-radius: 9999px;"
    , "  background: white;"
    , "  margin-left: 0.25rem;"
    , "  display: inline-flex;"
    , "  align-items: center;"
    , "  justify-content: center;"
    , "}"
    , ""
    , "/* Tag content layout */"
    , ".tagify__tag > div {"
    , "  padding: 0.25rem 0.25rem;"
    , "  display: inline-flex;"
    , "  align-items: center;"
    , "  gap: 0rem;"
    , "}"
    , ""
    , ".tagify__tag__removeBtn .tagify__tag__removeBtn__inner::before,"
    , ".tagify__tag__removeBtn .tagify__tag__removeBtn__inner::after {"
    , "  background: #e2e8f0;"
    , "  width: 0.125rem;"
    , "  height: 0.125rem;"
    , "  padding: 0.125rem;"
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


-- (code, country name)
type CountryCode = (Text, Text)


-- Generate the select element
countrySelect :: [CountryCode] -> Html ()
countrySelect countries =
  div_ [class_ "relative max-w-20"] $ do
    select_
      [ class_ "appearance-none bg-transparent outline-none text-slate-500 text-sm font-normal font-['Inter'] leading-snug pr-5"
      , onchange_ "this.options[this.selectedIndex].textContent = this.options[this.selectedIndex].value"
      , onfocus_ "this.options[this.selectedIndex].textContent = this.options[this.selectedIndex].dataset.full"
      , name_ "country_code"
      ]
      $ do
        option_ [value_ "+234"] "+234"
        mapM_ makeOption countries
    div_ [class_ "pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2"] $
      faSprite_ "chevron-down" "regular" "h-3.5 w-3.5"
  where
    makeOption :: CountryCode -> Html ()
    makeOption (code, country) =
      let fullText = country <> " (" <> code <> ")"
       in option_
            [ value_ code
            , data_ "full" fullText
            ]
            (toHtml fullText)


fullCountryCodes :: [CountryCode]
fullCountryCodes =
  [ ("+93", "Afghanistan")
  , ("+355", "Albania")
  , ("+213", "Algeria")
  , ("+1-684", "American Samoa")
  , ("+376", "Andorra")
  , ("+244", "Angola")
  , ("+1-264", "Anguilla")
  , ("+672", "Antarctica")
  , ("+1-268", "Antigua and Barbuda")
  , ("+54", "Argentina")
  , ("+374", "Armenia")
  , ("+297", "Aruba")
  , ("+61", "Australia")
  , ("+43", "Austria")
  , ("+994", "Azerbaijan")
  , ("+1-242", "Bahamas")
  , ("+973", "Bahrain")
  , ("+880", "Bangladesh")
  , ("+1-246", "Barbados")
  , ("+375", "Belarus")
  , ("+32", "Belgium")
  , ("+501", "Belize")
  , ("+229", "Benin")
  , ("+1-441", "Bermuda")
  , ("+975", "Bhutan")
  , ("+591", "Bolivia")
  , ("+387", "Bosnia and Herzegovina")
  , ("+267", "Botswana")
  , ("+55", "Brazil")
  , ("+246", "British Indian Ocean Territory")
  , ("+1-284", "British Virgin Islands")
  , ("+673", "Brunei")
  , ("+359", "Bulgaria")
  , ("+226", "Burkina Faso")
  , ("+257", "Burundi")
  , ("+855", "Cambodia")
  , ("+237", "Cameroon")
  , ("+1", "Canada")
  , ("+238", "Cape Verde")
  , ("+1-345", "Cayman Islands")
  , ("+236", "Central African Republic")
  , ("+235", "Chad")
  , ("+56", "Chile")
  , ("+86", "China")
  , ("+61", "Christmas Island")
  , ("+61", "Cocos Islands")
  , ("+57", "Colombia")
  , ("+269", "Comoros")
  , ("+682", "Cook Islands")
  , ("+506", "Costa Rica")
  , ("+385", "Croatia")
  , ("+53", "Cuba")
  , ("+599", "Curacao")
  , ("+357", "Cyprus")
  , ("+420", "Czech Republic")
  , ("+243", "Democratic Republic of the Congo")
  , ("+45", "Denmark")
  , ("+253", "Djibouti")
  , ("+1-767", "Dominica")
  , ("+1-809", "Dominican Republic")
  , ("+670", "East Timor")
  , ("+593", "Ecuador")
  , ("+20", "Egypt")
  , ("+503", "El Salvador")
  , ("+240", "Equatorial Guinea")
  , ("+291", "Eritrea")
  , ("+372", "Estonia")
  , ("+251", "Ethiopia")
  , ("+500", "Falkland Islands")
  , ("+298", "Faroe Islands")
  , ("+679", "Fiji")
  , ("+358", "Finland")
  , ("+33", "France")
  , ("+689", "French Polynesia")
  , ("+241", "Gabon")
  , ("+220", "Gambia")
  , ("+995", "Georgia")
  , ("+49", "Germany")
  , ("+233", "Ghana")
  , ("+350", "Gibraltar")
  , ("+30", "Greece")
  , ("+299", "Greenland")
  , ("+1-473", "Grenada")
  , ("+1-671", "Guam")
  , ("+502", "Guatemala")
  , ("+44-1481", "Guernsey")
  , ("+224", "Guinea")
  , ("+245", "Guinea-Bissau")
  , ("+592", "Guyana")
  , ("+509", "Haiti")
  , ("+504", "Honduras")
  , ("+852", "Hong Kong")
  , ("+36", "Hungary")
  , ("+354", "Iceland")
  , ("+91", "India")
  , ("+62", "Indonesia")
  , ("+98", "Iran")
  , ("+964", "Iraq")
  , ("+353", "Ireland")
  , ("+44-1624", "Isle of Man")
  , ("+972", "Israel")
  , ("+39", "Italy")
  , ("+225", "Ivory Coast")
  , ("+1-876", "Jamaica")
  , ("+81", "Japan")
  , ("+44-1534", "Jersey")
  , ("+962", "Jordan")
  , ("+7", "Kazakhstan")
  , ("+254", "Kenya")
  , ("+686", "Kiribati")
  , ("+383", "Kosovo")
  , ("+965", "Kuwait")
  , ("+996", "Kyrgyzstan")
  , ("+856", "Laos")
  , ("+371", "Latvia")
  , ("+961", "Lebanon")
  , ("+266", "Lesotho")
  , ("+231", "Liberia")
  , ("+218", "Libya")
  , ("+423", "Liechtenstein")
  , ("+370", "Lithuania")
  , ("+352", "Luxembourg")
  , ("+853", "Macau")
  , ("+389", "Macedonia")
  , ("+261", "Madagascar")
  , ("+265", "Malawi")
  , ("+60", "Malaysia")
  , ("+960", "Maldives")
  , ("+223", "Mali")
  , ("+356", "Malta")
  , ("+692", "Marshall Islands")
  , ("+222", "Mauritania")
  , ("+230", "Mauritius")
  , ("+262", "Mayotte")
  , ("+52", "Mexico")
  , ("+691", "Micronesia")
  , ("+373", "Moldova")
  , ("+377", "Monaco")
  , ("+976", "Mongolia")
  , ("+382", "Montenegro")
  , ("+1-664", "Montserrat")
  , ("+212", "Morocco")
  , ("+258", "Mozambique")
  , ("+95", "Myanmar")
  , ("+264", "Namibia")
  , ("+674", "Nauru")
  , ("+977", "Nepal")
  , ("+31", "Netherlands")
  , ("+599", "Netherlands Antilles")
  , ("+687", "New Caledonia")
  , ("+64", "New Zealand")
  , ("+505", "Nicaragua")
  , ("+227", "Niger")
  , ("+234", "Nigeria")
  , ("+683", "Niue")
  , ("+850", "North Korea")
  , ("+1-670", "Northern Mariana Islands")
  , ("+47", "Norway")
  , ("+968", "Oman")
  , ("+92", "Pakistan")
  , ("+680", "Palau")
  , ("+970", "Palestine")
  , ("+507", "Panama")
  , ("+675", "Papua New Guinea")
  , ("+595", "Paraguay")
  , ("+51", "Peru")
  , ("+63", "Philippines")
  , ("+64", "Pitcairn")
  , ("+48", "Poland")
  , ("+351", "Portugal")
  , ("+1-787", "Puerto Rico")
  , ("+974", "Qatar")
  , ("+242", "Republic of the Congo")
  , ("+262", "Reunion")
  , ("+40", "Romania")
  , ("+7", "Russia")
  , ("+250", "Rwanda")
  , ("+590", "Saint Barthelemy")
  , ("+290", "Saint Helena")
  , ("+1-869", "Saint Kitts and Nevis")
  , ("+1-758", "Saint Lucia")
  , ("+590", "Saint Martin")
  , ("+508", "Saint Pierre and Miquelon")
  , ("+1-784", "Saint Vincent and the Grenadines")
  , ("+685", "Samoa")
  , ("+378", "San Marino")
  , ("+239", "Sao Tome and Principe")
  , ("+966", "Saudi Arabia")
  , ("+221", "Senegal")
  , ("+381", "Serbia")
  , ("+248", "Seychelles")
  , ("+232", "Sierra Leone")
  , ("+65", "Singapore")
  , ("+1-721", "Sint Maarten")
  , ("+421", "Slovakia")
  , ("+386", "Slovenia")
  , ("+677", "Solomon Islands")
  , ("+252", "Somalia")
  , ("+27", "South Africa")
  , ("+82", "South Korea")
  , ("+211", "South Sudan")
  , ("+34", "Spain")
  , ("+94", "Sri Lanka")
  , ("+249", "Sudan")
  , ("+597", "Suriname")
  , ("+47", "Svalbard and Jan Mayen")
  , ("+268", "Swaziland")
  , ("+46", "Sweden")
  , ("+41", "Switzerland")
  , ("+963", "Syria")
  , ("+886", "Taiwan")
  , ("+992", "Tajikistan")
  , ("+255", "Tanzania")
  , ("+66", "Thailand")
  , ("+228", "Togo")
  , ("+690", "Tokelau")
  , ("+676", "Tonga")
  , ("+1-868", "Trinidad and Tobago")
  , ("+216", "Tunisia")
  , ("+90", "Turkey")
  , ("+993", "Turkmenistan")
  , ("+1-649", "Turks and Caicos Islands")
  , ("+688", "Tuvalu")
  , ("+1-340", "U.S. Virgin Islands")
  , ("+256", "Uganda")
  , ("+380", "Ukraine")
  , ("+971", "United Arab Emirates")
  , ("+44", "United Kingdom")
  , ("+1", "United States")
  , ("+598", "Uruguay")
  , ("+998", "Uzbekistan")
  , ("+678", "Vanuatu")
  , ("+379", "Vatican")
  , ("+58", "Venezuela")
  , ("+84", "Vietnam")
  , ("+681", "Wallis and Futuna")
  , ("+212", "Western Sahara")
  , ("+967", "Yemen")
  , ("+260", "Zambia")
  , ("+263", "Zimbabwe")
  ]
