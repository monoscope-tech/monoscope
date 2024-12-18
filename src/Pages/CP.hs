module Pages.CP where

import Lucid
import Lucid.Hyperscript
import NeatInterpolation
import Relude
import System.Types


pageH :: ATBaseCtx (Html ())
pageH = do
  pure page


-- | Common utility for classes
c :: Text -> Attribute
c = class_


-- A generic icon placeholder, you can customize src or inner structure if needed.
iconPlaceholder :: Html ()
iconPlaceholder = div_ [c "w-5 h-5 relative"] (return ())


-- A small circular icon (like the avatar/status in the sidebar)
userStatusIcon :: Html ()
userStatusIcon = div_ [c "w-10 h-10 relative rounded-full"] $ do
  div_ [c "w-10 h-10 left-0 top-0 absolute rounded-full border border-black/10"] (return ())
  div_ [c "w-2.5 h-2.5 left-[30px] top-[30px] absolute bg-[#17b169] rounded-full border border-white"] (return ())


-- Sidebar icon item: pass a boolean for whether it's highlighted (bg-neutral-50)
sidebarIconItem :: Bool -> Html ()
sidebarIconItem highlighted =
  div_
    [ c
        ( "w-10 h-10 p-2 rounded-md justify-center items-center inline-flex"
            <> if highlighted then " bg-neutral-50" else ""
        )
    ]
    iconPlaceholder


-- A vertical stack of icons for the sidebar
sidebarIcons :: [Bool] -> Html ()
sidebarIcons = mapM_ sidebarIconItem


-- A styled button (like "Submit corrected data" or filters)
-- Takes: classes, text
styledButton :: Text -> Text -> Html ()
styledButton extraClasses label =
  div_ [c ("px-3.5 py-2.5 rounded-lg shadow shadow-inner border-2 border-white justify-center items-center gap-1 flex " <> extraClasses)] $ do
    div_ [c "w-5 h-5 relative"] (return ())
    div_ [c "px-0.5 justify-center items-center flex"]
      $ div_ [c "text-white text-sm font-semibold font-['Inter'] leading-tight"] (toHtml label)


-- A variant of styledButton for neutral (non-purple) buttons
neutralButton :: Text -> Text -> Html ()
neutralButton extraClasses label =
  div_ [c ("px-3.5 py-2.5 bg-white rounded-lg shadow shadow-inner border border-[#d5d6d9] justify-center items-center gap-1 flex " <> extraClasses)] $ do
    div_ [c "w-5 h-5 relative"] (return ())
    div_ [c "px-0.5 justify-center items-center flex"]
      $ div_ [c "text-[#414651] text-sm font-semibold font-['Inter'] leading-tight"] (toHtml label)


-- Title with subtitle block
titleBlock :: Html ()
titleBlock = do
  div_ [c "self-stretch text-[#181d27] text-2xl font-semibold font-['Inter'] leading-loose"] "Data Overview"
  div_
    [c "self-stretch text-[#535861] text-base font-normal font-['Inter'] leading-normal"]
    "An overview of the customer data that has been flagged and requires a manual check."


-- The main rendering
page :: Html ()
page = do
  doctypehtml_ do
    head_ do
      title_ "XX"
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      link_ [rel_ "stylesheet", href_ "https://rsms.me/inter/inter.css"]
      -- link_ [rel_ "stylesheet", href_ "http://localhost:8080/static/dist/css/tailwind.min.css"]
      script_ [src_ "https://unpkg.com/hyperscript.org@0.9.4"] ""
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/public/assets/css/tailwind.min.css"]
      script_ [src_ "https://kit.fontawesome.com/0bb000a320.js"] ""
      style_
        [text|
/* CSS */
:root {
  font-family: Inter, sans-serif;
  font-feature-settings: 'liga' 1, 'calt' 1; /* fix for Chrome */
}
@supports (font-variation-settings: normal) {
  :root { font-family: InterVariable, sans-serif; }
}
        |]
    body_ [class_ "h-screen"] do
      div_ [c "w-full h-full bg-white justify-start items-start inline-flex overflow-hidden"] $ do
        -- Left sidebar
        div_ [c "w-[68px] h-full self-stretch bg-white justify-start items-start flex"]
          $ div_ [c "grow shrink basis-0 self-stretch bg-white border-r border-[#e9e9eb] flex-col justify-between items-start inline-flex"]
          $ do
            -- Top icon section
            div_ [c "self-stretch h-[318px] pt-5 flex-col justify-start items-start gap-4 flex"] $ do
              div_ [c "self-stretch h-8 px-3 flex-col justify-start items-center flex"]
                $ img_ [c "w-8 h-8", src_ "https://via.placeholder.com/32x32"]
              div_ [c "self-stretch h-[250px] px-3 flex-col justify-start items-center gap-0.5 flex"]
                $ sidebarIcons [False, True, False, False, False, False]

            -- Bottom icon section
            div_ [c "self-stretch px-3 pb-5 flex-col justify-start items-center gap-4 flex"] $ do
              div_ [c "flex-col justify-start items-start gap-0.5 flex"]
                $ sidebarIcons [False, False]
              userStatusIcon

        -- Main content area
        div_ [c "group/pg grow shrink overflow-y-scroll basis-0 self-stretch bg-white flex-col justify-start items-center gap-8 flex flex-col"] $ do
          -- Header Row
          div_ [c "self-stretch gap-6 flex justify-between px-8 pt-8"] do
            div_ do
              h2_ [class_ "text-[#181d27] text-semibold text-2xl leading-loose"] "Data Overview"
              p_ [class_ "text-[#535861] text-base font-normal"] "An overview of the customer data that has been flagged and requires a manual check."
            div_ do
              button_ [class_ "gap-2 px-3.5 py-2.5 bg-[#554696] rounded-lg shadow shadow-inner text-white text-sm justify-center items-center inline-flex"] do
                i_ [class_ "fa-regular fa-file-circle-check"] ""
                "Submit corrected data"
          div_ [c "self-stretch grow shrink basis-0 justify-between items-center flex flex-col"] $ do
            
            div_ [c "self-stretch px-8 flex-col justify-start items-start gap-6 flex"] $ do
              -- Filter Bar
              div_ [class_ "sticky top-0 z-10 px-4 py-3 bg-neutral-50 rounded-xl justify-between items-start flex w-full"] do
                select_ [class_ "select select-md select-bordered rounded-lg min-w-52 correction"] do
                  option_ "Select Field"
                  option_ [value_ "birthdayCor"] "Birthday Correction"
                  option_ [value_ "phoneCor"] "Phone Correction"
                  option_ [value_ "emailCor"] "Email Correction"
                  
                div_ [class_ "flex gap-3"] do
                  button_ [class_ "flex gap-2 items-center px-3.5 py-2.5 bg-white rounded-lg shadow shadow-inner border border-[#d5d6d9]"] do
                    i_ [class_ "fa-regular fa-calendar"] ""
                    "Nov 10, 2024 – Nov 31, 2024"

                  div_ [class_ "dropdown dropdown-end"] do
                    div_ [tabindex_ "0", role_ "button", class_ "flex gap-2 items-center px-3.5 py-2.5 bg-white rounded-lg shadow shadow-inner border border-[#d5d6d9] "] do
                      i_ [class_ "fa-solid fa-bars-filter"] ""
                      "Filters"
                    div_ [tabindex_ "0", class_ "dropdown-content flex flex-col bg-white shadow w-64 rounded-xl p-3 [&>*]:flex [&>*]:justify-between [&>*]:w-full  [&>*]:p-3"] do
                      label_ [] $ span_ "Approved Data" >> input_ [type_ "checkbox", checked_, name_ "colApprovedD", class_ "checkbox colApprovedD"]
                      label_ [] $ span_ "Incorrect Data" >> input_ [type_ "checkbox", checked_, name_ "colIncorrectD", class_ "checkbox colIncorrectD"]
                      label_ [] $ span_ "No Error Data" >> input_ [type_ "checkbox", checked_, name_ "colNoErrD", class_ "checkbox colNoErrD"]

                  div_ [class_ "dropdown dropdown-end"] do
                    div_ [tabindex_ "0", role_ "button", class_ "flex gap-2 items-center px-3.5 py-2.5 bg-white rounded-lg shadow shadow-inner border border-[#d5d6d9] "] do
                      i_ [class_ "fa-regular fa-columns-3"] "" >> "Columns"
                    div_ [tabindex_ "0", class_ "dropdown-content flex flex-col bg-white shadow w-64 rounded-xl p-3 [&>*]:flex [&>*]:justify-between [&>*]:w-full  [&>*]:p-3"] do
                      label_ [] $ span_ "Birthday" >> input_ [type_ "checkbox", checked_, name_ "colBirthday", class_ "checkbox colBirthday"]
                      label_ [] $ span_ "Phone" >> input_ [type_ "checkbox", checked_, name_ "colPhone", class_ "checkbox colPhone"]
                      label_ [] $ span_ "Email" >> input_ [type_ "checkbox", checked_, name_ "colEmail", class_ "checkbox colEmail"]
                      label_ [] $ span_ "Street" >> input_ [type_ "checkbox", checked_, name_ "colStreet", class_ "checkbox colStreet"]


              div_ [class_ "rounded-xl border w-full grow"] $ table_ [class_ "table table-pin-cols table-pin-rows text-[#535861]"] do
                thead_ do
                  tr_ [class_ "rounded-lg [&>*]:top-[4.5rem]"] do
                    th_ [class_ " w-6 text-center"] $ input_ [type_ "checkbox", class_ "checkbox rounded-lg", [__|  on click set .contactCheckbox.checked to my.checked |]]
                    th_ [class_ ""] "Contact"
                    th_ [class_ "hidden group-has-[.colBirthday:checked]/pg:table-cell space-x-2"] $ span_ "Birthday" >> i_ [class_ "fa-regular fa-angles-up-down"] "" 
                    th_ [class_ "hidden group-has-[.correction_option[value='birthdayCor']:checked]/pg:table-cell space-x-2"] $ span_ "Latest Birthday" >> i_ [class_ "fa-regular fa-angles-up-down"] ""
                    th_ [class_ "hidden group-has-[.correction_option[value='birthdayCor']:checked]/pg:table-cell space-x-2"] $ span_ "Initial Birthday" >> i_ [class_ "fa-regular fa-angles-up-down"] ""
                    th_ [class_ "hidden group-has-[.colPhone:checked]/pg:table-cell space-x-2"] $ span_ "Phone" >> i_ [class_ "fa-regular fa-angles-up-down"] "" 
                    th_ [class_ "hidden group-has-[.correction_option[value='phoneCor']:checked]/pg:table-cell space-x-2"] $ span_ "Latest Phone" >> i_ [class_ "fa-regular fa-angles-up-down"] ""
                    th_ [class_ "hidden group-has-[.correction_option[value='phoneCor']:checked]/pg:table-cell space-x-2"] $ span_ "Initial Phone" >> i_ [class_ "fa-regular fa-angles-up-down"] ""
                    th_ [class_ "hidden group-has-[.colEmail:checked]/pg:table-cell space-x-2"] $ span_ "Email" >> i_ [class_ "fa-regular fa-angles-up-down"] "" 
                    th_ [class_ "hidden group-has-[.correction_option[value='emailCor']:checked]/pg:table-cell space-x-2"] $ span_ "Latest Email" >> i_ [class_ "fa-regular fa-angles-up-down"] "" 
                    th_ [class_ "hidden group-has-[.correction_option[value='emailCor']:checked]/pg:table-cell space-x-2"] $ span_ "Initial Email" >> i_ [class_ "fa-regular fa-angles-up-down"] "" 
                    th_ [class_ "hidden group-has-[.colStreet:checked]/pg:table-cell space-x-2"] $ span_ "Street" >> i_ [class_ "fa-regular fa-angles-up-down"] "" 
                tbody_ do
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
                  tableRow1
            -- Bottom Buttons
            bottomButtons


-- The last row of buttons: Approve Data, Take last value, Delete Value
bottomButtons :: Html ()
bottomButtons =
  div_ [c "sticky bottom-0 bg-white w-full p-4 justify-center items-center gap-1.5 inline-flex"] do
    neutralActionButton "fa-regular fa-circle-check" "Approve Data"
    neutralActionButton "fa-regular fa-rotate-reverse" "Take last value"
    neutralActionButton "fa-regular fa-trash-can" "Delete Value"
  where
    neutralActionButton :: Text -> Text -> Html ()
    neutralActionButton ic txt =
      button_ [c "px-3.5 py-2.5 rounded-md border border-[#d5d6d9] justify-center items-center gap-2 flex bg-neutral-50 text-neutral-400 group-has-[.contactCheckbox:checked]/pg:!bg-white group-has-[.contactCheckbox:checked]/pg:!text-[#414651]"] do
        i_ [class_ ic] ""
        span_ [c "text-md font-semibold"] (toHtml txt)


tableRow1 :: Html ()
tableRow1 = do
  tr_ do
    th_ [class_ "text-center"] $ input_ [type_ "checkbox", class_ "checkbox rounded-lg contactCheckbox"]
    th_ [class_ "space-y-1"] do
      strong_ [class_ "text-[#181d27] text-sm font-medium"] "Prof. Nicole Doe"
      span_ [class_ "text-[#535861] text-sm font-normal block"] "#ID24421234"
    td_ [class_ "hidden group-has-[.colBirthday:checked]/pg:table-cell"] "1.01.1999"
    td_ [class_ "hidden group-has-[.correction_option[value='birthdayCor']:checked]/pg:table-cell"] "1.01.1999"
    td_ [class_ "hidden group-has-[.correction_option[value='birthdayCor']:checked]/pg:table-cell"] "1.01.1999"
    td_ [class_ "hidden group-has-[.colPhone:checked]/pg:table-cell"] "+49123456789"
    td_ [class_ "hidden group-has-[.correction_option[value='phoneCor']:checked]/pg:table-cell"] "+49123456789"
    td_ [class_ "hidden group-has-[.correction_option[value='phoneCor']:checked]/pg:table-cell"] "+49123456789"
    td_ [class_ "hidden group-has-[.colEmail:checked]/pg:table-cell"] "fake@abc.com"
    td_ [class_ "hidden group-has-[.correction_option[value='emailCor']:checked]/pg:table-cell"] "fake@abc.com"
    td_ [class_ "hidden group-has-[.correction_option[value='emailCor']:checked]/pg:table-cell"] "fake@abc.com"
    td_ [class_ "hidden group-has-[.colStreet:checked]/pg:table-cell"] "Gruntaler str."
