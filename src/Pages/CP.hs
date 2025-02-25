module Pages.CP where

import Lucid
import Lucid.Base (TermRaw (termRaw))
import Lucid.Hyperscript
import NeatInterpolation
import Relude
import Relude.Unsafe
import System.Types


pageH :: ATBaseCtx (Html ())
pageH = pure page


c :: Text -> Attribute
c = class_


-- A small circular icon (like the avatar/status in the sidebar)
userStatusIcon :: Html ()
userStatusIcon = div_ [c "w-10 h-10 relative rounded-full"] $ do
  div_ [c "w-10 h-10 left-0 top-0 absolute rounded-full border border-black/10"] (return ())
  div_ [c "w-2.5 h-2.5 left-[30px] top-[30px] absolute bg-[#17b169] rounded-full border border-white"] (return ())


data Contact = Contact
  { title :: Text
  , firstName :: Text
  , lastName :: Text
  , nativeId :: Text
  , birthday :: [Text]
  , phone :: [Text]
  , email :: [Text]
  , street :: [Text]
  , houseNo :: [Text]
  , zip :: [Text]
  , city :: [Text]
  , addition :: [Text]
  }


columns :: [(Text, Text, (Contact -> [Text]))]
columns =
  [ ("Birthday", "colBirthday", (.birthday))
  , ("Phone", "colPhone", (.phone))
  , ("Email", "colEmail", (.email))
  , ("Street", "colStreet", (.street))
  , ("H-Nr", "colHouseNo", (.houseNo))
  , ("Zip", "colZip", (.zip))
  , ("City", "colCity", (.city))
  , ("Addition", "colAddition", (.addition))
  ]


forceClasses_ :: Html ()
forceClasses_ = do
  span_ [class_ "colBirthday colPhone colEmail colStreet colHouseNo colZip colCity colAddition "] ""
  span_ [class_ "group-has-[.colBirthday:checked]/pg:table-cell group-has-[.colPhone:checked]/pg:table-cell group-has-[.colEmail:checked]/pg:table-cell group-has-[.colStreet:checked]/pg:table-cell group-has-[.colHouseNo:checked]/pg:table-cell group-has-[.colZip:checked]/pg:table-cell group-has-[.colCity:checked]/pg:table-cell group-has-[.colAddition:checked]/pg:table-cell "] ""
  span_ [class_ "group-has-[.fix_option[value='colBirthday']:checked]/pg:table-cell group-has-[.fix_option[value='colPhone']:checked]/pg:table-cell group-has-[.fix_option[value='colEmail']:checked]/pg:table-cell"] ""
  span_ [class_ "group-has-[.fix_option[value='colStreet']:checked]/pg:table-cell group-has-[.fix_option[value='colHouseNo']:checked]/pg:table-cell group-has-[.fix_option[value='colZip']:checked]/pg:table-cell group-has-[.fix_option[value='colCity']:checked]/pg:table-cell"] ""
  span_ [class_ "group-has-[.fix_option[value='colAddition']:checked]/pg:table-cell "] ""

  span_ [class_ "group-has-[.fix_option[value='colBirthday']:checked]/pg:flex group-has-[.fix_option[value='colPhone']:checked]/pg:flex group-has-[.fix_option[value='colEmail']:checked]/pg:flex"] ""
  span_ [class_ "group-has-[.fix_option[value='colStreet']:checked]/pg:flex group-has-[.fix_option[value='colHouseNo']:checked]/pg:flex group-has-[.fix_option[value='colZip']:checked]/pg:flex group-has-[.fix_option[value='colCity']:checked]/pg:flex"] ""
  span_ [class_ "group-has-[.fix_option[value='colAddition']:checked]/pg:flex"] ""

  span_ [class_ "group-has-[.fix_option[value='colBirthday']:checked]/pg:opacity-100 group-has-[.fix_option[value='colPhone']:checked]/pg:opacity-100 group-has-[.fix_option[value='colEmail']:checked]/pg:opacity-100"] ""
  span_ [class_ "group-has-[.fix_option[value='colStreet']:checked]/pg:opacity-100 group-has-[.fix_option[value='colHouseNo']:checked]/pg:opacity-100 group-has-[.fix_option[value='colZip']:checked]/pg:opacity-100 group-has-[.fix_option[value='colCity']:checked]/pg:opacity-100"] ""
  span_ [class_ "group-has-[.fix_option[value='colAddition']:checked]/pg:opacity-100"] ""


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
                $ ""

            -- Bottom icon section
            div_ [c "self-stretch px-3 pb-5 flex-col justify-start items-center gap-4 flex"] $ do
              div_ [c "flex-col justify-start items-start gap-0.5 flex"]
                $ ""
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
                select_ [class_ "select select-sm h-auto px-3.5 py-1.5 leading-normal bg-white rounded-lg shadow shadow-inner border !border-[#d5d6d9] rounded-lg min-w-52 fix"] do
                  option_ [value_ ""] "Select Field"
                  forM_ columns \(title, colId, _) -> option_ [value_ colId] $ toHtml $ title <> " Correction"

                div_ [class_ "flex gap-3"] do
                  button_ [class_ "flex gap-2 items-center px-3.5 py-1.5 bg-white rounded-lg shadow shadow-inner border border-[#d5d6d9]"] do
                    i_ [class_ "fa-regular fa-calendar"] ""
                    "Nov 10, 2024 – Nov 31, 2024"

                  div_ [class_ "dropdown dropdown-end"] do
                    div_ [tabindex_ "0", role_ "button", class_ "flex gap-2 items-center px-3.5 py-1.5 bg-white rounded-lg shadow shadow-inner border border-[#d5d6d9] "] do
                      i_ [class_ "fa-solid fa-bars-filter"] ""
                      "Filters"
                    div_ [tabindex_ "0", class_ "dropdown-content flex flex-col bg-white shadow w-64 rounded-xl p-3 [&>*]:flex [&>*]:justify-between [&>*]:w-full  [&>*]:p-3"] do
                      label_ [] $ span_ "Approved Data" >> input_ [type_ "checkbox", checked_, name_ "colApprovedD", class_ "checkbox colApprovedD"]
                      label_ [] $ span_ "Incorrect Data" >> input_ [type_ "checkbox", checked_, name_ "colIncorrectD", class_ "checkbox colIncorrectD"]
                      label_ [] $ span_ "No Error Data" >> input_ [type_ "checkbox", checked_, name_ "colNoErrD", class_ "checkbox colNoErrD"]

                  div_ [class_ "dropdown dropdown-end"] do
                    div_ [tabindex_ "0", role_ "button", class_ "flex gap-2 items-center px-3.5 py-1.5 bg-white rounded-lg shadow shadow-inner border border-[#d5d6d9] "] do
                      i_ [class_ "fa-regular fa-columns-3"] "" >> "Columns"
                    div_ [tabindex_ "0", class_ "dropdown-content flex flex-col bg-white shadow w-64 rounded-xl p-3 [&>*]:flex [&>*]:justify-between [&>*]:w-full  [&>*]:p-3"] do
                      forM_ columns \(title, colId, _) -> label_ [] do
                        span_ (toHtml title)
                        input_ [type_ "checkbox", checked_, name_ colId, class_ $ "checkbox " <> colId]

              div_ [class_ "rounded-xl border w-full grow"] $ div_ [class_ "h-full w-full"] do
                table_ [id_ "correctionTable", class_ "table table-pin-cols table-pin-rows text-[#535861] rounded-xl "] do
                  colgroup_ do
                    col_ []
                    forM_ columns \(_, colId, _) -> col_ [class_ $ "min-w-48 "]
                  thead_ do
                    tr_ [class_ "rounded-lg [&>*]:top-[3.8rem] text-[#717680] font-semibold"] do
                      th_ [class_ " w-6 text-center"] $ input_ [type_ "checkbox", class_ "checkbox !border-[#D5D7DA] rounded-lg", [__|  on click set .contactCheckbox.checked to my.checked |]]
                      th_ [class_ "space-x-2 group"] do
                        span_ "Contact"
                        span_ [class_ "sortIcon absolute right-2 top-1/2 -translate-y-1/2 opacity-0 group-hover:opacity-100 transition-opacity"] "↕"
                      -- i_ [class_ "fa-regular fa-angles-up-down"] ""
                      forM_ columns \(title, colId, _) -> columnTitleGroup_ colId title
                  tbody_ $ forM_ contacts tableRow1
            -- Bottom Buttons
            bottomButtons
  script_
    [text|

    const getCellValue = (cell) => {
      const input = cell.querySelector('input');
      return input ? input.value.trim() : cell.textContent.trim();
    };
    document.addEventListener('DOMContentLoaded', () => {
        const table = document.getElementById('correctionTable');
        let currentSort = { col: null, asc: true };

        // Add click handlers to headers
        table.querySelectorAll('thead th').forEach((th, i) => {
            th.addEventListener('click', () => {
                console.log("th click", th)
                const arrow = th.querySelector('.sortIcon');
                table.querySelectorAll('thead th .sortIcon').forEach(span => {
                    span.textContent = '↕';
                    span.classList.add('opacity-0');
                });
                
                // Update sort direction
                currentSort = {col: i, asc: currentSort.col === i ? !currentSort.asc : true};
                
                if (arrow) {
                    arrow.textContent = currentSort.asc ? '↑' : '↓';
                    arrow.classList.remove('opacity-0');
                }

                const tbody = table.querySelector('tbody');

                // Sort rows
                const sortedRows = [...tbody.querySelectorAll('tr')]
                    .sort((a, b) => {
                        const aVal = getCellValue(a.cells[i]);
                        const bVal = getCellValue(b.cells[i]);
                        
                        // Try number sort
                        const aNum = parseFloat(aVal);
                        const bNum = parseFloat(bVal);
                        if (!isNaN(aNum) && !isNaN(bNum)) {
                            return currentSort.asc ? aNum - bNum : bNum - aNum;
                        }
                        
                        // Try date sort
                        const aDate = new Date(aVal);
                        const bDate = new Date(bVal);
                        if (aDate.toString() !== 'Invalid Date' && bDate.toString() !== 'Invalid Date') {
                            return currentSort.asc ? aDate - bDate : bDate - aDate;
                        }
                        
                        // Default to string sort
                        return currentSort.asc 
                            ? aVal.localeCompare(bVal) 
                            : bVal.localeCompare(aVal);
                    });
                tbody.innerHTML = '';
                sortedRows.forEach(tr => tbody.appendChild(tr));
            });
        });
    });
    |]


columnTitleGroup_ :: Text -> Text -> Html ()
columnTitleGroup_ colId title = do
  th_ [class_ $ "group px-6 hidden opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 group-has-[." <> colId <> ":checked]/pg:table-cell space-x-2"] do
    span_ $ toHtml title
    span_ [class_ "sortIcon absolute right-2 top-1/2 -translate-y-1/2 opacity-0 group-hover:opacity-100 transition-opacity"] "↕"
  -- i_ [class_ "fa-regular fa-angles-up-down"] ""
  th_ [class_ $ "px-6 hidden opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:table-cell space-x-2"] do
    span_ (toHtml $ "Latest " <> title)
    div_ [class_ "inline-block"] $ a_ [termRaw "data-tip" "Latest version of the data as inputed by user", class_ "tooltip tooltip-left"] $ i_ [class_ "fa-regular fa-circle-question text-[#a3a7ae]"] ""
  th_ [class_ $ "px-6 hidden opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:table-cell space-x-2"] do
    span_ (toHtml $ "Last " <> title)
    a_ [termRaw "data-tip" "Last version of data, if user performed multiple updates", class_ "tooltip tooltip-left"] $ i_ [class_ "fa-regular fa-circle-question text-[#a3a7ae]"] ""
  th_ [class_ $ "px-6 hidden opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:table-cell space-x-2"] do
    span_ (toHtml $ "Initial " <> title)
    a_ [termRaw "data-tip" "Original version of data before update", class_ "tooltip tooltip-left"] $ i_ [class_ "fa-regular fa-circle-question text-[#a3a7ae]"] ""


columnGroup_ :: Text -> Text -> Text -> Text -> Text -> Text -> Html ()
columnGroup_ nativeId colId value latestValue lastValue initialValue = do
  td_ [class_ $ "group/cg hidden p-0 opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 group-has-[." <> colId <> ":checked]/pg:table-cell whitespace-nowrap"] $ div_ [class_ "flex items-center gap-2"] do
    input_ [type_ "text", value_ value, class_ $ "inputValue_" <> nativeId <> colId <> " columnValue h-full w-full py-6 px-6 group-has-[.approved:checked]/cg:bg-[#ecfcf2] group-has-[.approved:checked]/cg:bg-[#ecfcf2]"]
    div_ [class_ $ "gap-2 hidden group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:flex"] do
      label_ [class_ "p-1 tooltip cursor-pointer", termRaw "data-tip" "Approve"] do
        input_ [class_ "hidden approved", type_ "checkbox", name_ $ "approved_" <> nativeId <> colId, id_ $ "approved_" <> nativeId <> colId]
        i_ [class_ "fa-regular fa-circle-check text-[#a3a7ae]"] ""
      label_
        [ class_ "p-1 cursor-pointer"
        , termRaw "data-tip" "Delete"
        , [__|on click  set (previous <input.columnValue/>).value to '' |]
        ]
        do
          input_ [class_ "hidden deleted", type_ "checkbox", name_ $ "deleted_" <> nativeId <> colId, id_ $ "deleted_" <> nativeId <> colId]
          i_ [class_ "fa-regular fa-trash-can text-[#a3a7ae]"] ""
  td_ [class_ $ "latestValue_" <> nativeId <> colId <> " group opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 hover:bg-neutral-50  hidden group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:table-cell px-6 space-x-2 whitespace-nowrap"] do
    span_ $ toHtml latestValue
    label_ [class_ "p-1 tooltip hidden group-hover:inline-block", termRaw "data-tip" "Approve"] $ i_ [class_ "fa-regular fa-copy text-[#a3a7ae]"] ""
  td_ [class_ $ "lastValue_" <> nativeId <> colId <> " group opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 hover:bg-neutral-50 hidden group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:table-cell px-6 space-x-2 whitespace-nowrap"] do
    span_ $ toHtml lastValue
    label_
      [ class_ "p-1 tooltip hidden group-hover:inline-block"
      , termRaw "data-tip" "Approve"
      , [__|  on click if 'clipboard' in window.navigator then
                    call navigator.clipboard.writeText((previous <span />)'s innerText)
                    send successToast(value:['Value copied to the Clipboard']) to <body/>
                    halt
              end|]
      ]
      $ i_ [class_ "fa-regular fa-copy text-[#a3a7ae]"] ""
  td_ [class_ $ "initialValue_" <> nativeId <> colId <> " group opacity-20 group-has-[.fix_option[value='']:checked]/pg:opacity-100 group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:opacity-100 hover:bg-neutral-50 hidden group-has-[.fix_option[value='" <> colId <> "']:checked]/pg:table-cell px-6 space-x-2 whitespace-nowrap"] do
    span_ $ toHtml initialValue
    label_ [class_ "p-1 tooltip hidden group-hover:inline-block", termRaw "data-tip" "Copy"] do
      i_ [class_ "fa-regular fa-copy text-[#a3a7ae]"] ""


-- The last row of buttons: Approve Data, Take last value, Delete Value
bottomButtons :: Html ()
bottomButtons =
  div_ [c "sticky bottom-0 bg-white w-full p-4 justify-center items-center gap-1.5 inline-flex"] do
    script_
      [text|
    const forEachChecked = callback => {
      const col = document.querySelector('.fix')?.value;
      [...document.querySelectorAll('.contactCheckbox:checked')]
        .map(({ id }) => id.slice(6)).forEach(id => callback(id, col));
    };

    const approveDataList = () =>
      forEachChecked((id, col) =>
        col
          ? (document.getElementById(`approved_$${id}$${col}`).checked = true)
          : document
              .querySelectorAll(`input[id^="approved_$${id}"]`)
              .forEach(cb => (cb.checked = true))
      );

    const takeLastValue = () =>
      forEachChecked((id, col) => {
        if (col) {
          const lastValue = document.querySelector(`.lastValue_$${id}$${col} > span`)?.innerText;
          document.querySelector(`.inputValue_$${id}$${col}`).value = lastValue;
        }
      });

    const deleteRowValue = () =>
      forEachChecked((id, col) => col && (document.querySelector(`.inputValue_$${id}$${col}`).value = ''));
    |]
    button_ [c "px-3.5 py-2.5 rounded-md border border-[#d5d6d9] justify-center items-center gap-2 flex bg-neutral-50 text-neutral-400 group-has-[.contactCheckbox:checked]/pg:!bg-white group-has-[.contactCheckbox:checked]/pg:!text-[#414651]", onclick_ "approveDataList()"] do
      i_ [class_ "fa-regular fa-circle-check"] ""
      span_ [c "text-md font-semibold"] "Approve Data"
    button_ [c "px-3.5 py-2.5 rounded-md border border-[#d5d6d9] justify-center items-center gap-2 flex bg-neutral-50 text-neutral-400 group-has-[.contactCheckbox:checked]/pg:!bg-white group-has-[.contactCheckbox:checked]/pg:!text-[#414651]", onclick_ "takeLastValue()"] do
      i_ [class_ "fa-regular fa-rotate-reverse"] ""
      span_ [c "text-md font-semibold"] "Take last value"
    button_ [c "px-3.5 py-2.5 rounded-md border border-[#d5d6d9] justify-center items-center gap-2 flex bg-neutral-50 text-neutral-400 group-has-[.contactCheckbox:checked]/pg:!bg-white group-has-[.contactCheckbox:checked]/pg:!text-[#414651]", onclick_ "deleteRowValue()"] do
      i_ [class_ "fa-regular fa-trash-can"] ""
      span_ [c "text-md font-semibold"] "Delete Value"


tableRow1 :: Contact -> Html ()
tableRow1 cc =
  tr_ do
    th_ [class_ "text-center"] do
      input_ [type_ "checkbox", class_ "checkbox !border-[#D5D7DA] rounded-lg contactCheckbox", id_ $ "check_" <> cc.nativeId]
    th_ [class_ "space-y-1"] do
      strong_ [class_ "text-[#181d27] text-sm font-medium"] $ toHtml $ cc.title <> "." <> cc.firstName <> " " <> cc.lastName
      span_ [class_ "text-[#535861] text-sm font-normal block"] $ toHtml $ "#" <> cc.nativeId
    forM_ columns \(title, colId, fn) -> columnGroup_ cc.nativeId colId ((fn cc) !! 0) ((fn cc) !! 1) ((fn cc) !! 2) ((fn cc) !! 3)


contacts :: [Contact]
contacts =
  [ Contact
      { title = "Mr."
      , firstName = "John"
      , lastName = "Doe"
      , nativeId = "NID10014"
      , birthday = ["1980-05-15", "15/05/1980", "May 15, 1980", "15 May 1980"]
      , phone = ["+1-202-555-0110", "+1 202 555 0111", "+1.202.555.0112", "+12025550113"]
      , email = ["john.doe@example.com", "j.doe@example.com", "john.d@example.com", "contact@johndoe.com"]
      , street = ["Main Street", "Main St.", "Main Street", "Main"]
      , houseNo = ["101", "101A", "101", "101"]
      , zip = ["12345", "12345", "12345", "12345"]
      , city = ["Springfield", "Springfield", "Springfield", "Springfield"]
      , addition = ["Apt 1", "Suite 1", "Building A", "Block 1"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Jane"
      , lastName = "Smith"
      , nativeId = "NID10023"
      , birthday = ["1975-08-22", "22/08/1975", "August 22, 1975", "22 Aug 1975"]
      , phone = ["+1-303-555-0120", "+1 303 555 0121", "+1.303.555.0122", "+13035550123"]
      , email = ["jane.smith@example.com", "j.smith@example.com", "jane.s@example.com", "contact@janesmith.com"]
      , street = ["Oak Avenue", "Oak Ave.", "Oak Avenue", "Oak"]
      , houseNo = ["202", "202B", "202", "202"]
      , zip = ["54321", "54321", "54321", "54321"]
      , city = ["Rivertown", "Rivertown", "Rivertown", "Rivertown"]
      , addition = ["Unit 2", "Floor 2", "Suite 2", "Room 2"]
      }
  , Contact
      { title = "Dr."
      , firstName = "Alice"
      , lastName = "Johnson"
      , nativeId = "NID10031"
      , birthday = ["1985-12-03", "03/12/1985", "December 3, 1985", "3 Dec 1985"]
      , phone = ["+1-404-555-0130", "+1 404 555 0131", "+1.404.555.0132", "+14045550133"]
      , email = ["alice.johnson@example.com", "a.johnson@example.com", "alicej@example.com", "contact@alicejohnson.com"]
      , street = ["Pine Road", "Pine Rd.", "Pine Road", "Pine"]
      , houseNo = ["303", "303C", "303", "303"]
      , zip = ["67890", "67890", "67890", "67890"]
      , city = ["Lakeside", "Lakeside", "Lakeside", "Lakeside"]
      , addition = ["Suite 3", "Apt 3", "Building 3", "Block 3"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Robert"
      , lastName = "Brown"
      , nativeId = "NID10042"
      , birthday = ["1992-03-10", "10/03/1992", "March 10, 1992", "10 Mar 1992"]
      , phone = ["+1-505-555-0140", "+1 505 555 0141", "+1.505.555.0142", "+15055550143"]
      , email = ["robert.brown@example.com", "r.brown@example.com", "robb@example.com", "contact@robertbrown.com"]
      , street = ["Cedar Lane", "Cedar Ln.", "Cedar Lane", "Cedar"]
      , houseNo = ["404", "404D", "404", "404"]
      , zip = ["11223", "11223", "11223", "11223"]
      , city = ["Hillview", "Hillview", "Hillview", "Hillview"]
      , addition = ["Floor 4", "Unit 4", "Suite 4", "Room 4"]
      }
  , Contact
      { title = "Mrs."
      , firstName = "Emily"
      , lastName = "Davis"
      , nativeId = "NID10051"
      , birthday = ["1978-11-30", "30/11/1978", "November 30, 1978", "30 Nov 1978"]
      , phone = ["+1-606-555-0150", "+1 606 555 0151", "+1.606.555.0152", "+16065550153"]
      , email = ["emily.davis@example.com", "e.davis@example.com", "emilyd@example.com", "contact@emilydavis.com"]
      , street = ["Birch Street", "Birch St.", "Birch Street", "Birch"]
      , houseNo = ["505", "505E", "505", "505"]
      , zip = ["33445", "33445", "33445", "33445"]
      , city = ["Mapleton", "Mapleton", "Mapleton", "Mapleton"]
      , addition = ["Apt 5", "Suite 5", "Block 5", "Floor 5"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Michael"
      , lastName = "Miller"
      , nativeId = "NID10062"
      , birthday = ["1988-07-07", "07/07/1988", "July 7, 1988", "7 Jul 1988"]
      , phone = ["+1-707-555-0160", "+1 707 555 0161", "+1.707.555.0162", "+17075550163"]
      , email = ["michael.miller@example.com", "m.miller@example.com", "mike@example.com", "contact@michaelmiller.com"]
      , street = ["Willow Avenue", "Willow Ave.", "Willow Avenue", "Willow"]
      , houseNo = ["606", "606F", "606", "606"]
      , zip = ["55667", "55667", "55667", "55667"]
      , city = ["Oakwood", "Oakwood", "Oakwood", "Oakwood"]
      , addition = ["Unit 6", "Floor 6", "Suite 6", "Room 6"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Sarah"
      , lastName = "Wilson"
      , nativeId = "NID10073"
      , birthday = ["1990-09-25", "25/09/1990", "September 25, 1990", "25 Sep 1990"]
      , phone = ["+1-808-555-0170", "+1 808 555 0171", "+1.808.555.0172", "+18085550173"]
      , email = ["sarah.wilson@example.com", "s.wilson@example.com", "sarahw@example.com", "contact@sarahwilson.com"]
      , street = ["Maple Street", "Maple St.", "Maple Street", "Maple"]
      , houseNo = ["707", "707G", "707", "707"]
      , zip = ["77889", "77889", "77889", "77889"]
      , city = ["Fairview", "Fairview", "Fairview", "Fairview"]
      , addition = ["Apt 7", "Unit 7", "Suite 7", "Block 7"]
      }
  , Contact
      { title = "Mr."
      , firstName = "David"
      , lastName = "Moore"
      , nativeId = "NID10084"
      , birthday = ["1982-02-28", "28/02/1982", "February 28, 1982", "28 Feb 1982"]
      , phone = ["+1-909-555-0180", "+1 909 555-0181", "+1.909.555-0182", "+19095550183"]
      , email = ["david.moore@example.com", "d.moore@example.com", "davmoore@example.com", "contact@davidmoore.com"]
      , street = ["Cypress Road", "Cypress Rd.", "Cypress Road", "Cypress"]
      , houseNo = ["808", "808H", "808", "808"]
      , zip = ["99001", "99001", "99001", "99001"]
      , city = ["Lakeside", "Lakeside", "Lakeside", "Lakeside"]
      , addition = ["Floor 8", "Unit 8", "Suite 8", "Room 8"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Laura"
      , lastName = "Taylor"
      , nativeId = "NID10095"
      , birthday = ["1995-04-18", "18/04/1995", "April 18, 1995", "18 Apr 1995"]
      , phone = ["+1-101-555-0190", "+1 101 555-0191", "+1.101.555.0192", "+11015550193"]
      , email = ["laura.taylor@example.com", "l.taylor@example.com", "laurat@example.com", "contact@laurataylor.com"]
      , street = ["Fir Street", "Fir St.", "Fir Street", "Fir"]
      , houseNo = ["909", "909I", "909", "909"]
      , zip = ["22334", "22334", "22334", "22334"]
      , city = ["Greenville", "Greenville", "Greenville", "Greenville"]
      , addition = ["Apt 9", "Suite 9", "Block 9", "Floor 9"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Daniel"
      , lastName = "Anderson"
      , nativeId = "NID10101"
      , birthday = ["1987-06-12", "12/06/1987", "June 12, 1987", "12 Jun 1987"]
      , phone = ["+1-202-555-0200", "+1 202 555-0201", "+1.202.555.0202", "+12025550203"]
      , email = ["daniel.anderson@example.com", "d.anderson@example.com", "daniela@example.com", "contact@danielanderson.com"]
      , street = ["Cherry Lane", "Cherry Ln.", "Cherry Lane", "Cherry"]
      , houseNo = ["1001", "1001J", "1001", "1001"]
      , zip = ["33456", "33456", "33456", "33456"]
      , city = ["Centerville", "Centerville", "Centerville", "Centerville"]
      , addition = ["Unit 10", "Floor 10", "Suite 10", "Room 10"]
      }
  , Contact
      { title = "Mr."
      , firstName = "John"
      , lastName = "Doe"
      , nativeId = "NID10012"
      , birthday = ["1980-05-15", "15/05/1980", "May 15, 1980", "15 May 1980"]
      , phone = ["+1-202-555-0110", "+1 202 555 0111", "+1.202.555.0112", "+12025550113"]
      , email = ["john.doe@example.com", "j.doe@example.com", "john.d@example.com", "contact@johndoe.com"]
      , street = ["Main Street", "Main St.", "Main Street", "Main"]
      , houseNo = ["101", "101A", "101", "101"]
      , zip = ["12345", "12345", "12345", "12345"]
      , city = ["Springfield", "Springfield", "Springfield", "Springfield"]
      , addition = ["Apt 1", "Suite 1", "Building A", "Block 1"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Jane"
      , lastName = "Smith"
      , nativeId = "NID1002"
      , birthday = ["1975-08-22", "22/08/1975", "August 22, 1975", "22 Aug 1975"]
      , phone = ["+1-303-555-0120", "+1 303 555 0121", "+1.303.555.0122", "+13035550123"]
      , email = ["jane.smith@example.com", "j.smith@example.com", "jane.s@example.com", "contact@janesmith.com"]
      , street = ["Oak Avenue", "Oak Ave.", "Oak Avenue", "Oak"]
      , houseNo = ["202", "202B", "202", "202"]
      , zip = ["54321", "54321", "54321", "54321"]
      , city = ["Rivertown", "Rivertown", "Rivertown", "Rivertown"]
      , addition = ["Unit 2", "Floor 2", "Suite 2", "Room 2"]
      }
  , Contact
      { title = "Dr."
      , firstName = "Alice"
      , lastName = "Johnson"
      , nativeId = "NID1003"
      , birthday = ["1985-12-03", "03/12/1985", "December 3, 1985", "3 Dec 1985"]
      , phone = ["+1-404-555-0130", "+1 404 555 0131", "+1.404.555.0132", "+14045550133"]
      , email = ["alice.johnson@example.com", "a.johnson@example.com", "alicej@example.com", "contact@alicejohnson.com"]
      , street = ["Pine Road", "Pine Rd.", "Pine Road", "Pine"]
      , houseNo = ["303", "303C", "303", "303"]
      , zip = ["67890", "67890", "67890", "67890"]
      , city = ["Lakeside", "Lakeside", "Lakeside", "Lakeside"]
      , addition = ["Suite 3", "Apt 3", "Building 3", "Block 3"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Robert"
      , lastName = "Brown"
      , nativeId = "NID1004"
      , birthday = ["1992-03-10", "10/03/1992", "March 10, 1992", "10 Mar 1992"]
      , phone = ["+1-505-555-0140", "+1 505 555 0141", "+1.505.555-0142", "+15055550143"]
      , email = ["robert.brown@example.com", "r.brown@example.com", "robb@example.com", "contact@robertbrown.com"]
      , street = ["Cedar Lane", "Cedar Ln.", "Cedar Lane", "Cedar"]
      , houseNo = ["404", "404D", "404", "404"]
      , zip = ["11223", "11223", "11223", "11223"]
      , city = ["Hillview", "Hillview", "Hillview", "Hillview"]
      , addition = ["Floor 4", "Unit 4", "Suite 4", "Room 4"]
      }
  , Contact
      { title = "Mrs."
      , firstName = "Emily"
      , lastName = "Davis"
      , nativeId = "NID1005"
      , birthday = ["1978-11-30", "30/11/1978", "November 30, 1978", "30 Nov 1978"]
      , phone = ["+1-606-555-0150", "+1 606 555 0151", "+1.606.555-0152", "+16065550153"]
      , email = ["emily.davis@example.com", "e.davis@example.com", "emilyd@example.com", "contact@emilydavis.com"]
      , street = ["Birch Street", "Birch St.", "Birch Street", "Birch"]
      , houseNo = ["505", "505E", "505", "505"]
      , zip = ["33445", "33445", "33445", "33445"]
      , city = ["Mapleton", "Mapleton", "Mapleton", "Mapleton"]
      , addition = ["Apt 5", "Suite 5", "Block 5", "Floor 5"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Michael"
      , lastName = "Miller"
      , nativeId = "NID1006"
      , birthday = ["1988-07-07", "07/07/1988", "July 7, 1988", "7 Jul 1988"]
      , phone = ["+1-707-555-0160", "+1 707 555 0161", "+1.707.555-0162", "+17075550163"]
      , email = ["michael.miller@example.com", "m.miller@example.com", "mike@example.com", "contact@michaelmiller.com"]
      , street = ["Willow Avenue", "Willow Ave.", "Willow Avenue", "Willow"]
      , houseNo = ["606", "606F", "606", "606"]
      , zip = ["55667", "55667", "55667", "55667"]
      , city = ["Oakwood", "Oakwood", "Oakwood", "Oakwood"]
      , addition = ["Unit 6", "Floor 6", "Suite 6", "Room 6"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Sarah"
      , lastName = "Wilson"
      , nativeId = "NID1007"
      , birthday = ["1990-09-25", "25/09/1990", "September 25, 1990", "25 Sep 1990"]
      , phone = ["+1-808-555-0170", "+1 808 555 0171", "+1.808.555-0172", "+18085550173"]
      , email = ["sarah.wilson@example.com", "s.wilson@example.com", "sarahw@example.com", "contact@sarahwilson.com"]
      , street = ["Maple Street", "Maple St.", "Maple Street", "Maple"]
      , houseNo = ["707", "707G", "707", "707"]
      , zip = ["77889", "77889", "77889", "77889"]
      , city = ["Fairview", "Fairview", "Fairview", "Fairview"]
      , addition = ["Apt 7", "Unit 7", "Suite 7", "Block 7"]
      }
  , Contact
      { title = "Mr."
      , firstName = "David"
      , lastName = "Moore"
      , nativeId = "NID1008"
      , birthday = ["1982-02-28", "28/02/1982", "February 28, 1982", "28 Feb 1982"]
      , phone = ["+1-909-555-0180", "+1 909 555-0181", "+1.909.555-0182", "+19095550183"]
      , email = ["david.moore@example.com", "d.moore@example.com", "davmoore@example.com", "contact@davidmoore.com"]
      , street = ["Cypress Road", "Cypress Rd.", "Cypress Road", "Cypress"]
      , houseNo = ["808", "808H", "808", "808"]
      , zip = ["99001", "99001", "99001", "99001"]
      , city = ["Lakeside", "Lakeside", "Lakeside", "Lakeside"]
      , addition = ["Floor 8", "Unit 8", "Suite 8", "Room 8"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Laura"
      , lastName = "Taylor"
      , nativeId = "NID1009"
      , birthday = ["1995-04-18", "18/04/1995", "April 18, 1995", "18 Apr 1995"]
      , phone = ["+1-101-555-0190", "+1 101 555-0191", "+1.101.555-0192", "+11015550193"]
      , email = ["laura.taylor@example.com", "l.taylor@example.com", "laurat@example.com", "contact@laurataylor.com"]
      , street = ["Fir Street", "Fir St.", "Fir Street", "Fir"]
      , houseNo = ["909", "909I", "909", "909"]
      , zip = ["22334", "22334", "22334", "22334"]
      , city = ["Greenville", "Greenville", "Greenville", "Greenville"]
      , addition = ["Apt 9", "Suite 9", "Block 9", "Floor 9"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Daniel"
      , lastName = "Anderson"
      , nativeId = "NID1010"
      , birthday = ["1987-06-12", "12/06/1987", "June 12, 1987", "12 Jun 1987"]
      , phone = ["+1-202-555-0200", "+1 202 555-0201", "+1.202.555-0202", "+12025550203"]
      , email = ["daniel.anderson@example.com", "d.anderson@example.com", "daniela@example.com", "contact@danielanderson.com"]
      , street = ["Cherry Lane", "Cherry Ln.", "Cherry Lane", "Cherry"]
      , houseNo = ["1001", "1001J", "1001", "1001"]
      , zip = ["33456", "33456", "33456", "33456"]
      , city = ["Centerville", "Centerville", "Centerville", "Centerville"]
      , addition = ["Unit 10", "Floor 10", "Suite 10", "Room 10"]
      }
  , -- Additional 10 contacts
    Contact
      { title = "Dr."
      , firstName = "George"
      , lastName = "King"
      , nativeId = "NID1011"
      , birthday = ["1970-01-15", "15/01/1970", "January 15, 1970", "15 Jan 1970"]
      , phone = ["+1-303-555-0210", "+1 303 555 0211", "+1.303.555-0212", "+13035550213"]
      , email = ["george.king@example.com", "g.king@example.com", "georgek@example.com", "contact@georgeking.com"]
      , street = ["Elm Street", "Elm St.", "Elm Street", "Elm"]
      , houseNo = ["111", "111A", "111", "111"]
      , zip = ["44556", "44556", "44556", "44556"]
      , city = ["Oldtown", "Oldtown", "Oldtown", "Oldtown"]
      , addition = ["Apt 11", "Suite 11", "Unit 11", "Block 11"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Olivia"
      , lastName = "Martin"
      , nativeId = "NID1012"
      , birthday = ["1982-04-22", "22/04/1982", "April 22, 1982", "22 Apr 1982"]
      , phone = ["+1-404-555-0220", "+1 404 555-0221", "+1.404.555-0222", "+14045550223"]
      , email = ["olivia.martin@example.com", "o.martin@example.com", "oliviam@example.com", "contact@oliviamartin.com"]
      , street = ["Sunset Boulevard", "Sunset Blvd.", "Sunset Boulevard", "Sunset"]
      , houseNo = ["222", "222B", "222", "222"]
      , zip = ["55667", "55667", "55667", "55667"]
      , city = ["Riverside", "Riverside", "Riverside", "Riverside"]
      , addition = ["Unit 12", "Floor 12", "Suite 12", "Room 12"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Steven"
      , lastName = "Clark"
      , nativeId = "NID1013"
      , birthday = ["1993-07-19", "19/07/1993", "July 19, 1993", "19 Jul 1993"]
      , phone = ["+1-505-555-0230", "+1 505 555-0231", "+1.505.555-0232", "+15055550233"]
      , email = ["steven.clark@example.com", "s.clark@example.com", "stevenc@example.com", "contact@stevenclark.com"]
      , street = ["Lakeside Drive", "Lakeside Dr.", "Lakeside Drive", "Lakeside"]
      , houseNo = ["333", "333C", "333", "333"]
      , zip = ["66778", "66778", "66778", "66778"]
      , city = ["Seaside", "Seaside", "Seaside", "Seaside"]
      , addition = ["Apt 13", "Suite 13", "Block 13", "Floor 13"]
      }
  , Contact
      { title = "Mrs."
      , firstName = "Isabella"
      , lastName = "White"
      , nativeId = "NID1014"
      , birthday = ["1985-10-05", "05/10/1985", "October 5, 1985", "5 Oct 1985"]
      , phone = ["+1-606-555-0240", "+1 606 555 0241", "+1.606.555-0242", "+16065550243"]
      , email = ["isabella.white@example.com", "i.white@example.com", "isabellaw@example.com", "contact@isabellawhite.com"]
      , street = ["Park Avenue", "Park Ave.", "Park Avenue", "Park"]
      , houseNo = ["444", "444D", "444", "444"]
      , zip = ["77889", "77889", "77889", "77889"]
      , city = ["Uptown", "Uptown", "Uptown", "Uptown"]
      , addition = ["Unit 14", "Floor 14", "Suite 14", "Room 14"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Thomas"
      , lastName = "Lee"
      , nativeId = "NID1015"
      , birthday = ["1979-12-11", "11/12/1979", "December 11, 1979", "11 Dec 1979"]
      , phone = ["+1-707-555-0250", "+1 707 555-0251", "+1.707.555-0252", "+17075550253"]
      , email = ["thomas.lee@example.com", "t.lee@example.com", "thomasl@example.com", "contact@thomaslee.com"]
      , street = ["River Road", "River Rd.", "River Road", "River"]
      , houseNo = ["555", "555E", "555", "555"]
      , zip = ["88990", "88990", "88990", "88990"]
      , city = ["Brookside", "Brookside", "Brookside", "Brookside"]
      , addition = ["Apt 15", "Suite 15", "Unit 15", "Block 15"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Sophia"
      , lastName = "Harris"
      , nativeId = "NID1016"
      , birthday = ["1991-03-29", "29/03/1991", "March 29, 1991", "29 Mar 1991"]
      , phone = ["+1-808-555-0260", "+1 808 555-0261", "+1.808.555-0262", "+18085550263"]
      , email = ["sophia.harris@example.com", "s.harris@example.com", "sophiah@example.com", "contact@sophiaharris.com"]
      , street = ["Clover Street", "Clover St.", "Clover Street", "Clover"]
      , houseNo = ["666", "666F", "666", "666"]
      , zip = ["99002", "99002", "99002", "99002"]
      , city = ["Downtown", "Downtown", "Downtown", "Downtown"]
      , addition = ["Unit 16", "Floor 16", "Suite 16", "Room 16"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Christopher"
      , lastName = "Martin"
      , nativeId = "NID1017"
      , birthday = ["1983-08-08", "08/08/1983", "August 8, 1983", "8 Aug 1983"]
      , phone = ["+1-909-555-0270", "+1 909 555-0271", "+1.909.555-0272", "+19095550273"]
      , email = ["christopher.martin@example.com", "c.martin@example.com", "chrism@example.com", "contact@christophermartin.com"]
      , street = ["Aspen Way", "Aspen Wy.", "Aspen Way", "Aspen"]
      , houseNo = ["777", "777G", "777", "777"]
      , zip = ["11234", "11234", "11234", "11234"]
      , city = ["Midtown", "Midtown", "Midtown", "Midtown"]
      , addition = ["Apt 17", "Suite 17", "Block 17", "Floor 17"]
      }
  , Contact
      { title = "Ms."
      , firstName = "Megan"
      , lastName = "Lewis"
      , nativeId = "NID1018"
      , birthday = ["1994-11-14", "14/11/1994", "November 14, 1994", "14 Nov 1994"]
      , phone = ["+1-101-555-0280", "+1 101 555-0281", "+1.101.555-0282", "+11015550283"]
      , email = ["megan.lewis@example.com", "m.lewis@example.com", "meganl@example.com", "contact@meganlewis.com"]
      , street = ["Forest Drive", "Forest Dr.", "Forest Drive", "Forest"]
      , houseNo = ["888", "888H", "888", "888"]
      , zip = ["33445", "33445", "33445", "33445"]
      , city = ["Elmwood", "Elmwood", "Elmwood", "Elmwood"]
      , addition = ["Unit 18", "Floor 18", "Suite 18", "Room 18"]
      }
  , Contact
      { title = "Mr."
      , firstName = "Andrew"
      , lastName = "Walker"
      , nativeId = "NID1019"
      , birthday = ["1986-02-02", "02/02/1986", "February 2, 1986", "2 Feb 1986"]
      , phone = ["+1-202-555-0290", "+1 202 555-0291", "+1.202.555-0292", "+12025550293"]
      , email = ["andrew.walker@example.com", "a.walker@example.com", "andreww@example.com", "contact@andrewwalker.com"]
      , street = ["King Street", "King St.", "King Street", "King"]
      , houseNo = ["999", "999J", "999", "999"]
      , zip = ["44556", "44556", "44556", "44556"]
      , city = ["Capitol", "Capitol", "Capitol", "Capitol"]
      , addition = ["Apt 19", "Suite 19", "Block 19", "Floor 19"]
      }
  , Contact
      { title = "Mrs."
      , firstName = "Victoria"
      , lastName = "Young"
      , nativeId = "NID1020"
      , birthday = ["1977-05-06", "06/05/1977", "May 6, 1977", "6 May 1977"]
      , phone = ["+1-303-555-0300", "+1 303 555-0301", "+1.303.555-0302", "+13035550303"]
      , email = ["victoria.young@example.com", "v.young@example.com", "victoriay@example.com", "contact@victoriayoung.com"]
      , street = ["Elmwood Avenue", "Elmwood Ave.", "Elmwood Avenue", "Elmwood"]
      , houseNo = ["1010", "1010K", "1010", "1010"]
      , zip = ["55667", "55667", "55667", "55667"]
      , city = ["Greenfield", "Greenfield", "Greenfield", "Greenfield"]
      , addition = ["Unit 20", "Floor 20", "Suite 20", "Room 20"]
      }
  ]
