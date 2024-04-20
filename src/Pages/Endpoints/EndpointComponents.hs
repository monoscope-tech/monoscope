module Pages.Endpoints.EndpointComponents (fieldTypeToDisplay, fieldCategoryToDisplay) where

import Lucid (Html, class_, span_)
import Models.Apis.Fields.Types qualified as Fields


fieldTypeToDisplay :: Fields.FieldTypes -> Html ()
fieldTypeToDisplay fieldType = case fieldType of
  Fields.FTUnknown -> span_ [class_ "px-2 rounded-xl bg-red-100 red-800 monospace"] "unknown"
  Fields.FTString -> span_ [class_ "px-2 rounded-xl bg-slate-100 slate-800 monospace"] "abc"
  Fields.FTNumber -> span_ [class_ "px-2 rounded-xl bg-blue-100 blue-800 monospace"] "123"
  Fields.FTBool -> span_ [class_ "px-2 rounded-xl bg-gray-100 black-800 monospace"] "bool"
  Fields.FTObject -> span_ [class_ "px-2 rounded-xl bg-orange-100 orange-800 monospace"] "{obj}"
  Fields.FTList -> span_ [class_ "px-2 rounded-xl bg-stone-100 stone-800 monospace"] "[list]"
  Fields.FTNull -> span_ [class_ "px-2 rounded-xl bg-red-100 red-800 monospace"] "null"


fieldCategoryToDisplay :: Fields.FieldCategoryEnum -> Html ()
fieldCategoryToDisplay fieldType = case fieldType of
  Fields.FCRequestBody -> span_ [class_ "px-2 rounded-xl bg-slate-100 slate-800 monospace"] "Request Body"
  Fields.FCQueryParam -> span_ [class_ "px-2 rounded-xl bg-blue-100 blue-800 monospace"] "Query Param"
  Fields.FCPathParam -> span_ [class_ "px-2 rounded-xl bg-gray-100 black-800 monospace"] "Path Param"
  Fields.FCRequestHeader -> span_ [class_ "px-2 rounded-xl bg-orange-100 orange-800 monospace"] "Request Header"
  Fields.FCResponseHeader -> span_ [class_ "px-2 rounded-xl bg-stone-100 stone-800 monospace"] "Response Header"
  Fields.FCResponseBody -> span_ [class_ "px-2 rounded-xl bg-red-100 red-800 monospace"] "Response Body"
