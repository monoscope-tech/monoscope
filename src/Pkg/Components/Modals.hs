module Pkg.Components.Modals (modal_) where

import Data.Text qualified as T
import Lucid
import Lucid.Hyperscript


modal_ :: T.Text -> Html () -> Html () -> Html ()
modal_ modalId btnTrigger contentHtml = do
  label_ [Lucid.for_ modalId] btnTrigger
  input_
    [ class_ "modal-toggle"
    , Lucid.id_ modalId
    , Lucid.type_ "checkbox"
    , [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup end
          on closeModal from body set my.checked to false end
      |]
    ]
  div_ [class_ "modal w-screen", role_ "dialog"] do
    label_ [class_ "modal-backdrop", Lucid.for_ modalId] ""
    div_ [class_ "modal-box w-auto flex flex-col gap-5 max-w-5xl"] do
      label_ [Lucid.for_ modalId, class_ "btn btn-sm btn-circle btn-ghost absolute right-2 top-2"] "✕"
      div_ contentHtml
