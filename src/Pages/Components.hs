module Pages.Components (statBox) where
import Relude
import Lucid
import Utils
import Fmt

statBox :: Text -> Text -> Int -> Maybe Int -> Html ()
statBox title helpInfo val bckupValM= do
  div_ [class_ "col-span-1 card-round p-5 flex flex-row content-between justify-between"] $ do
    div_ $ do
      div_ [class_ "inline-block flex flex-row content-between"] $ do
        strong_ [class_ "font-bold text-2xl"] $ toHtml  @Text $ fmt (commaizeF val)
        maybe "" (\bVal -> small_ $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
      span_ $ toHtml title
    span_ [class_ "inline-block", term "data-tippy-content" helpInfo] $ mIcon_ "info" "w-4 h-4"
