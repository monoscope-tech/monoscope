module Pages.LemonSqueezy (webhookPostH, WebhookData (..), DataVals (..), MetaData (..), CustomData (..), Attributes (..), FirstSubItem (..), manageBillingGetH, BillingGet (..)) where

import Data.Aeson qualified as AE
import Data.Default
import Data.Text qualified as T
import Data.Time (getZonedTime, timeOfDayToTime, timeToTimeOfDay)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.UUID.V4 qualified as UUIDV4
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful.Reader.Static (ask, asks)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxGet_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (PageCtx))
import Pages.Components (paymentPlanPicker)
import Relude hiding (ask, asks)
import System.Config
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Text.Printf (printf)
import Utils (faSprite_)


data FirstSubItem = FirstSubItem
  { id :: Int
  , subscriptionId :: Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake FirstSubItem


newtype CustomData = CustomData
  { projectId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake CustomData


data MetaData = MetaData
  {customData :: Maybe CustomData, eventName :: Text}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake MetaData


data Attributes = Attributes
  { firstSubscriptionItem :: FirstSubItem
  , productName :: Text
  , orderId :: Int
  , userEmail :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake Attributes


data DataVals = DataVals
  { id :: Text
  , attributes :: Attributes
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake DataVals


data WebhookData = WebhookData
  { dataVal :: DataVals
  , meta :: MetaData
  }
  deriving stock (Generic, Show)


instance AE.FromJSON WebhookData where
  parseJSON = AE.withObject "WebhookData" $ \obj -> do
    dataVal <- obj AE..: "data"
    meta <- obj AE..: "meta"
    return (WebhookData{dataVal = dataVal, meta = meta})


webhookPostH :: Maybe Text -> WebhookData -> ATBaseCtx (Html ())
webhookPostH secretHeaderM dat = do
  envConfig <- asks env
  let orderId = dat.dataVal.attributes.orderId
  let subItem = dat.dataVal.attributes.firstSubscriptionItem
  case dat.meta.eventName of
    "subscription_created" -> do
      currentTime <- liftIO getZonedTime
      subId <- Projects.LemonSubId <$> liftIO UUIDV4.nextRandom
      let projectId = case dat.meta.customData of
            Nothing -> ""
            Just d -> fromMaybe "" d.projectId
      let sub =
            Projects.LemonSub
              { id = subId
              , createdAt = currentTime
              , updatedAt = currentTime
              , projectId = projectId
              , subscriptionId = subItem.subscriptionId
              , orderId
              , firstSubId = subItem.id
              , productName = dat.dataVal.attributes.productName
              , userEmail = dat.dataVal.attributes.userEmail
              }
      _ <- Projects.addSubscription sub
      pure "subscription created"
    "subscription_cancelled" -> do
      _ <- Projects.downgradeToFree orderId subItem.subscriptionId subItem.id
      pure "downgraded"
    "subscription_resumed" -> do
      _ <- Projects.upgradeToPaid orderId subItem.subscriptionId subItem.id
      pure "Upgraded"
    "subscription_expired" -> do
      _ <- Projects.downgradeToFree orderId subItem.subscriptionId subItem.id
      pure "Downgraded to free,sub expired"
    _ -> pure ""


newtype BillingGet = BillingGet (PageCtx (Projects.ProjectId, Int64, Text, Text, Text, Text, Text, Bool, Bool))


instance ToHtml BillingGet where
  toHtml (BillingGet (PageCtx bwconf (pid, totalReqs, amount, last_reported, lemonUrl, critical, paymentPlan, enableFreetier, basicAuthEnabled))) = toHtml $ PageCtx bwconf $ billingPage pid totalReqs amount last_reported lemonUrl critical paymentPlan enableFreetier basicAuthEnabled
  toHtmlRaw = toHtml


manageBillingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders BillingGet)
manageBillingGetH pid from = do
  (sess, project) <- Sessions.sessionAndProject pid
  let dat = fromMaybe project.createdAt project.billingDay
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  currentTime <- Time.currentTime
  let cycleStart = calculateCycleStartDate dat currentTime
  totalRequests <- Projects.getTotalUsage pid cycleStart
  let requestAfter = totalRequests - 20_000_000
  let estimatedAmount = show $ if requestAfter <= 0 then "34.00" else printf "%.2f" (fromIntegral requestAfter / 500_000 + 34.00)
  let last_reported = show project.usageLastReported
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Manage billing"
          , isSettingsPage = True
          , config = appCtx.config
          }
  let lemonUrl = envCfg.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
      critical = envCfg.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
  addRespHeaders $ BillingGet $ PageCtx bwconf (pid, totalRequests, estimatedAmount, last_reported, lemonUrl, critical, project.paymentPlan, envCfg.enableFreetier, envCfg.basicAuthEnabled)


billingPage :: Projects.ProjectId -> Int64 -> Text -> Text -> Text -> Text -> Text -> Bool -> Bool -> Html ()
billingPage pid reqs amount last_reported lemonUrl critical paymentPlan enableFreetier basicAuthEnabled = div_ [id_ "main-content", class_ "w-full h-full overflow-y-auto"] do
  let pidTxt = pid.toText
  section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
    div_ [class_ "mb-2"] do
      h2_ [class_ "text-textStrong text-xl font-semibold"] "Manage billing"
      p_ [class_ "text-textWeak text-sm mt-1"] "Track your usage and estimated costs"

    div_ [class_ "surface-raised rounded-2xl p-6 space-y-6"] do
      div_ [class_ "flex items-center gap-2 mb-4"] do
        div_ [class_ "p-1.5 rounded-md bg-fillBrand-weak"] $ faSprite_ "chart-line" "regular" "h-3.5 w-3.5 text-textBrand"
        label_ [class_ "text-sm font-medium text-textStrong"] "Usage Overview"

      div_ [class_ "grid grid-cols-2 gap-4"] do
        div_ [class_ "flex flex-col gap-2"] do
          div_ [class_ "text-4xl font-bold text-textStrong"] $ toHtml $ formatNumberWithCommas reqs
          div_ [class_ "text-textWeak text-sm"] "Total Requests Made"
          div_ [class_ "text-textWeak text-xs"] "*Calculation may not be up-to-date"
        div_ [class_ "flex flex-col gap-2"] do
          div_ [class_ "text-4xl font-bold text-textStrong"] $ toHtml $ "$" <> if paymentPlan == "Free" then "0" else T.replace "\"" "" amount
          div_ [class_ "text-textWeak text-sm"] "Estimated Cost"
          div_ [class_ "text-textWeak text-xs"] "*Based on current usage"

      div_ [class_ "border-t border-strokeWeak pt-4 space-y-3"] do
        div_ [class_ "flex items-center gap-2 text-textWeak text-sm"] do
          faSprite_ "regular-calendar-days-clock" "regular" "h-4 w-4"
          span_ $ toHtml $ "Latest data: " <> T.take 19 last_reported
        unless (paymentPlan == "Free") do
          a_ [class_ "flex items-center gap-2 text-textBrand hover:underline cursor-pointer text-sm font-medium", hxGet_ [text| /p/$pidTxt/manage_subscription |]] do
            faSprite_ "link-simple" "regular" "h-4 w-4"
            span_ "View on LemonSqueezy"

    div_ [class_ "surface-raised rounded-2xl p-6 space-y-4"] do
      div_ [class_ "flex items-center gap-2 mb-2"] do
        div_ [class_ "p-1.5 rounded-md bg-fillSuccess-weak"] $ faSprite_ "dollar" "regular" "h-3.5 w-3.5 text-textSuccess"
        label_ [class_ "text-sm font-medium text-textStrong"] "Current Plan"

      div_ [class_ "flex items-center justify-between p-4 border border-strokeWeak rounded-xl bg-fillWeaker"] do
        div_ [class_ "flex flex-col gap-2"] do
          span_ [class_ "text-textStrong font-semibold text-lg"] $ toHtml paymentPlan
          span_ [class_ "rounded-lg text-textWeak bg-fillWeak border border-strokeWeak py-1 px-2.5 text-xs w-max"] "Active"
        div_ [class_ "flex items-baseline gap-1"] do
          span_ [class_ "text-textStrong text-xl"] "$"
          span_ [class_ "text-4xl text-textStrong font-bold"] $ if paymentPlan == "Free" then "0" else if paymentPlan == "Bring your own storage" then "199" else "29"
          span_ [class_ "text-textWeak text-sm ml-1"] "/month"

      div_ [class_ "border-t border-strokeWeak pt-4"] do
        div_ [class_ "text-textStrong text-sm font-semibold mb-2"] "Upgrade plan"
        p_ [class_ "text-textWeak text-sm mb-4"] "Monoscope pricing, click on compare feature below to select the option that best suit your project."
        label_ [class_ "btn btn-primary btn-sm", Lucid.for_ "pricing-modal", [__|on click set #pricing-modal.check to true|]] "Change plan"

  input_ [type_ "checkbox", id_ "pricing-modal", class_ "modal-toggle"]
  div_ [class_ "modal p-8", role_ "dialog", [__|on closeModal from body set #pricing-modal.checked to false |]] do
    div_ [class_ "modal-box relative flex flex-col gap-5 w-[1250px] py-16 px-32", style_ "max-width:1300px"] do
      button_ [class_ "absolute top-8 right-8 cursor-pointer", [__| on click set #pricing-modal.checked to false |]] do
        faSprite_ "circle-xmark" "regular" "w-8 h-8"
      div_ [class_ "text-center text-sm text-textWeak w-full mx-auto max-w-96"] do
        span_ [class_ "text-textStrong text-2xl font-semibold"] "What's Included?"
        p_ [class_ "mt-2 mb-4"] "See and compare what you get in each plan."
        p_ [] "Please adjust the bar below to see difference in price as your events increase"
      paymentPlanPicker pid lemonUrl critical paymentPlan enableFreetier basicAuthEnabled
    label_ [class_ "modal-backdrop", Lucid.for_ "pricing-modal"] "Close"


calculateCycleStartDate :: UTCTime -> UTCTime -> UTCTime
calculateCycleStartDate start current =
  let (_startYear, _startMonth, startDay) = toGregorian $ utctDay start
      (currentYear, currentMonth, currentDay) = toGregorian $ utctDay current
      timeOfDay = timeToTimeOfDay $ utctDayTime start
      cycleStartDay
        | currentDay > startDay = fromGregorian currentYear currentMonth startDay
        | currentMonth == 1 = fromGregorian (currentYear - 1) 12 startDay
        | otherwise = fromGregorian currentYear (currentMonth - 1) startDay
   in UTCTime cycleStartDay (timeOfDayToTime timeOfDay)


formatNumberWithCommas :: Int64 -> String
formatNumberWithCommas n = reverse $ insertCommas $ reverse (show n)
  where
    insertCommas [] = []
    insertCommas xs = case splitAt 3 xs of
      (chunk, []) -> chunk
      (chunk, rest) -> chunk ++ "," ++ insertCommas rest
