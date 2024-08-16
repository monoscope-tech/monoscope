module Pages.LemonSqueezy (webhookPostH, WebhookData, manageBillingGetH, BillingGet) where

import Data.Aeson qualified as AE
import Data.Default
import Data.Text qualified as T
import Data.Time (LocalTime (LocalTime), ZonedTime, getCurrentTime, getCurrentTimeZone, getZonedTime, localTimeOfDay, utctDay, zonedTimeToLocalTime, zonedTimeToUTC)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (localTimeToUTC, utc)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (asks)
import Lucid
import Lucid.Htmx (hxGet_)
import Models.Projects.LemonSqueezy qualified as LemonSqueezy
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.GRPC.HighLevel (AuthContext)
import Pages.BodyWrapper (BWConfig (..), PageCtx (PageCtx))
import Relude hiding (asks)
import System.Config (env, lemonSqueezyWebhookSecret)
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Text.Printf (printf)
import Utils (faSprite_)


data FirstSubItem = FirstSubItem
  { id :: Int
  , subscriptionId :: Int
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] FirstSubItem


newtype CustomData = CustomData
  { projectId :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] CustomData


data MetaData = MetaData
  {customData :: Maybe CustomData, eventName :: Text}
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] MetaData


data Attributes = Attributes
  { firstSubscriptionItem :: FirstSubItem
  , productName :: Text
  , orderId :: Int
  , userEmail :: Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] Attributes


data DataVals = DataVals
  { id :: Text
  , attributes :: Attributes
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] DataVals


data WebhookData = WebhookData
  { dataVal :: DataVals
  , meta :: MetaData
  }
  deriving stock (Show, Generic)


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
      subId <- LemonSqueezy.LemonSubId <$> liftIO UUIDV4.nextRandom
      let projectId = case dat.meta.customData of
            Nothing -> ""
            Just d -> fromMaybe "" d.projectId
      let sub =
            LemonSqueezy.LemonSub
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
      _ <- dbtToEff $ LemonSqueezy.addSubscription sub
      pure "subscription created"
    "subscription_cancelled" -> do
      _ <- dbtToEff $ LemonSqueezy.downgradeToFree orderId subItem.subscriptionId subItem.id
      pure "downgraded"
    "subscription_resumed" -> do
      _ <- dbtToEff $ LemonSqueezy.upgradeToPaid orderId subItem.subscriptionId subItem.id
      pure "Upgraded"
    "subscription_expired" -> do
      _ <- dbtToEff $ LemonSqueezy.downgradeToFree orderId subItem.subscriptionId subItem.id
      pure "Downgraded to free,sub expired"
    _ -> pure ""


newtype BillingGet = BillingGet (PageCtx (Text, Int64, Text, Text))


instance ToHtml BillingGet where
  toHtml (BillingGet (PageCtx bwconf (pidText, totalReqs, amount, last_reported))) = toHtml $ PageCtx bwconf $ billingPage pidText totalReqs amount last_reported
  toHtmlRaw = toHtml


manageBillingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders BillingGet)
manageBillingGetH pid from = do
  (sess, project) <- Sessions.sessionAndProject pid
  let dat = fromMaybe project.createdAt project.billingDay
  currentTime <- liftIO getZonedTime
  let cycleStart = calculateCycleStartDate dat currentTime
  totalRequests <- dbtToEff $ LemonSqueezy.getTotalUsage pid cycleStart
  let estimatedAmount = show $ if totalRequests == 0 then "0.00" else printf "%.2f" (fromIntegral totalRequests / 20_000)
  let last_reported = show project.usageLastReported
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Manage Billing"
          }
  addRespHeaders $ BillingGet $ PageCtx bwconf (pid.toText, totalRequests, estimatedAmount, last_reported)


billingPage :: Text -> Int64 -> Text -> Text -> Html ()
billingPage pidTxt reqs amount last_reported = div_ [class_ "w-full pt-40"] do
  div_ [class_ "border w-[550px] rounded-xl shadow-sm mx-auto p-10"] do
    div_ [class_ "flex flex-col gap-1"] do
      h3_ [class_ "font-bold text-3xl"] "Usage & Billing"
      p_ [class_ "text-gray-600"] "Track your usage and estimated costs"
    div_ [class_ "flex gap-6 justify-between mt-8"] do
      div_ [class_ "flex flex-col gap-2"] $ do
        div_ [class_ "text-4xl font-bold"] $ toHtml $ formatNumberWithCommas reqs
        div_ [class_ "text-sm text-gray-600"] "Total Requests Made"
        div_ [class_ "text-sm text-gray-600"] "*Calculation may not be up-to-date"
      div_ [class_ "flex flex-col gap-2"] $ do
        div_ [class_ "text-4xl font-bold"] $ toHtml $ "$" <> T.replace "\"" "" amount
        div_ [class_ "text-sm text-gray-600"] "Estimated Cost"
        div_ [class_ "text-sm text-gray-600"] "*Based on current usage"
    div_ [class_ "mt-8 flex flex-col gap-4"] do
      div_ [class_ "flex items-center gap-2 text-sm text-gray-600"] $ do
        faSprite_ "regular-calendar-days-clock" "regular" "h-4 w-4"
        span_ [data_ "id" "18"] $ toHtml $ "Latest data: " <> T.take 19 last_reported
      a_ [class_ "flex items-center gap-2 font-bold cursor-pointer", hxGet_ [text| /p/$pidTxt/manage_subscription |]] $ do
        faSprite_ "link-simple" "regular" "h-4 w-4"
        span_ [data_ "id" "18"] "View on LemonSqueezy"


calculateCycleStartDate :: ZonedTime -> ZonedTime -> UTCTime
calculateCycleStartDate start currentZonedTime =
  let (_, _, startDay) = toGregorian (utctDay (zonedTimeToUTC start))
      (currentYear, currentMonth, currentDay) = toGregorian (utctDay (zonedTimeToUTC currentZonedTime))
      timeOfDay = localTimeOfDay $ zonedTimeToLocalTime start
      cycleStartDate
        | currentDay > startDay = fromGregorian currentYear currentMonth startDay
        | otherwise = if currentMonth == 1 then fromGregorian (currentYear - 1) 12 startDay else fromGregorian currentYear (currentMonth - 1) startDay
      locT = LocalTime cycleStartDate timeOfDay
      cycleStartTime = localTimeToUTC utc locT
   in cycleStartTime


formatNumberWithCommas :: Int64 -> String
formatNumberWithCommas n = reverse $ insertCommas $ reverse (show n)
  where
    insertCommas [] = []
    insertCommas xs = case splitAt 3 xs of
      (chunk, []) -> chunk
      (chunk, rest) -> chunk ++ "," ++ insertCommas rest
