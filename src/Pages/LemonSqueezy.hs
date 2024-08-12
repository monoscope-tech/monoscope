module Pages.LemonSqueezy (webhookPostH, WebhookData) where

import Data.Aeson qualified as AE
import Data.Time (getCurrentTime, getZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Models.Projects.LemonSqueezy qualified as LemonSqueezy
import Relude
import System.Types (ATBaseCtx, RespHeaders, addRespHeaders)


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
  {customData :: CustomData, eventName :: Text}
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


-- , subscriptionId :: Text
-- , orderId :: Text
-- , firstSubId :: Text
-- , productName :: Text
-- , useEmail :: Text

webhookPostH :: WebhookData -> ATBaseCtx (Html ())
webhookPostH dat = do
  currentTime <- liftIO getZonedTime
  subId <- LemonSqueezy.LemonSubId <$> liftIO UUIDV4.nextRandom
  let projectId = fromMaybe "" dat.meta.customData.projectId
  let subItem = dat.dataVal.attributes.firstSubscriptionItem
  let sub =
        LemonSqueezy.LemonSub
          { id = subId
          , createdAt = currentTime
          , updatedAt = currentTime
          , projectId = projectId
          , subscriptionId = subItem.subscriptionId
          , orderId = dat.dataVal.attributes.orderId
          , firstSubId = subItem.id
          , productName = dat.dataVal.attributes.productName
          , userEmail = dat.dataVal.attributes.userEmail
          }
  _ <- dbtToEff $ LemonSqueezy.addSubscription sub
  pure ""
