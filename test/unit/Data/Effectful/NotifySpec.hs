module Data.Effectful.NotifySpec (spec) where

import Data.Aeson qualified as AE
import Data.Effectful.Notify
import Effectful
import Effectful.Log (Log)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Relude
import System.Logging qualified as Logging
import Test.Hspec

spec :: Spec
spec = describe "Notify Effect" $ do
  describe "Test interpreter" $ do
    it "captures email notifications" $ do
      (notifications, result) <- runTestNotify $ do
        sendNotification $ emailNotification "test@example.com" Nothing (Just ("Test Subject", "Test Body"))
        pure "done"
      
      result `shouldBe` "done"
      length notifications `shouldBe` 1
      case viaNonEmpty head notifications of
        Just (EmailNotification emailData) -> do
          emailData.receiver `shouldBe` "test@example.com"
          emailData.subjectMessage `shouldBe` Just ("Test Subject", "Test Body")
        _ -> expectationFailure "Expected EmailNotification"
    
    it "captures multiple notifications" $ do
      (notifications, _) <- runTestNotify $ do
        sendNotification $ emailNotification "user1@example.com" Nothing (Just ("Subject 1", "Body 1"))
        sendNotification $ slackNotification "https://slack.webhook.url" (AE.object ["text" AE..= ("Test message" :: Text)])
        sendNotification $ discordNotification "channel123" (AE.object ["content" AE..= ("Discord message" :: Text)])
        getNotifications
      
      length notifications `shouldBe` 3
      
    it "getNotifications returns accumulated notifications" $ do
      (_, notifs) <- runTestNotify $ do
        sendNotification $ emailNotification "test@example.com" Nothing (Just ("Test", "Test"))
        sendNotification $ emailNotification "test2@example.com" Nothing (Just ("Test2", "Test2"))
        getNotifications
      
      length notifs `shouldBe` 2

-- Helper to run test with proper effect setup
runTestNotify :: Eff '[Notify, Log, IOE] a -> IO ([Notification], a)
runTestNotify action = LogBulk.withBulkStdOutLogger $ \logger ->
  action
    & runNotifyTest
    & Logging.runLog "test" logger
    & runEff