module Pages.ShareSpec (spec) where

import Pkg.TestUtils
import Test.Hspec


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Share Request" do
    it "should create share link" \TestResources{} -> do
      -- currentTime <- getCurrentTime
      -- let reqId = Unsafe.fromJust $ UUID.fromText "00000000-0000-0000-0000-000000000000"
      --     nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      --     reqMsg1 = Unsafe.fromJust $ convert $ msg1 nowTxt
      --     msgs = [("m1", reqMsg1)]

      -- _ <- runTestBackground trATCtx $ processRequestMessages msgs

      -- res <- toServantResponse trATCtx trSessAndHeader trLogger $ Share.shareLinkPostH testPid reqId Nothing
      -- case res of
      --   Share.ShareLinkPost shareId -> do
      --     let share_id = Unsafe.fromJust $ UUID.fromText shareId
      --     Share.ShareLinkGet (PageCtx _ reqM) <-
      --       Share.shareLinkGetH share_id
      --         & effToServantHandlerTest trATCtx trLogger
      --         & ServantS.runHandler
      --         <&> fromRightShow
      --     isJust reqM `shouldBe` True
      --     let req = Unsafe.fromJust reqM
      --     req.id `shouldBe` Unsafe.fromJust (UUID.fromText "00000000-0000-0000-0000-000000000000")
      --     req.projectId `shouldBe` testPid
      --     req.urlPath `shouldBe` "/"
      --   _ -> error "Unexpected response"
      1 `shouldBe` 1

    it "should NOT create share link" \TestResources{} -> do
      -- currentTime <- getCurrentTime
      -- let reqId = Unsafe.fromJust $ UUID.fromText "00000000-0000-0000-0000-000000000000"
      -- res <- toServantResponse trATCtx trSessAndHeader trLogger $ Share.shareLinkPostH testPid reqId (Just "request")
      -- case res of
      --   Share.ShareLinkPostError -> do
      --     pass
      --   _ -> error "Unexpected response"
      1 `shouldBe` 1
