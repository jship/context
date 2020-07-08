{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Context.ResourceSpec
  ( spec
  ) where

import Context.Resource (shareResource, withProvider, withResource)
import Foreign (Ptr)
import Prelude
import Test.Hspec
import qualified Control.Concurrent.Async as Async
import qualified Foreign

spec :: Spec
spec = do
  describe "withResource" do
    it "concurrent test" do
      Async.forConcurrently_ [1 :: Int ..10] $ const do
        withProvider withRes \provider -> do
          withResource provider \ptr1 -> do
            withResource provider \ptr2 -> do
              ptr1 `shouldNotBe` ptr2

  describe "shareResource" do
    it "concurrent test" do
      Async.forConcurrently_ [1 :: Int ..10] $ const do
        withProvider withRes \provider -> do
          withResource provider \ptr1 -> do
            shareResource provider ptr1 do
              withResource provider \ptr2 -> do
                ptr1 `shouldBe` ptr2
                withResource provider \ptr3 -> do
                  ptr2 `shouldBe` ptr3
            withResource provider \ptr4 -> do
              ptr1 `shouldNotBe` ptr4

withRes :: (Ptr Int -> IO a) -> IO a
withRes = Foreign.withArray [1..3]
