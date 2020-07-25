{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Context.ResourceSpec
  ( spec
  ) where

import Context.Resource (shareResource, withProvider, withResource, withSharedResource)
import Foreign (Ptr)
import Prelude
import Test.Hspec
import qualified Context
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
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
    it "cannot implicitly share across thread boundaries" do
      withProvider withRes \provider -> do
        withResource provider \parentThread'sPtr1 -> do
          shareResource provider parentThread'sPtr1 do
            withResource provider \parentThread'sPtr2 -> do
              parentThread'sPtr1 `shouldBe` parentThread'sPtr2
            threadDone <- Context.newEmptyMVar
            Monad.void $ Context.forkIO do
              withResource provider \childThread'sPtr1 -> do
                parentThread'sPtr1 `shouldNotBe` childThread'sPtr1
              Context.putMVar threadDone ()
            Context.takeMVar threadDone
          withResource provider \parentThread'sPtr3 -> do
            parentThread'sPtr1 `shouldNotBe` parentThread'sPtr3

  describe "withSharedResource" do
    it "concurrent test" do
      Async.forConcurrently_ [1 :: Int ..10] $ const do
        withProvider withRes \provider -> do
          withSharedResource provider \ptr1 -> do
            withResource provider \ptr2 -> do
              ptr1 `shouldBe` ptr2
              shareResource provider ptr2 do
                withResource provider \ptr3 -> do
                  ptr2 `shouldBe` ptr3
            withResource provider \ptr4 -> do
              ptr1 `shouldBe` ptr4
    it "cannot implicitly share across thread boundaries" do
      withProvider withRes \provider -> do
        withSharedResource provider \parentThread'sPtr1 -> do
          withResource provider \parentThread'sPtr2 -> do
            parentThread'sPtr1 `shouldBe` parentThread'sPtr2
          threadDone <- Context.newEmptyMVar
          Monad.void $ Context.forkIO do
            withResource provider \childThread'sPtr1 -> do
              parentThread'sPtr1 `shouldNotBe` childThread'sPtr1
            Context.putMVar threadDone ()
          Context.takeMVar threadDone
          withResource provider \parentThread'sPtr3 -> do
            parentThread'sPtr1 `shouldBe` parentThread'sPtr3

withRes :: (Ptr Int -> IO a) -> IO a
withRes = Foreign.withArray [1..3]
