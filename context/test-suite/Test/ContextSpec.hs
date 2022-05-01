{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Test.ContextSpec
  ( spec
  ) where

import Control.Concurrent (ThreadId)
import Prelude
import Test.Hspec
import qualified Context
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async

data Thing = Thing
  { stuff :: Int
  } deriving stock (Eq, Show)

data OtherThing = OtherThing
  { otherStuff :: Int
  } deriving stock (Eq, Show)

spec :: Spec
spec = do
  describe "withEmptyStore" do
    describe "mineMay" do
      it "empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          Context.mineMay store `shouldReturn` Nothing
      it "single context" do
        Context.withEmptyStore @IO @Thing \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
          Context.mineMay store `shouldReturn` Nothing
      it "nested contexts" do
        Context.withEmptyStore @IO @Thing \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
            Context.use store Thing { stuff = 2 } do
              Context.mineMay store `shouldReturn` Just Thing { stuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.mineMay store `shouldReturn` Just Thing { stuff = 3 }
              Context.mineMay store `shouldReturn` Just Thing { stuff = 2 }
            Context.use store Thing { stuff = 4 } do
              Context.mineMay store `shouldReturn` Just Thing { stuff = 4 }
            Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
          Context.mineMay store `shouldReturn` Nothing
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withEmptyStore @IO @Thing \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.mineMay store `shouldReturn` Nothing
            Context.use store (mkContext 1) do
              Context.mineMay store `shouldReturn` Just (mkContext 1)
              Context.use store (mkContext 2) do
                Context.mineMay store `shouldReturn` Just (mkContext 2)
                Context.use store (mkContext 3) do
                  Context.mineMay store `shouldReturn` Just (mkContext 3)
                Context.mineMay store `shouldReturn` Just (mkContext 2)
              Context.use store (mkContext 4) do
                Context.mineMay store `shouldReturn` Just (mkContext 4)
              Context.mineMay store `shouldReturn` Just (mkContext 1)
            Context.mineMay store `shouldReturn` Nothing
          Context.mineMay store `shouldReturn` Nothing

    describe "minesMay" do
      it "empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          Context.minesMay store stuff `shouldReturn` Nothing
      it "single context" do
        Context.withEmptyStore @IO @Thing \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.minesMay store stuff `shouldReturn` Just 1
          Context.minesMay store stuff `shouldReturn` Nothing
      it "nested contexts" do
        Context.withEmptyStore @IO @Thing \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.minesMay store stuff `shouldReturn` Just 1
            Context.use store Thing { stuff = 2 } do
              Context.minesMay store stuff `shouldReturn` Just 2
              Context.use store Thing { stuff = 3 } do
                Context.minesMay store stuff `shouldReturn` Just 3
              Context.minesMay store stuff `shouldReturn` Just 2
            Context.use store Thing { stuff = 4 } do
              Context.minesMay store stuff `shouldReturn` Just 4
            Context.minesMay store stuff `shouldReturn` Just 1
          Context.minesMay store stuff `shouldReturn` Nothing
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withEmptyStore @IO @Thing \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.minesMay store stuff `shouldReturn` Nothing
            Context.use store (mkContext 1) do
              Context.minesMay store stuff `shouldReturn` Just 1
              Context.use store (mkContext 2) do
                Context.minesMay store stuff `shouldReturn` Just 2
                Context.use store (mkContext 3) do
                  Context.minesMay store stuff `shouldReturn` Just 3
                Context.minesMay store stuff `shouldReturn` Just 2
              Context.use store (mkContext 4) do
                Context.minesMay store stuff `shouldReturn` Just 4
              Context.minesMay store stuff `shouldReturn` Just 1
            Context.minesMay store stuff `shouldReturn` Nothing
          Context.minesMay store stuff `shouldReturn` Nothing

    describe "mine" do
      it "empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.mine store `shouldThrow` notFound threadId
      it "single context" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldThrow` notFound threadId
      it "nested contexts" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.mine store `shouldReturn` Thing { stuff = 1 }
            Context.use store Thing { stuff = 2 } do
              Context.mine store `shouldReturn` Thing { stuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.mine store `shouldReturn` Thing { stuff = 3 }
              Context.mine store `shouldReturn` Thing { stuff = 2 }
            Context.use store Thing { stuff = 4 } do
              Context.mine store `shouldReturn` Thing { stuff = 4 }
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldThrow` notFound threadId
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        initThreadId <- Concurrent.myThreadId
        Context.withEmptyStore @IO @Thing \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            threadId <- Concurrent.myThreadId
            Context.mine store `shouldThrow` notFound threadId
            Context.use store (mkContext 1) do
              Context.mine store `shouldReturn` mkContext 1
              Context.use store (mkContext 2) do
                Context.mine store `shouldReturn` mkContext 2
                Context.use store (mkContext 3) do
                  Context.mine store `shouldReturn` mkContext 3
                Context.mine store `shouldReturn` mkContext 2
              Context.use store (mkContext 4) do
                Context.mine store `shouldReturn` mkContext 4
              Context.mine store `shouldReturn` mkContext 1
            Context.mine store `shouldThrow` notFound threadId
          Context.mine store `shouldThrow` notFound initThreadId

    describe "mines" do
      it "empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.mines store stuff `shouldThrow` notFound threadId
      it "single context" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.mines store stuff `shouldReturn` 1
          Context.mines store stuff `shouldThrow` notFound threadId
      it "nested contexts" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.mines store stuff `shouldReturn` 1
            Context.use store Thing { stuff = 2 } do
              Context.mines store stuff `shouldReturn` 2
              Context.use store Thing { stuff = 3 } do
                Context.mines store stuff `shouldReturn` 3
              Context.mines store stuff `shouldReturn` 2
            Context.use store Thing { stuff = 4 } do
              Context.mines store stuff `shouldReturn` 4
            Context.mines store stuff `shouldReturn` 1
          Context.mines store stuff `shouldThrow` notFound threadId
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        initThreadId <- Concurrent.myThreadId
        Context.withEmptyStore @IO @Thing \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            threadId <- Concurrent.myThreadId
            Context.mine store `shouldThrow` notFound threadId
            Context.use store (mkContext 1) do
              Context.mines store stuff `shouldReturn` 1
              Context.use store (mkContext 2) do
                Context.mines store stuff `shouldReturn` 2
                Context.use store (mkContext 3) do
                  Context.mines store stuff `shouldReturn` 3
                Context.mines store stuff `shouldReturn` 2
              Context.use store (mkContext 4) do
                Context.mines store stuff `shouldReturn` 4
              Context.mines store stuff `shouldReturn` 1
            Context.mines store stuff `shouldThrow` notFound threadId
          Context.mines store stuff `shouldThrow` notFound initThreadId

    describe "adjust" do
      it "empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.adjust store modifier (error "does not get here")
            `shouldThrow` notFound threadId
      it "single context" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.mine store `shouldReturn` Thing { stuff = 1 }
            Context.adjust store modifier do
              Context.mine store `shouldReturn` Thing { stuff = 2 }
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldThrow` notFound threadId
      it "nested contexts" do
        Context.withEmptyStore @IO @Thing \store -> do
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.adjust store modifier do
              Context.mine store `shouldReturn` Thing { stuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.mine store `shouldReturn` Thing { stuff = 3 }
                Context.use store Thing { stuff = 4 } do
                  Context.mine store `shouldReturn` Thing { stuff = 4 }
                Context.mine store `shouldReturn` Thing { stuff = 3 }
              Context.use store Thing { stuff = 4 } do
                Context.mine store `shouldReturn` Thing { stuff = 4 }
              Context.mine store `shouldReturn` Thing { stuff = 2 }
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldThrow` notFound threadId
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        initThreadId <- Concurrent.myThreadId
        Context.withEmptyStore @IO @Thing \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            threadId <- Concurrent.myThreadId
            Context.mine store `shouldThrow` notFound threadId
            Context.use store (mkContext 1) do
              Context.adjust store modifier do
                Context.mine store `shouldReturn` mkContext 2
                Context.use store (mkContext 3) do
                  Context.mine store `shouldReturn` mkContext 3
                  Context.use store (mkContext 4) do
                    Context.mine store `shouldReturn` mkContext 4
                  Context.mine store `shouldReturn` mkContext 3
                Context.use store (mkContext 5) do
                  Context.mine store `shouldReturn` mkContext 5
                Context.mine store `shouldReturn` mkContext 2
              Context.mine store `shouldReturn` mkContext 1
            Context.mine store `shouldThrow` notFound threadId
          Context.mine store `shouldThrow` notFound initThreadId

    describe "setDefault" do
      it "setting default converts store to non-empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          Context.mineMay store `shouldReturn` Nothing
          Context.setDefault store Thing { stuff = 1 }
          Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
          Context.setDefault store Thing { stuff = 2 }
          Context.mineMay store `shouldReturn` Just Thing { stuff = 2 }

  describe "withNonEmptyStore" do
    describe "mineMay" do
      it "default" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.mineMay store `shouldReturn` Just Thing { stuff = 0 }
      it "single context" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
          Context.mineMay store `shouldReturn` Just Thing { stuff = 0 }
      it "nested contexts" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
            Context.use store Thing { stuff = 2 } do
              Context.mineMay store `shouldReturn` Just Thing { stuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.mineMay store `shouldReturn` Just Thing { stuff = 3 }
              Context.mineMay store `shouldReturn` Just Thing { stuff = 2 }
            Context.use store Thing { stuff = 4 } do
              Context.mineMay store `shouldReturn` Just Thing { stuff = 4 }
            Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
          Context.mineMay store `shouldReturn` Just Thing { stuff = 0 }
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withNonEmptyStore (mkContext 0) \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.mineMay store `shouldReturn` Just (mkContext 0)
            Context.use store (mkContext 1) do
              Context.mineMay store `shouldReturn` Just (mkContext 1)
              Context.use store (mkContext 2) do
                Context.mineMay store `shouldReturn` Just (mkContext 2)
                Context.use store (mkContext 3) do
                  Context.mineMay store `shouldReturn` Just (mkContext 3)
                Context.mineMay store `shouldReturn` Just (mkContext 2)
              Context.use store (mkContext 4) do
                Context.mineMay store `shouldReturn` Just (mkContext 4)
              Context.mineMay store `shouldReturn` Just (mkContext 1)
            Context.mineMay store `shouldReturn` Just (mkContext 0)
          Context.mineMay store `shouldReturn` Just (mkContext 0)

    describe "minesMay" do
      it "default" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.minesMay store stuff `shouldReturn` Just 0
      it "single context" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.minesMay store stuff `shouldReturn` Just 1
          Context.minesMay store stuff `shouldReturn` Just 0
      it "nested contexts" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.minesMay store stuff `shouldReturn` Just 1
            Context.use store Thing { stuff = 2 } do
              Context.minesMay store stuff `shouldReturn` Just 2
              Context.use store Thing { stuff = 3 } do
                Context.minesMay store stuff `shouldReturn` Just 3
              Context.minesMay store stuff `shouldReturn` Just 2
            Context.use store Thing { stuff = 4 } do
              Context.minesMay store stuff `shouldReturn` Just 4
            Context.minesMay store stuff `shouldReturn` Just 1
          Context.minesMay store stuff `shouldReturn` Just 0
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withNonEmptyStore (mkContext 0) \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.minesMay store stuff `shouldReturn` Just 0
            Context.use store (mkContext 1) do
              Context.minesMay store stuff `shouldReturn` Just 1
              Context.use store (mkContext 2) do
                Context.minesMay store stuff `shouldReturn` Just 2
                Context.use store (mkContext 3) do
                  Context.minesMay store stuff `shouldReturn` Just 3
                Context.minesMay store stuff `shouldReturn` Just 2
              Context.use store (mkContext 4) do
                Context.minesMay store stuff `shouldReturn` Just 4
              Context.minesMay store stuff `shouldReturn` Just 1
            Context.minesMay store stuff `shouldReturn` Just 0
          Context.minesMay store stuff `shouldReturn` Just 0

    describe "mine" do
      it "default" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.mine store `shouldReturn` Thing { stuff = 0 }
      it "single context" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldReturn` Thing { stuff = 0 }
      it "nested contexts" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mine store `shouldReturn` Thing { stuff = 1 }
            Context.use store Thing { stuff = 2 } do
              Context.mine store `shouldReturn` Thing { stuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.mine store `shouldReturn` Thing { stuff = 3 }
              Context.mine store `shouldReturn` Thing { stuff = 2 }
            Context.use store Thing { stuff = 4 } do
              Context.mine store `shouldReturn` Thing { stuff = 4 }
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldReturn` Thing { stuff = 0 }
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withNonEmptyStore (mkContext 0) \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.mine store `shouldReturn` mkContext 0
            Context.use store (mkContext 1) do
              Context.mine store `shouldReturn` mkContext 1
              Context.use store (mkContext 2) do
                Context.mine store `shouldReturn` mkContext 2
                Context.use store (mkContext 3) do
                  Context.mine store `shouldReturn` mkContext 3
                Context.mine store `shouldReturn` mkContext 2
              Context.use store (mkContext 4) do
                Context.mine store `shouldReturn` mkContext 4
              Context.mine store `shouldReturn` mkContext 1
            Context.mine store `shouldReturn` mkContext 0
          Context.mine store `shouldReturn` mkContext 0

    describe "mines" do
      it "default" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.mines store stuff `shouldReturn` 0
      it "single context" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mines store stuff `shouldReturn` 1
          Context.mines store stuff `shouldReturn` 0
      it "nested contexts" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mines store stuff `shouldReturn` 1
            Context.use store Thing { stuff = 2 } do
              Context.mines store stuff `shouldReturn` 2
              Context.use store Thing { stuff = 3 } do
                Context.mines store stuff `shouldReturn` 3
              Context.mines store stuff `shouldReturn` 2
            Context.use store Thing { stuff = 4 } do
              Context.mines store stuff `shouldReturn` 4
            Context.mines store stuff `shouldReturn` 1
          Context.mines store stuff `shouldReturn` 0
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withNonEmptyStore (mkContext 0) \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.mines store stuff `shouldReturn` 0
            Context.use store (mkContext 1) do
              Context.mines store stuff `shouldReturn` 1
              Context.use store (mkContext 2) do
                Context.mines store stuff `shouldReturn` 2
                Context.use store (mkContext 3) do
                  Context.mines store stuff `shouldReturn` 3
                Context.mines store stuff `shouldReturn` 2
              Context.use store (mkContext 4) do
                Context.mines store stuff `shouldReturn` 4
              Context.mines store stuff `shouldReturn` 1
            Context.mines store stuff `shouldReturn` 0
          Context.mines store stuff `shouldReturn` 0

    describe "adjust" do
      it "default" do
        Context.withNonEmptyStore Thing { stuff = 1 } \store -> do
          Context.adjust store modifier do
            Context.mine store `shouldReturn` Thing { stuff = 2 }
          Context.mine store `shouldReturn` Thing { stuff = 1 }
      it "single context" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.mine store `shouldReturn` Thing { stuff = 1 }
            Context.adjust store modifier do
              Context.mine store `shouldReturn` Thing { stuff = 2 }
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldReturn` Thing { stuff = 0 }
      it "nested contexts" do
        Context.withNonEmptyStore Thing { stuff = 0 } \store -> do
          Context.use store Thing { stuff = 1 } do
            Context.adjust store modifier do
              Context.mine store `shouldReturn` Thing { stuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.mine store `shouldReturn` Thing { stuff = 3 }
                Context.use store Thing { stuff = 4 } do
                  Context.mine store `shouldReturn` Thing { stuff = 4 }
                Context.mine store `shouldReturn` Thing { stuff = 3 }
              Context.use store Thing { stuff = 4 } do
                Context.mine store `shouldReturn` Thing { stuff = 4 }
              Context.mine store `shouldReturn` Thing { stuff = 2 }
            Context.mine store `shouldReturn` Thing { stuff = 1 }
          Context.mine store `shouldReturn` Thing { stuff = 0 }
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withNonEmptyStore (mkContext 0) \store -> do
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.mine store `shouldReturn` mkContext 0
            Context.use store (mkContext 1) do
              Context.adjust store modifier do
                Context.mine store `shouldReturn` mkContext 2
                Context.use store (mkContext 3) do
                  Context.mine store `shouldReturn` mkContext 3
                  Context.use store (mkContext 4) do
                    Context.mine store `shouldReturn` mkContext 4
                  Context.mine store `shouldReturn` mkContext 3
                Context.use store (mkContext 5) do
                  Context.mine store `shouldReturn` mkContext 5
                Context.mine store `shouldReturn` mkContext 2
              Context.mine store `shouldReturn` mkContext 1
            Context.mine store `shouldReturn` mkContext 0
          Context.mine store `shouldReturn` mkContext 0

    describe "setDefault" do
      it "setting default overrides initial default" do
        Context.withNonEmptyStore Thing { stuff = 1 } \store -> do
          Context.mineMay store `shouldReturn` Just Thing { stuff = 1 }
          Context.setDefault store Thing { stuff = 2 }
          Context.mineMay store `shouldReturn` Just Thing { stuff = 2 }

    describe "viewMay" do
      it "empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          Context.viewMay storeView `shouldReturn` Nothing
      it "single context" do
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          Context.use store Thing { stuff = 1 } do
            Context.viewMay storeView `shouldReturn` Just OtherThing { otherStuff = 1 }
          Context.viewMay storeView `shouldReturn` Nothing
      it "nested contexts" do
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          Context.use store Thing { stuff = 1 } do
            Context.viewMay storeView `shouldReturn` Just OtherThing { otherStuff = 1 }
            Context.use store Thing { stuff = 2 } do
              Context.viewMay storeView `shouldReturn` Just OtherThing { otherStuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.viewMay storeView `shouldReturn` Just OtherThing { otherStuff = 3 }
              Context.viewMay storeView `shouldReturn` Just OtherThing { otherStuff = 2 }
            Context.use store Thing { stuff = 4 } do
              Context.viewMay storeView `shouldReturn` Just OtherThing { otherStuff = 4 }
            Context.viewMay storeView `shouldReturn` Just OtherThing { otherStuff = 1 }
          Context.viewMay storeView `shouldReturn` Nothing
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            Context.viewMay storeView `shouldReturn` Nothing
            Context.use store (mkContext 1) do
              Context.viewMay storeView `shouldReturn` Just (toOtherThing $ mkContext 1)
              Context.use store (mkContext 2) do
                Context.viewMay storeView `shouldReturn` Just (toOtherThing $ mkContext 2)
                Context.use store (mkContext 3) do
                  Context.viewMay storeView `shouldReturn` Just (toOtherThing $ mkContext 3)
                Context.viewMay storeView `shouldReturn` Just (toOtherThing $ mkContext 2)
              Context.use store (mkContext 4) do
                Context.viewMay storeView `shouldReturn` Just (toOtherThing $ mkContext 4)
              Context.viewMay storeView `shouldReturn` Just (toOtherThing $ mkContext 1)
            Context.viewMay storeView `shouldReturn` Nothing
          Context.viewMay storeView `shouldReturn` Nothing

    describe "view" do
      it "empty" do
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          threadId <- Concurrent.myThreadId
          Context.view storeView `shouldThrow` notFound threadId
      it "single context" do
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.view storeView `shouldReturn` OtherThing { otherStuff = 1 }
          Context.view storeView `shouldThrow` notFound threadId
      it "nested contexts" do
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          threadId <- Concurrent.myThreadId
          Context.use store Thing { stuff = 1 } do
            Context.view storeView `shouldReturn` OtherThing { otherStuff = 1 }
            Context.use store Thing { stuff = 2 } do
              Context.view storeView `shouldReturn` OtherThing { otherStuff = 2 }
              Context.use store Thing { stuff = 3 } do
                Context.view storeView `shouldReturn` OtherThing { otherStuff = 3 }
              Context.view storeView `shouldReturn` OtherThing { otherStuff = 2 }
            Context.use store Thing { stuff = 4 } do
              Context.view storeView `shouldReturn` OtherThing { otherStuff = 4 }
            Context.view storeView `shouldReturn` OtherThing { otherStuff = 1 }
          Context.view storeView `shouldThrow` notFound threadId
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        initThreadId <- Concurrent.myThreadId
        Context.withEmptyStore @IO @Thing \store -> do
          let storeView = fmap toOtherThing $ Context.toView store
          Async.forConcurrently_ [1 :: Int ..10] $ const do
            threadId <- Concurrent.myThreadId
            Context.view storeView `shouldThrow` notFound threadId
            Context.use store (mkContext 1) do
              Context.view storeView `shouldReturn` (toOtherThing $ mkContext 1)
              Context.use store (mkContext 2) do
                Context.view storeView `shouldReturn` (toOtherThing $ mkContext 2)
                Context.use store (mkContext 3) do
                  Context.view storeView `shouldReturn` (toOtherThing $ mkContext 3)
                Context.view storeView `shouldReturn` (toOtherThing $ mkContext 2)
              Context.use store (mkContext 4) do
                Context.view storeView `shouldReturn` (toOtherThing $ mkContext 4)
              Context.view storeView `shouldReturn` (toOtherThing $ mkContext 1)
            Context.view storeView `shouldThrow` notFound threadId
          Context.view storeView `shouldThrow` notFound initThreadId

notFound :: ThreadId -> Context.NotFoundException -> Bool
notFound threadId notFoundEx =
  Context.NotFoundException { Context.threadId } == notFoundEx

modifier :: Thing -> Thing
modifier thing = thing { stuff = 2 * stuff thing }

toOtherThing :: Thing -> OtherThing
toOtherThing Thing { stuff } = OtherThing { otherStuff = stuff }
