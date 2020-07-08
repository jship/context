{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Test.Context.ImplicitSpec
  ( spec
  ) where

import Context.Implicit (Store)
import Control.Concurrent (ThreadId)
import GHC.Classes (IP(ip))
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import qualified Context.Implicit as Context
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async

data Thing = Thing
  { stuff :: Int
  } deriving stock (Eq, Show)

store :: Store Thing
store = unsafePerformIO Context.emptyStore
{-# NOINLINE store #-}

instance IP "contextStore" (Store Thing) where
  ip = store

spec :: Spec
spec = do
  describe "emptyStore" do
    describe "mineMay" do
      it "empty" do
        Context.mineMay `shouldReturn` Nothing
      it "single context" do
        Context.use Thing { stuff = 1 } do
          Context.mineMay `shouldReturn` Just Thing { stuff = 1 }
        Context.mineMay `shouldReturn` Nothing
      it "nested contexts" do
        Context.use Thing { stuff = 1 } do
          Context.mineMay `shouldReturn` Just Thing { stuff = 1 }
          Context.use Thing { stuff = 2 } do
            Context.mineMay `shouldReturn` Just Thing { stuff = 2 }
            Context.use Thing { stuff = 3 } do
              Context.mineMay `shouldReturn` Just Thing { stuff = 3 }
            Context.mineMay `shouldReturn` Just Thing { stuff = 2 }
          Context.use Thing { stuff = 4 } do
            Context.mineMay `shouldReturn` Just Thing { stuff = 4 }
          Context.mineMay `shouldReturn` Just Thing { stuff = 1 }
        Context.mineMay `shouldReturn` Nothing
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Async.forConcurrently_ [1 :: Int ..10] $ const do
          Context.mineMay `shouldReturn` Nothing
          Context.use (mkContext 1) do
            Context.mineMay `shouldReturn` Just (mkContext 1)
            Context.use (mkContext 2) do
              Context.mineMay `shouldReturn` Just (mkContext 2)
              Context.use (mkContext 3) do
                Context.mineMay `shouldReturn` Just (mkContext 3)
              Context.mineMay `shouldReturn` Just (mkContext 2)
            Context.use (mkContext 4) do
              Context.mineMay `shouldReturn` Just (mkContext 4)
            Context.mineMay `shouldReturn` Just (mkContext 1)
          Context.mineMay `shouldReturn` Nothing
        Context.mineMay `shouldReturn` Nothing

    describe "minesMay" do
      it "empty" do
        Context.minesMay stuff `shouldReturn` Nothing
      it "single context" do
        Context.use Thing { stuff = 1 } do
          Context.minesMay stuff `shouldReturn` Just 1
        Context.minesMay stuff `shouldReturn` Nothing
      it "nested contexts" do
        Context.use Thing { stuff = 1 } do
          Context.minesMay stuff `shouldReturn` Just 1
          Context.use Thing { stuff = 2 } do
            Context.minesMay stuff `shouldReturn` Just 2
            Context.use Thing { stuff = 3 } do
              Context.minesMay stuff `shouldReturn` Just 3
            Context.minesMay stuff `shouldReturn` Just 2
          Context.use Thing { stuff = 4 } do
            Context.minesMay stuff `shouldReturn` Just 4
          Context.minesMay stuff `shouldReturn` Just 1
        Context.minesMay stuff `shouldReturn` Nothing
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        Async.forConcurrently_ [1 :: Int ..10] $ const do
          Context.minesMay stuff `shouldReturn` Nothing
          Context.use (mkContext 1) do
            Context.minesMay stuff `shouldReturn` Just 1
            Context.use (mkContext 2) do
              Context.minesMay stuff `shouldReturn` Just 2
              Context.use (mkContext 3) do
                Context.minesMay stuff `shouldReturn` Just 3
              Context.minesMay stuff `shouldReturn` Just 2
            Context.use (mkContext 4) do
              Context.minesMay stuff `shouldReturn` Just 4
            Context.minesMay stuff `shouldReturn` Just 1
          Context.minesMay stuff `shouldReturn` Nothing
        Context.minesMay stuff `shouldReturn` Nothing

    describe "mine" do
      it "empty" do
        threadId <- Concurrent.myThreadId
        Context.mine `shouldThrow` notFound threadId
      it "single context" do
        threadId <- Concurrent.myThreadId
        Context.use Thing { stuff = 1 } do
          Context.mine `shouldReturn` Thing { stuff = 1 }
        Context.mine `shouldThrow` notFound threadId
      it "nested contexts" do
        threadId <- Concurrent.myThreadId
        Context.use Thing { stuff = 1 } do
          Context.mine `shouldReturn` Thing { stuff = 1 }
          Context.use Thing { stuff = 2 } do
            Context.mine `shouldReturn` Thing { stuff = 2 }
            Context.use Thing { stuff = 3 } do
              Context.mine `shouldReturn` Thing { stuff = 3 }
            Context.mine `shouldReturn` Thing { stuff = 2 }
          Context.use Thing { stuff = 4 } do
            Context.mine `shouldReturn` Thing { stuff = 4 }
          Context.mine `shouldReturn` Thing { stuff = 1 }
        Context.mine `shouldThrow` notFound threadId
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        initThreadId <- Concurrent.myThreadId
        Async.forConcurrently_ [1 :: Int ..10] $ const do
          threadId <- Concurrent.myThreadId
          Context.mine `shouldThrow` notFound threadId
          Context.use (mkContext 1) do
            Context.mine `shouldReturn` mkContext 1
            Context.use (mkContext 2) do
              Context.mine `shouldReturn` mkContext 2
              Context.use (mkContext 3) do
                Context.mine `shouldReturn` mkContext 3
              Context.mine `shouldReturn` mkContext 2
            Context.use (mkContext 4) do
              Context.mine `shouldReturn` mkContext 4
            Context.mine `shouldReturn` mkContext 1
          Context.mine `shouldThrow` notFound threadId
        Context.mine `shouldThrow` notFound initThreadId

    describe "mines" do
      it "empty" do
        threadId <- Concurrent.myThreadId
        Context.mines stuff `shouldThrow` notFound threadId
      it "single context" do
        threadId <- Concurrent.myThreadId
        Context.use Thing { stuff = 1 } do
          Context.mines stuff `shouldReturn` 1
        Context.mines stuff `shouldThrow` notFound threadId
      it "nested contexts" do
        threadId <- Concurrent.myThreadId
        Context.use Thing { stuff = 1 } do
          Context.mines stuff `shouldReturn` 1
          Context.use Thing { stuff = 2 } do
            Context.mines stuff `shouldReturn` 2
            Context.use Thing { stuff = 3 } do
              Context.mines stuff `shouldReturn` 3
            Context.mines stuff `shouldReturn` 2
          Context.use Thing { stuff = 4 } do
            Context.mines stuff `shouldReturn` 4
          Context.mines stuff `shouldReturn` 1
        Context.mines stuff `shouldThrow` notFound threadId
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        initThreadId <- Concurrent.myThreadId
        Async.forConcurrently_ [1 :: Int ..10] $ const do
          threadId <- Concurrent.myThreadId
          Context.mine `shouldThrow` notFound threadId
          Context.use (mkContext 1) do
            Context.mines stuff `shouldReturn` 1
            Context.use (mkContext 2) do
              Context.mines stuff `shouldReturn` 2
              Context.use (mkContext 3) do
                Context.mines stuff `shouldReturn` 3
              Context.mines stuff `shouldReturn` 2
            Context.use (mkContext 4) do
              Context.mines stuff `shouldReturn` 4
            Context.mines stuff `shouldReturn` 1
          Context.mines stuff `shouldThrow` notFound threadId
        Context.mines stuff `shouldThrow` notFound initThreadId

    describe "adjust" do
      it "empty" do
        threadId <- Concurrent.myThreadId
        Context.adjust modifier (error "does not get here")
          `shouldThrow` notFound threadId
      it "single context" do
        threadId <- Concurrent.myThreadId
        Context.use Thing { stuff = 1 } do
          Context.mine `shouldReturn` Thing { stuff = 1 }
          Context.adjust modifier do
            Context.mine `shouldReturn` Thing { stuff = 2 }
          Context.mine `shouldReturn` Thing { stuff = 1 }
        Context.mine `shouldThrow` notFound threadId
      it "nested contexts" do
        threadId <- Concurrent.myThreadId
        Context.use Thing { stuff = 1 } do
          Context.adjust modifier do
            Context.mine `shouldReturn` Thing { stuff = 2 }
            Context.use Thing { stuff = 3 } do
              Context.mine `shouldReturn` Thing { stuff = 3 }
              Context.use Thing { stuff = 4 } do
                Context.mine `shouldReturn` Thing { stuff = 4 }
              Context.mine `shouldReturn` Thing { stuff = 3 }
            Context.use Thing { stuff = 4 } do
              Context.mine `shouldReturn` Thing { stuff = 4 }
            Context.mine `shouldReturn` Thing { stuff = 2 }
          Context.mine `shouldReturn` Thing { stuff = 1 }
        Context.mine `shouldThrow` notFound threadId
      it "concurrent nested contexts" do
        let mkContext i = Thing { stuff = i }
        initThreadId <- Concurrent.myThreadId
        Async.forConcurrently_ [1 :: Int ..10] $ const do
          threadId <- Concurrent.myThreadId
          Context.mine `shouldThrow` notFound threadId
          Context.use (mkContext 1) do
            Context.adjust modifier do
              Context.mine `shouldReturn` mkContext 2
              Context.use (mkContext 3) do
                Context.mine `shouldReturn` mkContext 3
                Context.use (mkContext 4) do
                  Context.mine `shouldReturn` mkContext 4
                Context.mine `shouldReturn` mkContext 3
              Context.use (mkContext 5) do
                Context.mine `shouldReturn` mkContext 5
              Context.mine `shouldReturn` mkContext 2
            Context.mine `shouldReturn` mkContext 1
          Context.mine `shouldThrow` notFound threadId
        Context.mine `shouldThrow` notFound initThreadId

notFound :: ThreadId -> Context.NotFoundException -> Bool
notFound threadId notFoundEx =
  Context.NotFoundException { Context.threadId } == notFoundEx

modifier :: Thing -> Thing
modifier thing = thing { stuff = 2 * stuff thing }
