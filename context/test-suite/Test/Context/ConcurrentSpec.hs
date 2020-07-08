{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Test.Context.ConcurrentSpec
  ( spec
  ) where

import Prelude
import Test.Hspec
import qualified Context
import qualified Context.Concurrent as Concurrent
import qualified Control.Monad as Monad

data Thing = Thing
  { stuff :: Int
  } deriving stock (Eq, Show)

spec :: Spec
spec = do
  describe "forkIO" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Monad.void $ Concurrent.forkIO do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Monad.void $ Concurrent.forkIO do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Monad.void $ Concurrent.forkIO do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Monad.void $ Concurrent.forkIO do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "forkFinally" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            let checkStores = do
                  Context.mineMay store1 `shouldReturn` Nothing
                  Context.mineMay store2 `shouldReturn` Nothing
            Monad.void $ Concurrent.forkFinally checkStores \_eResult -> do
              checkStores
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                let checkStores = do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                      Context.mineMay store2 `shouldReturn` Just 'a'
                Monad.void $ Concurrent.forkFinally checkStores \_eResult -> do
                  checkStores
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    let checkStores = do
                          Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                          Context.mineMay store2 `shouldReturn` Just 'b'
                    Monad.void $ Concurrent.forkFinally checkStores \_eResult -> do
                      checkStores
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            let checkStores = do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
            Monad.void $ Concurrent.forkFinally checkStores \_eResult -> do
              checkStores
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "forkIOWithUnmask" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Monad.void $ Concurrent.forkIOWithUnmask \_restore -> do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Monad.void $ Concurrent.forkIOWithUnmask \_restore -> do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Monad.void $ Concurrent.forkIOWithUnmask \_restore -> do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Monad.void $ Concurrent.forkIOWithUnmask \_restore -> do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "forkOn" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Monad.void $ Concurrent.forkOn 1 do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Monad.void $ Concurrent.forkOn 1 do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Monad.void $ Concurrent.forkOn 1 do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Monad.void $ Concurrent.forkOn 1 do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "forkOnWithUnmask" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Monad.void $ Concurrent.forkOnWithUnmask 1 \_restore -> do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Monad.void $ Concurrent.forkOnWithUnmask 1 \_restore -> do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Monad.void $ Concurrent.forkOnWithUnmask 1 \_restore -> do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Monad.void $ Concurrent.forkOnWithUnmask 1 \_restore -> do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "forkOS" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Monad.void $ Concurrent.forkOS do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Monad.void $ Concurrent.forkOS do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Monad.void $ Concurrent.forkOS do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Monad.void $ Concurrent.forkOS do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "forkOSWithUnmask" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Monad.void $ Concurrent.forkOSWithUnmask \_restore -> do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Monad.void $ Concurrent.forkOSWithUnmask \_restore -> do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Monad.void $ Concurrent.forkOSWithUnmask \_restore -> do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Monad.void $ Concurrent.forkOSWithUnmask \_restore -> do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "runInBoundThread" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Concurrent.runInBoundThread do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Concurrent.runInBoundThread do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Concurrent.runInBoundThread do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Concurrent.runInBoundThread do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone

  describe "runInUnboundThread" do
    describe "mineMay" do
      it "empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore @Thing \store1 -> do
          Context.withEmptyStore @Char \store2 -> do
            Concurrent.runInUnboundThread do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with registered context" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.withEmptyStore \store2 -> do
              Context.use store2 'a' do
                Concurrent.runInUnboundThread do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "initially-empty stores with nested contexts" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withEmptyStore \store1 -> do
          Context.use store1 Thing { stuff = 1 } do
            Context.use store1 Thing { stuff = 2 } do
              Context.withEmptyStore \store2 -> do
                Context.use store2 'a' do
                  Context.use store2 'b' do
                    Concurrent.runInUnboundThread do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
      it "non-empty stores" do
        threadDone <- Concurrent.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } \store1 -> do
          Context.withNonEmptyStore 'a' \store2 -> do
            Concurrent.runInUnboundThread do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Concurrent.putMVar threadDone ()
        Concurrent.takeMVar threadDone
