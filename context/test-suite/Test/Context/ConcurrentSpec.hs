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
import qualified Control.Monad as Monad

data Thing = Thing
  { stuff :: Int
  } deriving stock (Eq, Show)

spec :: Spec
spec = do
  describe "forkIO" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Monad.void $ Context.forkIO $ do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Monad.void $ Context.forkIO $ do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Monad.void $ Context.forkIO $ do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Monad.void $ Context.forkIO $ do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "forkFinally" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            let checkStores = do
                  Context.mineMay store1 `shouldReturn` Nothing
                  Context.mineMay store2 `shouldReturn` Nothing
            Monad.void $ Context.forkFinally checkStores $ \_eResult -> do
              checkStores
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                let checkStores = do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                      Context.mineMay store2 `shouldReturn` Just 'a'
                Monad.void $ Context.forkFinally checkStores $ \_eResult -> do
                  checkStores
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    let checkStores = do
                          Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                          Context.mineMay store2 `shouldReturn` Just 'b'
                    Monad.void $ Context.forkFinally checkStores $ \_eResult -> do
                      checkStores
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            let checkStores = do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
            Monad.void $ Context.forkFinally checkStores $ \_eResult -> do
              checkStores
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "forkIOWithUnmask" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Monad.void $ Context.forkIOWithUnmask $ \_restore -> do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Monad.void $ Context.forkIOWithUnmask $ \_restore -> do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Monad.void $ Context.forkIOWithUnmask $ \_restore -> do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Monad.void $ Context.forkIOWithUnmask $ \_restore -> do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "forkOn" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Monad.void $ Context.forkOn 1 $ do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Monad.void $ Context.forkOn 1 $ do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Monad.void $ Context.forkOn 1 $ do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Monad.void $ Context.forkOn 1 $ do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "forkOnWithUnmask" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Monad.void $ Context.forkOnWithUnmask 1 $ \_restore -> do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Monad.void $ Context.forkOnWithUnmask 1 $ \_restore -> do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Monad.void $ Context.forkOnWithUnmask 1 $ \_restore -> do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Monad.void $ Context.forkOnWithUnmask 1 $ \_restore -> do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "forkOS" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Monad.void $ Context.forkOS $ do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Monad.void $ Context.forkOS $ do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Monad.void $ Context.forkOS $ do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Monad.void $ Context.forkOS $ do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "forkOSWithUnmask" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Monad.void $ Context.forkOSWithUnmask $ \_restore -> do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Monad.void $ Context.forkOSWithUnmask $ \_restore -> do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Monad.void $ Context.forkOSWithUnmask $ \_restore -> do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Monad.void $ Context.forkOSWithUnmask $ \_restore -> do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "runInBoundThread" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Context.runInBoundThread $ do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Context.runInBoundThread $ do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Context.runInBoundThread $ do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Context.runInBoundThread $ do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone

  describe "runInUnboundThread" $ do
    describe "mineMay" $ do
      it "empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore @IO @Thing $ \store1 -> do
          Context.withEmptyStore @IO @Char $ \store2 -> do
            Context.runInUnboundThread $ do
              Context.mineMay store1 `shouldReturn` Nothing
              Context.mineMay store2 `shouldReturn` Nothing
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with registered context" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.withEmptyStore $ \store2 -> do
              Context.use store2 'a' $ do
                Context.runInUnboundThread $ do
                  Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
                  Context.mineMay store2 `shouldReturn` Just 'a'
                  Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "initially-empty stores with nested contexts" $ do
        threadDone <- Context.newEmptyMVar
        Context.withEmptyStore $ \store1 -> do
          Context.use store1 Thing { stuff = 1 } $ do
            Context.use store1 Thing { stuff = 2 } $ do
              Context.withEmptyStore $ \store2 -> do
                Context.use store2 'a' $ do
                  Context.use store2 'b' $ do
                    Context.runInUnboundThread $ do
                      Context.mineMay store1 `shouldReturn` Just Thing { stuff = 2 }
                      Context.mineMay store2 `shouldReturn` Just 'b'
                      Context.putMVar threadDone ()
        Context.takeMVar threadDone
      it "non-empty stores" $ do
        threadDone <- Context.newEmptyMVar
        Context.withNonEmptyStore Thing { stuff = 1 } $ \store1 -> do
          Context.withNonEmptyStore 'a' $ \store2 -> do
            Context.runInUnboundThread $ do
              Context.mineMay store1 `shouldReturn` Just Thing { stuff = 1 }
              Context.mineMay store2 `shouldReturn` Just 'a'
              Context.putMVar threadDone ()
        Context.takeMVar threadDone
