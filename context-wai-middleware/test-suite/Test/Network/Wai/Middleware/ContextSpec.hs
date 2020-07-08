{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Network.Wai.Middleware.ContextSpec
  ( spec
  ) where

import Control.Concurrent.STM.TQueue (TQueue)
import Network.Wai (Middleware, Request)
import Prelude
import Test.Hspec
import qualified Context
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as Foldable
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Context as Middleware

spec :: Spec
spec = do
  describe "addRequestContext" do
    it "concurrent test" do
      -- This function creates a value of our context type - 'Int' - by
      -- plucking the "number" header off the request. The header must be
      -- present.
      let mkContext :: Request -> IO Int
          mkContext request = do
            let headers = Wai.requestHeaders request
            let [number] =
                  fmap (read . ByteString.Char8.unpack . snd)
                    $ flip filter headers \(headerName, _) ->
                      "number" == CI.foldedCase headerName
            pure number

      Context.withEmptyStore \contextStore -> do
        numberQueue <- TQueue.newTQueueIO
        runTest numberQueue contextStore
          $ Middleware.addRequestContext contextStore mkContext

  describe "addRequestContextMay" do
    it "concurrent test" do
      -- This function creates a value of our context type - 'Int' - by
      -- plucking the "number" header off the request, if present.
      let mkContext :: Request -> IO (Maybe Int)
          mkContext request = do
            let headers = Wai.requestHeaders request
            let mNumber@Just {} =
                  Maybe.listToMaybe
                    $ fmap (read . ByteString.Char8.unpack . snd)
                    $ flip filter headers \(headerName, _) ->
                        "number" == CI.foldedCase headerName
            pure mNumber

      Context.withEmptyStore \contextStore -> do
        numberQueue <- TQueue.newTQueueIO
        runTest numberQueue contextStore
          $ Middleware.addRequestContextMay contextStore mkContext

  describe "addContext" do
    it "concurrent test" do
      counterRef <- IORef.newIORef 0

      -- This function creates a value of our context type - 'Int' - by
      -- using a sequential counter.
      let mkContext :: IO Int
          mkContext = do
            IORef.atomicModifyIORef' counterRef \counter ->
              (1 + counter, 1 + counter)

      Context.withEmptyStore \contextStore -> do
        numberQueue <- TQueue.newTQueueIO
        runTest numberQueue contextStore
          $ Middleware.addContext contextStore mkContext

runTest :: TQueue Int -> Context.Store Int -> Middleware -> IO ()
runTest numberQueue contextStore middleware = do
  let app =
        middleware \_request sendResponse -> do
          -- Ask for the request handler thread's context, then write it
          -- to the number queue, if present.
          Context.mineMay contextStore >>= \case
            Nothing ->
              pure ()
            Just number ->
              STM.atomically $ TQueue.writeTQueue numberQueue number

          sendResponse
            $ Wai.responseLBS
                HTTP.Types.status200
                [("Content-Type", "text/plain")]
                "Test.Network.Wai.Middleware.ContextSpec"

  -- Spin up a test server for the 'app' defined above.
  Warp.testWithApplication (pure app) \port -> do
    manager <- HTTP.Client.newManager HTTP.Client.defaultManagerSettings
    request <- HTTP.Client.parseRequest $ "http://localhost:" <> show port <> "/abc/def"

    -- Spin up 10 threads that each make 3 http requests into the test
    -- server.
    Async.forConcurrently_ [0 :: Int .. 9] \i -> do
      Foldable.for_ [1..3] \j -> do
        -- Every request gets a "number" header added to it.
        let newHeader = ("number", ByteString.Char8.pack $ show $ 3 * i + j)
        response <- flip HTTP.Client.httpLbs manager request
          { HTTP.Client.requestHeaders =
              newHeader : HTTP.Client.requestHeaders request
          }
        HTTP.Types.statusCode (HTTP.Client.responseStatus response)
          `shouldBe` 200
        HTTP.Client.responseBody response
          `shouldBe` "Test.Network.Wai.Middleware.ContextSpec"

    numbers <- STM.atomically $ TQueue.flushTQueue numberQueue
    List.sort numbers `shouldBe` [1..30]
