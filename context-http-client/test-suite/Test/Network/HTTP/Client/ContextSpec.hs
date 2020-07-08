{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Network.HTTP.Client.ContextSpec
  ( spec
  ) where

import Network.HTTP.Client (BodyReader, Request, Response)
import Prelude
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Context
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.CaseInsensitive as CI
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Network.HTTP.Client as HTTP.Client
import qualified Network.HTTP.Client.Context as HTTP.Client.Context
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

spec :: Spec
spec = do
  describe "modifyRequestsWithContext" do
    it "concurrent test" do
      numberQueue <- TQueue.newTQueueIO @Int
      let app = \request sendResponse -> do
            let headers = Wai.requestHeaders request
            let [number] =
                  fmap (read . ByteString.Char8.unpack . snd)
                    $ flip filter headers \(headerName, _) ->
                        "number" == CI.foldedCase headerName

            STM.atomically $ TQueue.writeTQueue numberQueue number

            sendResponse
              $ Wai.responseLBS
                  HTTP.Types.status200
                  [("Content-Type", "text/plain")]
                  "Test.Network.HTTP.Client.ContextSpec"

      -- This function modifies the request using a value of our context type -
      -- 'Int' - by adding a "number" header to the request containing this
      -- value.
      let modifyRequest :: Maybe Int -> Request -> IO Request
          modifyRequest mContext request = do
            case mContext of
              Nothing -> do
                error "mContext was Nothing!"
              Just context -> do
                pure request
                  { HTTP.Client.requestHeaders =
                      ("number", ByteString.Char8.pack $ show context)
                        : flip filter (HTTP.Client.requestHeaders request)
                            \(headerName, _) ->
                              "number" /= CI.foldedCase headerName
                  }

      Context.withEmptyStore \contextStore -> do
        manager <-
          HTTP.Client.newManager
            $ HTTP.Client.Context.modifyRequestsWithContext
                contextStore
                modifyRequest
                HTTP.Client.defaultManagerSettings

        -- Spin up a test server for the 'app' defined above.
        Warp.testWithApplication (pure app) \port -> do
          request <-
            HTTP.Client.parseRequest
              $ "http://localhost:" <> show port <> "/abc/def"

          -- Spin up 10 threads that each make 3 http requests into the test
          -- server.
          Async.forConcurrently_ [0 :: Int .. 9] \i -> do
            Foldable.for_ [1..3] \j -> do
              -- By using a context here, behind the scenes the manager will also
              -- have access to this context as it will send on this same thread.
              Context.use contextStore (3 * i + j) do
                response <- HTTP.Client.httpLbs request manager
                HTTP.Types.statusCode (HTTP.Client.responseStatus response)
                  `shouldBe` 200
                HTTP.Client.responseBody response
                  `shouldBe` "Test.Network.HTTP.Client.ContextSpec"

        numbers <- STM.atomically $ TQueue.flushTQueue numberQueue
        List.sort numbers `shouldBe` [1..30]

  describe "modifyResponsesWithContext" do
    it "concurrent test" do
      let app = \_request sendResponse -> do
            sendResponse
              $ Wai.responseLBS
                  HTTP.Types.status200
                  [("Content-Type", "text/plain")]
                  "Test.Network.HTTP.Client.ContextSpec"

      -- This function modifies the request using a value of our context type -
      -- 'Int' - by adding a "number" header to the request containing this
      -- value.
      let modifyResponse
            :: Maybe Int
            -> Response BodyReader
            -> IO (Response BodyReader)
          modifyResponse mContext response = do
            case mContext of
              Nothing -> do
                error "mContext was Nothing!"
              Just context -> do
                pure response
                  { HTTP.Client.responseHeaders =
                      ("number", ByteString.Char8.pack $ show context)
                        : HTTP.Client.responseHeaders response
                  }

      Context.withEmptyStore \contextStore -> do
        manager <-
          HTTP.Client.newManager
            $ HTTP.Client.Context.modifyResponsesWithContext
                contextStore
                modifyResponse
                HTTP.Client.defaultManagerSettings

        -- Spin up a test server for the 'app' defined above.
        Warp.testWithApplication (pure app) \port -> do
          request <-
            HTTP.Client.parseRequest
              $ "http://localhost:" <> show port <> "/abc/def"

          -- Spin up 10 threads that each make 3 http requests into the test
          -- server.
          Async.forConcurrently_ [0 :: Int .. 9] \i -> do
            Foldable.for_ [1..3] \j -> do
              Context.use contextStore (3 * i + j) do
                response <- HTTP.Client.httpLbs request manager
                HTTP.Types.statusCode (HTTP.Client.responseStatus response)
                  `shouldBe` 200
                HTTP.Client.responseBody response
                  `shouldBe` "Test.Network.HTTP.Client.ContextSpec"

                let headers = HTTP.Client.responseHeaders response
                let [number] =
                      fmap (read . ByteString.Char8.unpack . snd)
                        $ flip filter headers \(headerName, _) ->
                            "number" == CI.foldedCase headerName

                number `shouldBe` 3 * i + j
