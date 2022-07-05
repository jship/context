{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Network.Wai.Middleware.Context
  ( -- * Middlewares
    -- ** Request-specific
    addRequestContext
  , addRequestContextMay

    -- ** General
  , addContext
  ) where

import Prelude
import qualified Context
import qualified Network.Wai as Wai

-- | Register request-specific context into the provided 'Context.Store', for
-- subsequent use in your 'Wai.Application'. This middleware expects to be able
-- to build a context value from every request. Use the 'addRequestContextMay'
-- variant in your application instead if only some requests will result in a
-- context value.
--
-- Endpoints can access their context from the middleware via `Context.mine'
-- and friends.
--
-- @since 0.1.0.0
addRequestContext
  :: Context.Store ctx
  -> (Wai.Request -> IO ctx)
  -> Wai.Middleware
addRequestContext contextStore mkContext app = \request sendResponse -> do
  context <- mkContext request
  Context.use contextStore context $ do
    app request sendResponse

-- | Register request-specific context into the provided 'Context.Store', for
-- subsequent use in your 'Wai.Application'. This middleware does not expect to
-- be able to build a context value from every request. Use the
-- 'addRequestContext' variant in your application instead if all requests will
-- result in a context value.
--
-- Endpoints can access their context from the middleware via `Context.mineMay'
-- and friends.
--
-- @since 0.1.0.0
addRequestContextMay
  :: Context.Store ctx
  -> (Wai.Request -> IO (Maybe ctx))
  -> Wai.Middleware
addRequestContextMay contextStore mkContext app = \request sendResponse -> do
  mkContext request >>= \case
    Nothing -> app request sendResponse
    Just context ->
      Context.use contextStore context $ do
        app request sendResponse

-- | Register arbitrary context into the provided 'Context.Store', for
-- subsequent use in your 'Wai.Application'. This middleware ignores requests
-- when building context values. Use 'addRequestContext'/'addRequestContextMay'
-- in your application instead if you would like to register request-specific
-- context.
--
-- Endpoints can access their context from the middleware via `Context.mine'
-- and friends.
--
-- @since 0.1.0.0
addContext :: Context.Store ctx -> IO ctx -> Wai.Middleware
addContext contextStore mkContext app = \request sendResponse -> do
  context <- mkContext
  Context.use contextStore context $ do
    app request sendResponse
