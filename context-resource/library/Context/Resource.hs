{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Context.Resource
  ( -- * Introduction
    -- $intro

    -- * Provider
    Provider
  , withProvider

    -- * Operations
  , withResource
  , shareResource
  ) where

import Prelude
import qualified Context

-- | An opaque resource provider.
newtype Provider res = Provider
  { store :: Context.Store (WithRes res)
  }

newtype WithRes res = WithRes (forall r. (res -> IO r) -> IO r)

-- | Given a @with@-style function to acquire a resource, supplies the caller
-- with a resource 'Provider'. This 'Provider' should ideally be long-lived and
-- threaded throughout the application to the components that need to acquire
-- resources.
withProvider
  :: (forall r. (res -> IO r) -> IO r)
  -> (Provider res -> IO a)
  -> IO a
withProvider withRes f = do
  Context.withNonEmptyStore (WithRes withRes) \store -> do
    f Provider { store }

-- | Acquire a resource from the specified 'Provider', for the duration of the
-- specified action.
withResource :: Provider res -> (res -> IO a) -> IO a
withResource Provider { store } f = do
  WithRes withRes <- Context.mine store
  withRes f

-- | Tell the specified 'Provider' to share the specified resource for the
-- duration of the specified action.
shareResource :: Provider res -> res -> IO a -> IO a
shareResource Provider { store } resource action = do
  Context.use store (WithRes ($ resource)) action

-- $intro
--
-- This module provides a thread-safe, pool-compatible resource provider
-- abstraction that supports resource-sharing within nested actions. While it
-- was designed with resource pools in mind, the interface supports any
-- @with@-style means of acquiring a resource.
--
-- 'withResource' can be used to acquire a resource from the provider, and
-- 'shareResource' can be used to share a particular resource for the duration
-- of an action.
