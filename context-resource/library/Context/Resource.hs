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

    -- * Core operations
  , withResource
  , shareResource

    -- * Convenience
  , withSharedResource
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
  Context.withStore Context.noPropagation (Just (WithRes withRes)) \store -> do
    f Provider { store }

-- | Acquire a resource from the specified 'Provider', for the duration of the
-- specified action.
withResource :: Provider res -> (res -> IO a) -> IO a
withResource Provider { store } f = do
  WithRes withRes <- Context.mine store
  withRes f

-- | Tell the specified 'Provider' to share the specified resource for the
-- duration of the specified action. All calls to 'withResource' (or
-- 'withSharedResource') within the action will return the shared resource.
shareResource :: Provider res -> res -> IO a -> IO a
shareResource Provider { store } resource action = do
  Context.use store (WithRes ($ resource)) action

-- | Acquire a resource from the specified 'Provider' and share that resource
-- for the duration of the specified action. All calls to 'withResource' (or
-- 'withSharedResource') within the action will return the shared resource.
--
-- This is a convenience function combining 'withResource' and 'shareResource'.
withSharedResource :: Provider res -> (res -> IO a) -> IO a
withSharedResource provider f = do
  withResource provider \resource -> do
    shareResource provider resource do
      f resource

-- $intro
--
-- This module provides a thread-safe, pool-compatible resource provider
-- abstraction that supports resource-sharing within nested actions. While it
-- was designed with resource pools in mind, the interface supports any
-- @with@-style means of acquiring a resource.
--
-- 'withResource' can be used to acquire a resource from the provider, and
-- 'shareResource' can be used to share a particular resource for the duration
-- of an action. Subsequent calls to 'shareResource' in that action are
-- idempotent. Note that if a resource-shared action spins up new threads, the
-- shared resource will /not/ be shared implicitly across thread boundaries.
--
-- While 'shareResource' offers the most control over resource-sharing,
-- 'withSharedResource' can be used as a convenience in the relatively common
-- case where a resource is acquired and then immediately shared within an
-- inner action.
