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

import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Prelude
import qualified Context

-- | An opaque resource provider.
--
-- @since 0.1.0.0
newtype Provider m res = Provider
  { store :: Context.Store (WithRes m res)
  }

newtype WithRes m res = WithRes (forall r. (res -> m r) -> m r)

-- | Given a @with@-style function to acquire a resource, supplies the caller
-- with a resource 'Provider'. This 'Provider' should ideally be long-lived and
-- threaded throughout the application to the components that need to acquire
-- resources.
--
-- @since 0.1.0.0
withProvider
  :: forall m res a
   . (MonadIO m, MonadMask m)
  => (forall r. (res -> m r) -> m r)
  -> (Provider m res -> m a)
  -> m a
withProvider withRes f = do
  Context.withStore Context.noPropagation (Just (WithRes withRes)) \store -> do
    f Provider { store }

-- | Acquire a resource from the specified 'Provider', for the duration of the
-- specified action.
--
-- @since 0.1.0.0
withResource
  :: forall m res a
   . (MonadIO m, MonadThrow m)
  => Provider m res
  -> (res -> m a)
  -> m a
withResource Provider { store } f = do
  WithRes withRes <- Context.mine store
  withRes f

-- | Tell the specified 'Provider' to share the specified resource for the
-- duration of the specified action. All calls to 'withResource' (or
-- 'withSharedResource') within the action will return the shared resource.
--
-- @since 0.1.0.0
shareResource
  :: forall m res a
   . (MonadIO m, MonadMask m)
  => Provider m res
  -> res
  -> m a
  -> m a
shareResource Provider { store } resource action = do
  Context.use store (WithRes ($ resource)) action

-- | Acquire a resource from the specified 'Provider' and share that resource
-- for the duration of the specified action. All calls to 'withResource' (or
-- 'withSharedResource') within the action will return the shared resource.
--
-- This is a convenience function combining 'withResource' and 'shareResource'.
--
-- @since 0.1.0.0
withSharedResource
  :: forall m res a
   . (MonadIO m, MonadMask m)
  => Provider m res
  -> (res -> m a)
  -> m a
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
