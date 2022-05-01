{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Context
  ( -- * Introduction
    -- $intro

    -- * Storage
    Store
  , withNonEmptyStore
  , withEmptyStore

    -- * Operations
    -- ** Registering context
  , use
  , adjust
  , withAdjusted

    -- ** Asking for context
  , mine
  , mines

  , mineMay
  , minesMay

    -- * Views
  , module Context.View

    -- * Exceptions
  , NotFoundException(NotFoundException, threadId)

    -- * Concurrency
  , module Context.Concurrent

    -- * Lower-level storage
  , module Context.Storage
  ) where

import Context.Concurrent
import Context.Internal (NotFoundException(NotFoundException, threadId), Store, mineMay, use)
import Context.Storage
import Context.View
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Prelude
import qualified Context.Internal as Internal

-- | Provides a new, non-empty 'Store' that uses the specified context value as a
-- default when the calling thread has no registered context. 'mine', 'mines',
-- and 'adjust' are guaranteed to never throw 'NotFoundException' when applied
-- to a non-empty 'Store'.
--
-- @since 0.1.0.0
withNonEmptyStore
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => ctx
  -> (Store ctx -> m a)
  -> m a
withNonEmptyStore = Internal.withStore defaultPropagation . Just

-- | Provides a new, empty 'Store'. 'mine', 'mines', and 'adjust' will throw
-- 'NotFoundException' when the calling thread has no registered context. Useful
-- when the 'Store' will contain context values that are always thread-specific.
--
-- @since 0.1.0.0
withEmptyStore
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => (Store ctx -> m a)
  -> m a
withEmptyStore = Internal.withStore defaultPropagation Nothing

-- | Adjust the calling thread's context in the specified 'Store' for the
-- duration of the specified action. Throws a 'NotFoundException' when the
-- calling thread has no registered context.
--
-- @since 0.1.0.0
adjust
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => Store ctx
  -> (ctx -> ctx)
  -> m a
  -> m a
adjust store f action = withAdjusted store f $ const action

-- | Convenience function to 'adjust' the context then supply the adjusted
-- context to the inner action. This function is equivalent to calling 'adjust'
-- and then immediately calling 'mine' in the inner action of 'adjust', e.g.:
--
-- > doStuff :: Store Thing -> (Thing -> Thing) -> IO ()
-- > doStuff store f = do
-- >   adjust store f do
-- >     adjustedThing <- mine store
-- >     ...
--
-- Throws a 'NotFoundException' when the calling thread has no registered
-- context.
--
-- @since 0.2.0.0
withAdjusted
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => Store ctx
  -> (ctx -> ctx)
  -> (ctx -> m a)
  -> m a
withAdjusted store f action = do
  adjustedContext <- mines store f
  use store adjustedContext $ action adjustedContext

-- | Provide the calling thread its current context from the specified
-- 'Store'. Throws a 'NotFoundException' when the calling thread has no
-- registered context.
--
-- @since 0.1.0.0
mine
  :: forall m ctx
   . (MonadIO m, MonadThrow m)
  => Store ctx
  -> m ctx
mine = maybe Internal.throwContextNotFound pure <=< mineMay

-- | Provide the calling thread a selection from its current context in the
-- specified 'Store'. Throws a 'NotFoundException' when the calling
-- thread has no registered context.
--
-- @since 0.1.0.0
mines
  :: forall m ctx a
   . (MonadIO m, MonadThrow m)
  => Store ctx
  -> (ctx -> a)
  -> m a
mines store = maybe Internal.throwContextNotFound pure <=< minesMay store

-- | Provide the calling thread a selection from its current context in the
-- specified 'Store', if present.
--
-- @since 0.1.0.0
minesMay
  :: forall m ctx a
   . (MonadIO m)
  => Store ctx
  -> (ctx -> a)
  -> m (Maybe a)
minesMay store selector = fmap (fmap selector) $ mineMay store

-- $intro
--
-- This module provides an opaque 'Store' for thread-indexed storage around
-- arbitrary context values. The interface supports nesting context values per
-- thread, and at any point, the calling thread may ask for its current context.
--
-- Note that threads in Haskell have no explicit parent-child relationship. So if
-- you register a context in a 'Store' produced by 'withEmptyStore', spin up a
-- separate thread, and from that thread you ask for a context, that thread will
-- not have a context in the 'Store'. Use "Context.Concurrent" as a drop-in
-- replacement for "Control.Concurrent" to have the library handle context
-- propagation from one thread to another automatically. Otherwise, you must
-- explicitly register contexts from each thread when using a 'Store' produced by
-- 'withEmptyStore'.
--
-- If you have a default context that is always applicable to all threads, you may
-- wish to use 'withNonEmptyStore'. All threads may access this default context
-- (without leveraging "Context.Concurrent" or explicitly registering context
-- for the threads) when using a 'Store' produced by 'withNonEmptyStore'.
--
-- Regardless of how you initialize your 'Store', every thread is free to nest its
-- own specific context values.
--
-- This module is designed to be imported qualified:
--
-- > import qualified Context
