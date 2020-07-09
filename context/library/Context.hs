{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

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

    -- ** Asking for context
  , mine
  , mines

  , mineMay
  , minesMay

    -- * Exceptions
  , NotFoundException(NotFoundException, threadId)

    -- * Concurrency
  , module Context.Concurrent

    -- * Lower-level storage
  , module Context.Storage
  ) where

import Context.Concurrent
import Context.Internal (Store, mineMay, use)
import Context.Storage
import Control.Exception (Exception)
import Control.Monad ((<=<))
import GHC.Generics (Generic)
import Prelude
import qualified Context.Internal as Internal
import qualified Control.Exception as Exception

-- | An exception which may be thrown via 'mine', 'mines', and 'adjust' when the
-- calling thread does not have a registered context.
data NotFoundException = NotFoundException
  { threadId :: ThreadId
  } deriving stock (Eq, Generic, Show)
    deriving anyclass Exception

-- | Provides a new, non-empty 'Store' that uses the specified context value as a
-- default when the calling thread has no registered context. 'mine', 'mines',
-- and 'adjust' are guaranteed to never throw 'NotFoundException' when applied
-- to a non-empty 'Store'.
withNonEmptyStore :: ctx -> (Store ctx -> IO a) -> IO a
withNonEmptyStore = Internal.withStore defaultPropagation . Just

-- | Provides a new, empty 'Store'. 'mine', 'mines', and 'adjust' will throw
-- 'NotFoundException' when the calling thread has no registered context. Useful
-- when the 'Store' will contain context values that are always thread-specific.
withEmptyStore :: (Store ctx -> IO a) -> IO a
withEmptyStore = Internal.withStore defaultPropagation Nothing

-- | Adjust the calling thread's context in the specified 'Store' for the
-- duration of the specified action. Throws a 'NotFoundException' when the
-- calling thread has no registered context.
adjust :: Store ctx -> (ctx -> ctx) -> IO a -> IO a
adjust store f action = do
  adjustedContext <- mines store f
  use store adjustedContext action

-- | Provide the calling thread its current context from the specified
-- 'Store'. Throws a 'NotFoundException' when the calling thread has no
-- registered context.
mine :: Store ctx -> IO ctx
mine = maybe throwContextNotFound pure <=< mineMay

-- | Provide the calling thread a selection from its current context in the
-- specified 'Store'. Throws a 'NotFoundException' when the calling
-- thread has no registered context.
mines :: Store ctx -> (ctx -> a) -> IO a
mines store = maybe throwContextNotFound pure <=< minesMay store

-- | Provide the calling thread a selection from its current context in the
-- specified 'Store', if present.
minesMay :: Store ctx -> (ctx -> a) -> IO (Maybe a)
minesMay store selector = fmap (fmap selector) $ mineMay store

throwContextNotFound :: IO a
throwContextNotFound = do
  threadId <- myThreadId
  Exception.throwIO $ NotFoundException { threadId }

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
