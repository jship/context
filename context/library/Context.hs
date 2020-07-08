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

    -- * Lower-level storage
  , nonEmptyStore
  , emptyStore
  , setDefault
  ) where

import Context.Internal (Store, mineMay, setDefault, use)
import Control.Concurrent (ThreadId)
import Control.Exception (Exception)
import Control.Monad ((<=<))
import GHC.Generics (Generic)
import Prelude
import qualified Context.Internal as Internal
import qualified Control.Concurrent as Concurrent
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
withNonEmptyStore = Internal.withStore . Just

-- | Provides a new, empty 'Store'. 'mine', 'mines', and 'adjust' will throw
-- 'NotFoundException' when the calling thread has no registered context. Useful
-- when the 'Store' will contain context values that are always thread-specific.
withEmptyStore :: (Store ctx -> IO a) -> IO a
withEmptyStore = Internal.withStore Nothing

-- | Creates a new, non-empty 'Store' that uses the specified context value as a
-- default when the calling thread has no registered context. 'mine', 'mines',
-- and 'adjust' are guaranteed to never throw 'NotFoundException' when applied
-- to a non-empty 'Store'.
--
-- This is a lower-level function and is provided /only/ to support the use case
-- of creating a non-empty 'Store' as a global:
--
-- > store :: Store Int
-- > store = unsafePerformIO $ nonEmptyStore 42
-- > {-# NOINLINE store #-}
--
-- Outside of the global variable use case, 'withNonEmptyStore' should /always/
-- be preferred over this function.
nonEmptyStore :: ctx -> IO (Store ctx)
nonEmptyStore = Internal.newStore . Just

-- | Creates a new, empty 'Store'. 'mine', 'mines', and 'adjust' will throw
-- 'NotFoundException' when the calling thread has no registered context. Useful
-- when the 'Store' will contain context values that are always thread-specific.
--
-- This is a lower-level function and is provided /only/ to support the use case
-- of creating an empty 'Store' as a global:
--
-- > store :: Store Int
-- > store = unsafePerformIO emptyStore
-- > {-# NOINLINE store #-}
--
-- Outside of the global variable use case, 'withEmptyStore' should /always/
-- be preferred over this function.
emptyStore :: IO (Store ctx)
emptyStore = Internal.newStore Nothing

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
  threadId <- Concurrent.myThreadId
  Exception.throwIO $ NotFoundException { threadId }

-- $intro
--
-- This module provides an opaque 'Store' for thread-indexed storage around
-- arbitrary context values. The interface supports nesting context values per
-- thread, and at any point, the calling thread may ask for its current context.
--
-- Note that threads in Haskell have no explicit parent-child relationship. So if
-- you register a context in a 'Store' produced by 'withEmptyStore'/'emptyStore',
-- spin up a separate thread, and from that thread you ask for a context, that
-- thread will not have a context in the 'Store'. Use "Context.Concurrent" as a
-- drop-in replacement for "Control.Concurrent" to have the library handle context
-- propagation from one thread to another automatically. Otherwise, you must
-- explicitly register contexts from each thread when using a 'Store' produced by
-- 'withEmptyStore'/'emptyStore'.
--
-- If you have a default context that is always applicable to all threads, you may
-- wish to use 'withNonEmptyStore'/'nonEmptyStore'. All threads may access this
-- default context (without leveraging "Context.Concurrent" or explicitly
-- registering context for the threads) when using a 'Store' produced by
-- 'withNonEmptyStore'/'nonEmptyStore'.
--
-- Regardless of how you initialize your 'Store', every thread is free to nest its
-- own specific context values.
--
-- This module is designed to be imported qualified:
--
-- > import qualified Context
