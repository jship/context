{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Context.Concurrent
  ( -- * Introduction
    -- $intro

    -- * Control.Concurrent wrappers
    forkIO
  , forkFinally
  , forkIOWithUnmask
  , forkOn
  , forkOnWithUnmask
  , forkOS
  , forkOSWithUnmask
  , runInBoundThread
  , runInUnboundThread

    -- * Control.Concurrent re-exports
  , ThreadId
  , myThreadId
  , killThread
  , throwTo
  , getNumCapabilities
  , setNumCapabilities
  , threadCapability
  , yield
  , threadDelay
  , threadWaitRead
  , threadWaitWrite
  , threadWaitReadSTM
  , threadWaitWriteSTM
  , rtsSupportsBoundThreads
  , isCurrentThreadBound
  , mkWeakThreadId
  , module Control.Concurrent.MVar
  , module Control.Concurrent.Chan
  , module Control.Concurrent.QSem
  , module Control.Concurrent.QSemN
  ) where

import Control.Concurrent
  ( ThreadId, getNumCapabilities, isCurrentThreadBound, killThread, mkWeakThreadId, myThreadId
  , rtsSupportsBoundThreads, setNumCapabilities, threadCapability, threadDelay, threadWaitRead
  , threadWaitReadSTM, threadWaitWrite, threadWaitWriteSTM, throwTo, yield
  )
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import Control.Exception (SomeException)
import Prelude
import qualified Context.Internal as Internal
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception

-- | See 'Concurrent.forkIO'.
--
-- @since 0.1.0.0
forkIO :: IO () -> IO ThreadId
forkIO action = do
  Internal.withPropagator \propagate -> do
    Concurrent.forkIO $ propagate action

-- | See 'Concurrent.forkFinally'.
--
-- @since 0.1.0.0
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then = do
  -- N.B. We re-implement forkFinally instead of delegating directly to the
  -- Control.Concurrent function. This enables us to propagate context a single
  -- time to make it available to both the thread's main action and terminating
  -- action.
  Internal.withPropagator \propagate -> do
    Exception.mask \restore -> do
      Concurrent.forkIO $ propagate do
        Exception.try (restore action) >>= and_then

-- | See 'Concurrent.forkIOWithUnmask'.
--
-- @since 0.1.0.0
forkIOWithUnmask :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
forkIOWithUnmask io = do
  Internal.withPropagator \propagate -> do
    Concurrent.forkIOWithUnmask \restore -> do
      propagate $ io restore

-- | See 'Concurrent.forkOn'.
--
-- @since 0.1.0.0
forkOn :: Int -> IO () -> IO ThreadId
forkOn cpu action = do
  Internal.withPropagator \propagate -> do
    Concurrent.forkOn cpu $ propagate action

-- | See 'Concurrent.forkOnWithUnmask'.
--
-- @since 0.1.0.0
forkOnWithUnmask :: Int -> ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
forkOnWithUnmask cpu io = do
  Internal.withPropagator \propagate -> do
    Concurrent.forkOnWithUnmask cpu \restore -> do
      propagate $ io restore

-- | See 'Concurrent.forkOS'.
--
-- @since 0.1.0.0
forkOS :: IO () -> IO ThreadId
forkOS action = do
  Internal.withPropagator \propagate -> do
    Concurrent.forkOS $ propagate action

-- | See 'Concurrent.forkOSWithUnmask'.
--
-- @since 0.1.0.0
forkOSWithUnmask :: ((forall a. IO a -> IO a) -> IO ()) -> IO ThreadId
forkOSWithUnmask io = do
  Internal.withPropagator \propagate -> do
    Concurrent.forkOSWithUnmask \restore -> do
      propagate $ io restore

-- | See 'Concurrent.runInBoundThread'.
--
-- @since 0.1.0.0
runInBoundThread :: IO a -> IO a
runInBoundThread action =
  Internal.withPropagator \propagate -> do
    Concurrent.runInBoundThread $ propagate action

-- | See 'Concurrent.runInUnboundThread'.
--
-- @since 0.1.0.0
runInUnboundThread :: IO a -> IO a
runInUnboundThread action =
  Internal.withPropagator \propagate -> do
    Concurrent.runInUnboundThread $ propagate action

-- $intro
--
-- This module provides a "Context"-compatible interface around
-- "Control.Concurrent". Depending on the 'Context.Storage.PropagationStrategy'
-- of the 'Context.Store', the @fork*@ and @run*@ functions in this module can
-- automatically propagate the calling thread's latest registered contexts, if
-- any, over so that they are also available to the thread being created.
--
-- This module is designed to be a drop-in replacement for "Control.Concurrent" so
-- that users only have to import this module instead of both this module and
-- "Control.Concurrent". It is also re-exported from "Context" for convenience.
