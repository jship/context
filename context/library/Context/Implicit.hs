{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Context.Implicit
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

import Context
  ( NotFoundException(NotFoundException, threadId), Store, withEmptyStore, withNonEmptyStore
  )
import Context.Concurrent
import Context.Storage
import Context.View
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Prelude
import qualified Context

-- | Register a context in the implicit 'Store' on behalf of the calling
-- thread, for the duration of the specified action.
--
-- @since 0.1.0.0
use
  :: forall m ctx a
   . (MonadIO m, MonadMask m, ?contextStore :: Store ctx)
  => ctx
  -> m a
  -> m a
use = Context.use ?contextStore

-- | Adjust the calling thread's context in the implicit 'Store' for the
-- duration of the specified action. Throws a 'NotFoundException' when the
-- calling thread has no registered context.
--
-- @since 0.1.0.0
adjust
  :: forall m ctx a
   . (MonadIO m, MonadMask m, ?contextStore :: Store ctx)
  => (ctx -> ctx)
  -> m a
  -> m a
adjust = Context.adjust ?contextStore

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
   . (MonadIO m, MonadMask m, ?contextStore :: Store ctx)
  => (ctx -> ctx)
  -> (ctx -> m a)
  -> m a
withAdjusted = Context.withAdjusted ?contextStore

-- | Provide the calling thread its current context from the implicit
-- 'Store'. Throws a 'NotFoundException' when the calling thread has no
-- registered context.
--
-- @since 0.1.0.0
mine
  :: forall m ctx
   . (MonadIO m, MonadThrow m, ?contextStore :: Store ctx)
  => m ctx
mine = Context.mine ?contextStore

-- | Provide the calling thread a selection from its current context in the
-- implicit 'Store'. Throws a 'NotFoundException' when the calling
-- thread has no registered context.
--
-- @since 0.1.0.0
mines
  :: forall m ctx a
   . (MonadIO m, MonadThrow m, ?contextStore :: Store ctx)
  => (ctx -> a)
  -> m a
mines = Context.mines ?contextStore

-- | Provide the calling thread its current context from the implicit
-- 'Store', if present.
--
-- @since 0.1.0.0
mineMay
  :: forall m ctx
   . (MonadIO m, ?contextStore :: Store ctx)
  => m (Maybe ctx)
mineMay = Context.mineMay ?contextStore

-- | Provide the calling thread a selection from its current context in the
-- implicit 'Store', if present.
--
-- @since 0.1.0.0
minesMay
  :: forall m ctx a
   . (MonadIO m, ?contextStore :: Store ctx)
  => (ctx -> a)
  -> m (Maybe a)
minesMay = Context.minesMay ?contextStore

-- $intro
--
-- This module provides the same interface provided by "Context", but uses an
-- implicit 'Store' where applicable. Usage of this module requires that the
-- implicit parameter is named @contextStore@ in calling code.
--
-- This module is designed to be imported qualified:
--
-- > import qualified Context.Implicit
--
-- If you are only working with an implicit 'Store' in your application, you may
-- prefer shortening the import name:
--
-- > import qualified Context.Implicit as Context
--
-- Usage of this module might look something like this:
--
-- > main :: IO ()
-- > main = do
-- >   let config = -- ...
-- >   withDependencies config \deps -> do
-- >     Context.withNonEmptyStore deps \depsStore -> do
-- >       let ?contextStore = depsStore
-- >       doStuff
-- >
-- > doStuff :: (?contextStore :: Store Dependencies) => IO ()
-- > doStuff = do
-- >   deps <- Context.mine
-- >   -- ...
--
-- If the application is using a 'Store' in a global variable, then this 'Store'
-- can be conveniently injected in for use with this module via a global
-- implicit parameter:
--
-- > module Dependencies where
-- >
-- > import qualified Context.Implicit as Context
-- > import GHC.Classes(IP(ip)) -- from 'ghc-prim' package
-- >
-- > data Dependencies = -- ...
-- >
-- > depsStore :: Store Dependencies
-- > depsStore = unsafePerformIO $ Context.newStore Context.defaultPropagation Nothing
-- > {-# NOINLINE depsStore #-}
-- >
-- > instance IP "contextStore" (Store Dependencies) where
-- >   ip = depsStore
--
-- With a global implicit parameter, the @(?contextStore :: Store Dependencies)@
-- constraint does not need to be threaded throughout the application's
-- signatures, but it still can be overridden within local scopes as needed.
--
-- For an intro to global implicit parameters, see this post:
-- <https://kcsongor.github.io/global-implicit-parameters/>
