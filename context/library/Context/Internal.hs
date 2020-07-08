{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

module Context.Internal
  ( Store
  , setDefault
  , mineMay
  , use
  , withStore
  , newStore
  , withPropagator
  ) where

import Control.Concurrent (ThreadId)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Unique (Unique)
import GHC.Stack (HasCallStack)
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Traversable
import qualified Data.Unique as Unique

-- | Opaque type that manages thread-indexed storage of context values.
data Store ctx = Store
  { ref :: IORef (State ctx)
  , key :: Unique
  }

data State ctx = State
  { stacks :: Map ThreadId [ctx]
  , def :: Maybe ctx
  }

-- | Set the default context value for a store. If the store was initialized as
-- an empty store, this function converts it to a non-empty store. If the store
-- was initialized as a non-empty store, this overwrites the default context
-- value.
--
-- One common use case for this function is to convert an empty store in a
-- global variable to a non-empty store while the application is
-- initializing/acquiring resources:
--
-- > depsStore :: Store Dependencies
-- > depsStore = unsafePerformIO Context.emptyStore
-- > {-# NOINLINE depsStore #-}
-- >
-- > main :: IO ()
-- > main = do
-- >   let config = -- ...
-- >   withDependencies config \deps -> do
-- >     Context.setDefault depsStore deps
-- >     -- ...
setDefault :: Store ctx -> ctx -> IO ()
setDefault Store { ref } context = do
  IORef.atomicModifyIORef' ref \state ->
    (state { def = Just context }, ())

-- | Provide the calling thread its current context from the specified
-- 'Store', if present.
mineMay :: Store ctx -> IO (Maybe ctx)
mineMay = mineMayOnDefault id

mineMayOnDefault :: (Maybe ctx -> Maybe ctx) -> Store ctx -> IO (Maybe ctx)
mineMayOnDefault onDefault Store { ref } = do
  threadId <- Concurrent.myThreadId
  State { stacks, def } <- IORef.readIORef ref
  pure
    case Map.lookup threadId stacks of
      Nothing -> onDefault def
      Just [] -> bug "mineMayOnDefault"
      Just (context : _rest) -> Just context

-- | Register a context in the specified 'Store' on behalf of the calling
-- thread, for the duration of the specified action.
use :: Store ctx -> ctx -> IO a -> IO a
use store context =
  Exception.bracket_ (push context store) (pop store)

withStore :: Maybe ctx -> (Store ctx -> IO a) -> IO a
withStore mContext f = do
  store <- newStore mContext
  Exception.finally (f store) $ unregister registry store

newStore :: Maybe ctx -> IO (Store ctx)
newStore def = do
  key <- Unique.newUnique
  ref <- IORef.newIORef State { stacks = Map.empty, def }
  let store = Store { ref, key }
  register registry store
  pure store

push :: ctx -> Store ctx -> IO ()
push context Store { ref } = do
  threadId <- Concurrent.myThreadId
  IORef.atomicModifyIORef' ref \state@State { stacks } ->
    case Map.lookup threadId stacks of
      Nothing ->
        (state { stacks = Map.insert threadId [context] stacks }, ())
      Just contexts ->
        (state { stacks = Map.insert threadId (context : contexts) stacks}, ())

pop :: Store ctx -> IO ()
pop Store { ref } = do
  threadId <- Concurrent.myThreadId
  IORef.atomicModifyIORef' ref \state@State { stacks } ->
    case Map.lookup threadId stacks of
      Nothing -> bug "pop-1"
      Just [] -> bug "pop-2"

      Just [_context] ->
        (state { stacks = Map.delete threadId stacks }, ())
      Just (_context : rest) ->
        (state { stacks = Map.insert threadId rest stacks }, ())

data AnyStore where
  MkAnyStore :: forall ctx. Store ctx -> AnyStore

newtype Registry = Registry
  { ref :: IORef (Map Unique AnyStore)
  }

registry :: Registry
registry = unsafePerformIO emptyRegistry
{-# NOINLINE registry #-}

emptyRegistry :: IO Registry
emptyRegistry = do
  ref <- IORef.newIORef Map.empty
  pure Registry { ref }

withPropagator :: ((IO a -> IO a) -> IO b) -> IO b
withPropagator = withRegisteredPropagator registry

-- The with-style here is not necessary but it helps keep calling code honest by
-- encouraging not holding onto the propagator any longer than needed. It also
-- makes the signature compatible if the registry's state is ever changed to an
-- MVar and withMVar is used within this function. At this time, an IORef is
-- sufficient because while other stores could be registered after the calling
-- thread reads from the IORef, it would be impossible for the calling thread to
-- have any contexts in those new stores, so there would be nothing to propagate
-- from them.
withRegisteredPropagator :: Registry -> ((IO a -> IO a) -> IO b) -> IO b
withRegisteredPropagator Registry { ref } f = do
  stores <- IORef.readIORef ref
  propagator <- do
    fmap (foldr (.) id) do
      Traversable.for stores \case
        MkAnyStore store -> do
          -- N.B. When propagating context and the "parent" thread doesn't
          -- have any specific context in this particular store but there is
          -- a default, if we just used mineMay directly we would grab the
          -- default and then unnecessarily propagate it as a specific context
          -- for the new thread. Here we override the default value to Nothing
          -- as an optimization.
          mineMayOnDefault (const Nothing) store >>= \case
            Nothing -> pure id
            Just context -> pure $ use store context
  f propagator

register :: Registry -> Store ctx -> IO ()
register Registry { ref } store@Store { key } = do
  IORef.atomicModifyIORef' ref \stores ->
    (Map.insert key (MkAnyStore store) stores, ())

unregister :: Registry -> Store ctx -> IO ()
unregister Registry { ref } Store { key } = do
  IORef.atomicModifyIORef' ref \stores ->
    (Map.delete key stores, ())

bug :: HasCallStack => String -> a
bug prefix =
  error
    $ "Context." <> prefix <> ": Impossible! (if you see this message, please "
        <> "report it as a bug at https://github.com/jship/context)"
