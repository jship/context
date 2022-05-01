{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Context.Internal
  ( -- * Disclaimer
    -- $disclaimer

    -- ** Store-related
    Store(Store, ref, key)
  , State(State, stacks, def)
  , NotFoundException(NotFoundException, threadId)
  , withStore
  , newStore
  , use
  , push
  , pop
  , mineMay
  , mineMayOnDefault
  , setDefault
  , throwContextNotFound

    -- ** View-related
  , View(MkView)
  , view
  , viewMay
  , toView

    -- ** Propagation-related
  , PropagationStrategy(NoPropagation, LatestPropagation)
  , Registry(Registry, ref)
  , AnyStore(MkAnyStore)
  , registry
  , emptyRegistry
  , withPropagator
  , withRegisteredPropagator
  , register
  , unregister

    -- ** Miscellaneous
  , bug
  ) where

import Control.Concurrent (ThreadId)
import Control.Exception (Exception)
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Unique (Unique)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Catch as Catch
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Traversable
import qualified Data.Unique as Unique

-- | Opaque type that manages thread-indexed storage of context values.
--
-- @since 0.1.0.0
data Store ctx = Store
  { ref :: IORef (State ctx)
  , key :: Unique
  }

data State ctx = State
  { stacks :: Map ThreadId [ctx]
  , def :: Maybe ctx
  }

-- | An exception which may be thrown when the calling thread does not have a
-- registered context.
--
-- @since 0.1.0.0
data NotFoundException = NotFoundException
  { threadId :: ThreadId
  } deriving stock (Eq, Generic, Show)
    deriving anyclass Exception

-- | The 'PropagationStrategy' controls the behavior used by
-- "Context.Concurrent" when propagating context from a "parent" thread to a new
-- thread.
--
-- @since 0.1.0.0
data PropagationStrategy
  = NoPropagation
  | LatestPropagation

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
-- > depsStore = unsafePerformIO $ Context.newStore Context.defaultPropagation Nothing
-- > {-# NOINLINE depsStore #-}
-- >
-- > main :: IO ()
-- > main = do
-- >   let config = -- ...
-- >   withDependencies config \deps -> do
-- >     Context.setDefault depsStore deps
-- >     -- ...
--
-- @since 0.1.0.0
setDefault
  :: forall m ctx
   . (MonadIO m)
  => Store ctx
  -> ctx
  -> m ()
setDefault Store { ref } context = do
  liftIO $ IORef.atomicModifyIORef' ref \state ->
    (state { def = Just context }, ())

throwContextNotFound
  :: forall m a
   . (MonadIO m, MonadThrow m)
  => m a
throwContextNotFound = do
  threadId <- liftIO $ Concurrent.myThreadId
  Catch.throwM $ NotFoundException { threadId }

-- | Provide the calling thread its current context from the specified
-- 'Store', if present.
--
-- @since 0.1.0.0
mineMay
  :: forall m ctx
   . (MonadIO m)
  => Store ctx
  -> m (Maybe ctx)
mineMay = mineMayOnDefault id

mineMayOnDefault
  :: forall m ctx
   . (MonadIO m)
  => (Maybe ctx -> Maybe ctx)
  -> Store ctx
  -> m (Maybe ctx)
mineMayOnDefault onDefault Store { ref } = do
  threadId <- liftIO $ Concurrent.myThreadId
  State { stacks, def } <- liftIO $ IORef.readIORef ref
  pure
    case Map.lookup threadId stacks of
      Nothing -> onDefault def
      Just [] -> bug "mineMayOnDefault"
      Just (context : _rest) -> Just context

-- | Register a context in the specified 'Store' on behalf of the calling
-- thread, for the duration of the specified action.
--
-- @since 0.1.0.0
use
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => Store ctx
  -> ctx
  -> m a
  -> m a
use store context =
  Catch.bracket_ (liftIO $ push store context) (liftIO $ pop store)

-- | Provides a new 'Store'. This is a lower-level function and is provided
-- mainly to give library authors more fine-grained control when using a 'Store'
-- as an implementation detail.
--
-- 'Context.withNonEmptyStore'/'Context.withEmptyStore' should generally be preferred over this
-- function when acquiring a 'Store'.
--
-- @since 0.1.0.0
withStore
  :: forall m ctx a
   . (MonadIO m, MonadMask m)
  => PropagationStrategy
  -- ^ The strategy used by "Context.Concurrent" for propagating context from a
  -- "parent" thread to a new thread.
  -> Maybe ctx
  -- ^ The default value for the 'Store'.
  --
  -- Providing a value will produce a non-empty 'Store' such that 'Context.mine',
  -- 'Context.mines', and 'Context.adjust' are guaranteed to never throw 'Context.NotFoundException'
  -- when applied to this 'Store'.
  --
  -- Providing 'Nothing' will produce an empty 'Store' such that 'Context.mine',
  -- 'Context.mines', and 'Context.adjust' will throw 'Context.NotFoundException' when the calling
  -- thread has no registered context. Providing 'Nothing' is useful when the
  -- 'Store' will contain context values that are always thread-specific.
  -> (Store ctx -> m a)
  -> m a
withStore propagationStrategy mContext f = do
  store <- newStore propagationStrategy mContext
  Catch.finally (f store) do
    case propagationStrategy of
      NoPropagation -> pure ()
      LatestPropagation -> liftIO $ unregister registry store

-- | Creates a new 'Store'. This is a lower-level function and is provided
-- /only/ to support the use case of creating a 'Store' as a global:
--
-- > store :: Store Int
-- > store = unsafePerformIO $ Context.newStore Context.defaultPropagation Nothing
-- > {-# NOINLINE store #-}
--
-- Outside of the global variable use case, 'Context.withNonEmptyStore',
-- 'Context.withEmptyStore', or even the lower-level
-- 'Context.Storage.withStore' should /always/ be preferred over this function
-- when acquiring a 'Store'.
--
-- @since 0.1.0.0
newStore
  :: forall m ctx
   . (MonadIO m)
  => PropagationStrategy
  -- ^ The strategy used by "Context.Concurrent" for propagating context from a
  -- "parent" thread to a new thread.
  -> Maybe ctx
  -- ^ The default value for the 'Store'.
  --
  -- Providing a value will produce a non-empty 'Store' such that 'Context.mine',
  -- 'Context.mines', and 'Context.adjust' are guaranteed to never throw 'Context.NotFoundException'
  -- when applied to this 'Store'.
  --
  -- Providing 'Nothing' will produce an empty 'Store' such that 'Context.mine',
  -- 'Context.mines', and 'Context.adjust' will throw 'Context.NotFoundException' when the calling
  -- thread has no registered context. Providing 'Nothing' is useful when the
  -- 'Store' will contain context values that are always thread-specific.
  -> m (Store ctx)
newStore propagationStrategy def = do
  key <- liftIO $ Unique.newUnique
  ref <- liftIO $ IORef.newIORef State { stacks = Map.empty, def }
  let store = Store { ref, key }
  case propagationStrategy of
    NoPropagation -> pure ()
    LatestPropagation -> liftIO $ register registry store
  pure store

push :: Store ctx -> ctx -> IO ()
push Store { ref } context = do
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

-- | A 'Context.View.View' provides a read-only view into a 'Context.Store'.
-- 'Context.View.View' trades the 'Context.Store' ability to register new
-- context for the ability to arbitrarily transform context values locally to
-- the 'Context.View.View'.
--
-- @since 0.1.1.0
data View ctx where
  MkView :: (ctx' -> ctx) -> Store ctx' -> View ctx

instance Functor View where
  fmap g (MkView f store) = MkView (g . f) store

-- | Provide the calling thread a view of its current context from the specified
-- 'Context.View.View'. Throws a 'Context.NotFoundException' when the calling
-- thread has no registered context.
--
-- @since 0.1.1.0
view :: (MonadIO m, MonadThrow m) => View ctx -> m ctx
view = maybe throwContextNotFound pure <=< viewMay

-- | Provide the calling thread a view of its current context from the specified
-- 'Context.View.View', if present.
--
-- @since 0.1.1.0
viewMay :: (MonadIO m) => View ctx -> m (Maybe ctx)
viewMay = \case
  MkView f store -> fmap (fmap f) $ mineMay store

-- | Create a 'Context.View.View' from the provided 'Context.Store'.
--
-- @since 0.1.1.0
toView :: Store ctx -> View ctx
toView = MkView id

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

-- $disclaimer
--
-- In general, changes to this module will not be reflected in the library's
-- version updates. Direct use of this module should be done with /extreme/ care
-- as it becomes very easy to violate the library's invariants.
