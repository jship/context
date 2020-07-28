{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

module Context.Storage
  ( -- * Introduction
    -- $intro

    -- * Lower-level storage
    withStore
  , newStore
  , setDefault

    -- ** Propagation strategies
  , PropagationStrategy
  , defaultPropagation
  , noPropagation
  ) where

import Context.Internal (PropagationStrategy(LatestPropagation, NoPropagation), newStore, setDefault, withStore)

-- | The default 'PropagationStrategy'. For any 'Context.Store' initialized with this
-- 'PropagationStrategy', "Context.Concurrent" will automatically propagate the
-- "parent" thread's latest context value from this 'Context.Store' so that that context
-- is accessible in a newly-created thread.
--
-- @since 0.1.0.0
defaultPropagation :: PropagationStrategy
defaultPropagation = LatestPropagation

-- | This 'PropagationStrategy' does no propagation whatsoever. For any 'Context.Store'
-- initialized with this 'PropagationStrategy', "Context.Concurrent" will /not/
-- propagate the "parent" thread's context values from this 'Context.Store' in any way
-- to the newly-created thread.
--
-- @since 0.1.0.0
noPropagation :: PropagationStrategy
noPropagation = NoPropagation

-- $intro
--
-- This module provides lower-level functions for acquiring and working with
-- 'Context.Store' values. It may be useful for:
--
--   * library authors using a 'Context.Store' value(s) as an implementation
--     detail
--   * application developers leveraging a global 'Context.Store' value(s)
--
-- In any case, this module provides more fine-grained control over
-- 'Context.Store' creation. The most important aspect of this control is being
-- able to precisely specify how "Context.Concurrent" will behave in regards to
-- context propagation.
--
-- While this module is lower-level than the "Context" module, it is still
-- re-exported from "Context" out of convenience.
