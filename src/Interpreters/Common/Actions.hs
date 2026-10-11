-- |
-- Module      : Interpreters.Common.Actions
-- Description : The PAL engine, shared by all interpreters
--
-- The core semantic actions of PAL: defining types, expressions, rules,
-- reductions, values and definitions, and inferring and checking types
-- with them. The interpreters ("Interpreters.Core", "Interpreters.Debug",
-- "Interpreters.IO") and the other parts of PAL use this module; it keeps
-- the semantics independent of the execution environment.
--
-- The engine is in these modules, in the order in which they build on
-- each other:
--
-- * "Interpreters.Common.Terms" — variables, binders and substitution;
-- * "Interpreters.Common.Reduce" — reductions (definitional equality);
-- * "Interpreters.Common.InferMonad" — the state and monad of inference;
-- * "Interpreters.Common.Unify" — unification;
-- * "Interpreters.Common.Inference" — inference and checking with the rules;
-- * "Interpreters.Common.Definitions" — definitions and their checks.
module Interpreters.Common.Actions
  ( module Interpreters.Common.Terms,
    module Interpreters.Common.Reduce,
    module Interpreters.Common.InferMonad,
    module Interpreters.Common.Unify,
    module Interpreters.Common.Inference,
    module Interpreters.Common.Definitions,
  )
where

import Interpreters.Common.Definitions
import Interpreters.Common.InferMonad
import Interpreters.Common.Inference
import Interpreters.Common.Reduce
import Interpreters.Common.Terms
import Interpreters.Common.Unify
