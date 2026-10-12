-- |
-- Module      : Interpreters.Common.InferMonad
-- Description : The state and monad of inference
--
-- The 'Infer' monad: state plus errors, where the state is kept when an
-- error is caught, so that failures recorded inside a failed region are not
-- lost. The state holds the substitution, the supply of fresh
-- metavariables, the premises being checked, the failures, the postponed
-- unifications and the scopes of the metavariables.
module Interpreters.Common.InferMonad where

import Control.Monad (ap, forM, forM_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Interpreters.Common.Terms
import qualified Types

-- | State threaded through inference.
data InferState = InferState
  { -- | Supply of fresh type variable names.
    inferSupply :: !Int,
    -- | The substitution solved so far.
    inferSubst :: !Subst,
    -- | The premises being checked, innermost first.
    inferFrames :: [Types.Frame],
    -- | The path of the subterm being checked.
    inferPath :: [Int],
    -- | Failures recorded so far, most recent first.
    inferFailures :: [Types.Failure],
    -- | Unifications that wait for a metavariable, most recent first.
    inferPostponed :: [Postponed],
    -- | For each metavariable, the rigid variables that its solution can
    --   mention (see 'scopeOf'). A name in a scope can be a metavariable
    --   that stands for the name of a binder.
    inferScopes :: !(Map String [String]),
    -- | Rigid variables that every metavariable can mention: the free
    --   variables of the type that a top-level @check@ checks against.
    inferRigid :: [String],
    -- | The linear and affine variables in scope (see 'Types.Usage').
    inferLinear :: Map String Types.Usage,
    -- | How often each of them has been used so far.
    inferUses :: Map String Int
  }

-- | The state at the start of inference.
initialState :: InferState
initialState = InferState 0 M.empty [] [] [] [] M.empty [] M.empty M.empty

-- | A unification that waits for a metavariable: one side is a
--   substitution @u[x := e]@ whose body @u@ is not solved yet. It is tried
--   again when a premise has been checked, and at the end of inference.
data Postponed = Postponed
  { postponed'ctx :: Types.Ctx,
    -- | The binders around it (see 'unifyUnder').
    postponed'bound :: [(String, String)],
    postponed'expected :: Types.Type,
    postponed'got :: Types.Type,
    -- | The stuck substitution, for the error if it stays stuck.
    postponed'stuck :: Types.Term,
    -- | Where it was postponed, for the error if it fails later.
    postponed'path :: [Int],
    postponed'frames :: [Types.Frame]
  }

-- | State plus errors, where the state is /kept/ when an error is caught,
--   so failures recorded inside a failed region are not lost.
newtype Infer a = Infer {runInfer :: InferState -> (InferState, Either Types.Err a)}

instance Functor Infer where
  fmap f (Infer g) = Infer (fmap (fmap f) . g)
  {-# INLINE fmap #-}

instance Applicative Infer where
  pure a = Infer (,Right a)
  {-# INLINE pure #-}
  (<*>) = ap

instance Monad Infer where
  Infer g >>= k = Infer $ \s -> case g s of
    (s', Left err) -> (s', Left err)
    (s', Right a) -> runInfer (k a) s'
  {-# INLINE (>>=) #-}

throwErr :: Types.Err -> Infer a
throwErr err = Infer (,Left err)

catchErr :: Infer a -> (Types.Err -> Infer a) -> Infer a
catchErr (Infer g) handler = Infer $ \s -> case g s of
  (s', Left err) -> runInfer (handler err) s'
  ok -> ok

fromEither :: Either Types.Err a -> Infer a
fromEither = either throwErr pure

{-# INLINE getsI #-}
getsI :: (InferState -> a) -> Infer a
getsI f = Infer (\s -> (s, Right (f s)))

{-# INLINE modifyI #-}
modifyI :: (InferState -> InferState) -> Infer ()
modifyI f = Infer (\s -> let s' = f s in s' `seq` (s', Right ()))

-- | Run an action while checking a premise, so failures report it.
withFrame :: Types.Frame -> Infer a -> Infer a
withFrame frame action = do
  modifyI (\s -> s {inferFrames = frame : inferFrames s})
  result <- action `catchErr` \err -> pop >> throwErr err
  pop
  pure result
  where
    pop = modifyI (\s -> s {inferFrames = drop 1 (inferFrames s)})

-- | Run an action at the given path, and with the given premises if any.
atPlace :: [Int] -> Maybe [Types.Frame] -> Infer a -> Infer a
atPlace path frames action = do
  (outerPath, outerFrames) <- getsI (\s -> (inferPath s, inferFrames s))
  modifyI (\s -> s {inferPath = path, inferFrames = fromMaybe outerFrames frames})
  result <- action `catchErr` \err -> restore outerPath outerFrames >> throwErr err
  restore outerPath outerFrames
  pure result
  where
    restore p fs = modifyI (\s -> s {inferPath = p, inferFrames = fs})

-- | Record a failure at the current path, with the current premises.
recordFailure :: Types.Ctx -> Types.Err -> Infer ()
recordFailure ctx err = do
  subst <- getsI inferSubst
  frames <- getsI inferFrames
  path <- getsI inferPath
  -- Show each premise with the types known when it failed. Apply each
  -- substitution whose body is known, e.g. Vec(n, Nat)[n := Zero].
  let frames' = fmap (\f -> f {Types.frame'premise = mapPremise (forceTerm ctx . applySubst subst) (Types.frame'premise f)}) frames
  modifyI (\s -> s {inferFailures = Types.Failure err path frames' : inferFailures s})

-- | Postpone a unification that waits for a metavariable (see 'Postponed').
postpone :: Types.Ctx -> [(String, String)] -> Types.Type -> Types.Type -> Types.Term -> Infer ()
postpone ctx bound expected got stuck = do
  path <- getsI inferPath
  frames <- getsI inferFrames
  modifyI (\s -> s {inferPostponed = Postponed ctx bound expected got stuck path frames : inferPostponed s})

-- | A fresh type variable. Names start with @'@, which the parser never
--   produces, so they cannot clash with user-written variables.
fresh :: Infer Types.Type
fresh = do
  n <- getsI inferSupply
  modifyI (\st -> st {inferSupply = n + 1})
  pure (Types.TVar ('\'' : show n))

-- | Replace the given type variables with fresh ones.
freshen :: [String] -> Infer Subst
freshen vs = M.fromList . zip vs <$> traverse (const fresh) vs

-- | Count a use of a variable from the context, if it is linear or affine.
countUse :: String -> Infer ()
countUse v = modifyI $ \s ->
  if M.member v (inferLinear s)
    then s {inferUses = M.insertWith (+) v 1 (inferUses s)}
    else s

-- | Run an action with the variables of some hypotheses in scope, each
--   with its usage (see 'Types.Usage'). Then check how often each linear
--   or affine one was used. A variable of the same name outside is hidden
--   for the action, and its count is kept.
withUsage :: [(String, Types.Usage)] -> Infer a -> Infer a
withUsage bound action = do
  saved <- getsI (\s -> [(v, M.lookup v (inferLinear s), M.lookup v (inferUses s)) | (v, _) <- bound])
  modifyI $ \s ->
    s
      { inferLinear = foldr bind (inferLinear s) bound,
        inferUses = foldr (M.delete . fst) (inferUses s) bound
      }
  result <- action `catchErr` \err -> restore saved >> throwErr err
  uses <- getsI inferUses
  restore saved
  forM_ bound $ \(v, u) -> check v u (M.findWithDefault 0 v uses)
  pure result
  where
    bind (v, Types.Unrestricted) = M.delete v
    bind (v, u) = M.insert v u
    restore saved = modifyI $ \s ->
      s
        { inferLinear = foldr (\(v, l, _) -> maybe (M.delete v) (M.insert v) l) (inferLinear s) saved,
          inferUses = foldr (\(v, _, n) -> maybe (M.delete v) (M.insert v) n) (inferUses s) saved
        }
    check v u n = case u of
      Types.Linear | n /= 1 -> throwErr (Types.UsageError v u n)
      Types.Affine | n > 1 -> throwErr (Types.UsageError v u n)
      _ -> pure ()

-- | Run the actions of premises that share the variables in scope (see
--   'Types.premise'share'): each starts from the same counts of uses, and
--   then each variable counts as used as often as in the action that used
--   it most.
sharing :: [Infer a] -> Infer [a]
sharing actions = do
  before <- getsI inferUses
  results <- forM actions $ \action -> do
    modifyI (\s -> s {inferUses = before})
    r <- action
    after <- getsI inferUses
    pure (r, after)
  modifyI (\s -> s {inferUses = M.unionsWith max (before : fmap snd results)})
  pure (fmap fst results)
