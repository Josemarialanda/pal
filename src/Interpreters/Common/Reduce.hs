-- |
-- Module      : Interpreters.Common.Reduce
-- Description : Reductions: definitional equality
--
-- The user's @reduce@ statements as a reduction relation: one step at the
-- root ('stepRoot'), weak head normal form ('whnf') and normal form
-- ('normalForm'), with a step limit, and equality of types up to
-- computation ('equivalent').
module Interpreters.Common.Reduce where

import Control.Monad (ap)
import Data.Map (Map)
import qualified Data.Map as M
import Interpreters.Common.Terms
import qualified Types

-- | Are two types equal up to computation (their normal forms), up to
--   α-equivalence, and up to the names of their free type variables? So
--   @Arrow\<a, a\>@ and @Arrow\<b, b\>@ are equivalent, and so are
--   @Pi(x, Nat, Vec(x, Nat))@ and @Pi(y, Nat, Vec(y, Nat))@.
equivalent :: Types.Ctx -> Types.Type -> Types.Type -> Bool
equivalent ctx a b = case (normalForm ctx a, normalForm ctx b) of
  (Right a', Right b') -> canonical ctx a' == canonical ctx b'
  _ -> False

-- | The most reduction steps for one normalisation, so that reductions that
--   never stop give an error ('Types.ReductionLimit').
reductionLimit :: Int
reductionLimit = 10000

-- | A computation that uses up reduction steps.
newtype Reduce a = Reduce {runReduce :: Int -> Either Types.Err (Int, a)}

instance Functor Reduce where
  fmap f (Reduce g) = Reduce (fmap (fmap f) . g)

instance Applicative Reduce where
  pure a = Reduce (\n -> Right (n, a))
  (<*>) = ap

instance Monad Reduce where
  Reduce g >>= k = Reduce $ \n -> g n >>= \(n', a) -> runReduce (k a) n'

-- | Use up one step to reduce the given term.
tick :: Types.Term -> Reduce ()
tick t = Reduce $ \n -> if n <= 0 then Left (Types.ReductionLimit t) else Right (n - 1, ())

-- | Reduce a term to weak head normal form: reduce at the root until no
--   reduction applies there.
whnf :: Types.Ctx -> Types.Term -> Either Types.Err Types.Term
whnf ctx t = snd <$> runReduce (whnfR ctx t) reductionLimit

-- | Reduce a term to normal form: no reduction applies anywhere in it.
normalForm :: Types.Ctx -> Types.Term -> Either Types.Err Types.Term
normalForm ctx t = snd <$> runReduce (go t) reductionLimit
  where
    go term =
      whnfR ctx term >>= \case
        Types.Con b c ts -> Types.Con b c <$> traverse go ts
        other -> pure other

whnfR :: Types.Ctx -> Types.Term -> Reduce Types.Term
whnfR ctx term = case forceTerm ctx term of
  t@(Types.ECon _ _) ->
    stepRootR ctx t >>= \case
      Just t' -> tick t >> whnfR ctx t'
      Nothing -> pure t
  t -> pure t

-- | Apply the first reduction (in order of definition) that matches at the
--   root of a term, once. 'Nothing' if none matches.
stepRoot :: Types.Ctx -> Types.Term -> Either Types.Err (Maybe Types.Term)
stepRoot ctx t = snd <$> runReduce (stepRootR ctx (forceTerm ctx t)) reductionLimit

stepRootR :: Types.Ctx -> Types.Term -> Reduce (Maybe Types.Term)
stepRootR ctx t = go (reverse (Types.ctx'reductions ctx))
  where
    go [] = pure Nothing
    go (Types.Reduction _ lhs rhs : rest) =
      matchRoot ctx lhs t >>= \case
        Just env -> pure (Just (forceTerm ctx (substituteExpr env rhs)))
        Nothing -> go rest

-- | Match the left side of a reduction against a term. The constructor at
--   the root must be the same. Below the root, a subterm that does not
--   match is reduced to weak head normal form and matched again, so that
--   e.g. @App(Lam(x, t, b), a)@ matches @App(Id, a)@ when @Id@ reduces to a
--   @Lam@.
matchRoot :: Types.Ctx -> Types.Term -> Types.Term -> Reduce (Maybe (Map String Types.Term))
matchRoot ctx = \cases
  (Types.ECon pn ps) (Types.ECon tn ts) | pn == tn && length ps == length ts -> matchArgs M.empty (zip ps ts)
  _ _ -> pure Nothing
  where
    matchArgs env = \case
      [] -> pure (Just env)
      (p, t) : rest -> go env p t >>= maybe (pure Nothing) (`matchArgs` rest)
    go env (Types.EVar v) t = pure $ case M.lookup v env of
      Nothing -> Just (M.insert v t env)
      Just t'
        | t' == t -> Just env
        | otherwise -> Nothing
    go env p@(Types.ECon pn ps) t = case t of
      Types.ECon tn ts | pn == tn && length ps == length ts -> matchArgs env (zip ps ts)
      _ -> do
        t' <- whnfR ctx t
        if t' == t then pure Nothing else go env p t'
    go _ _ _ = pure Nothing
