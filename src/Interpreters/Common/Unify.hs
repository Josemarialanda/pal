-- |
-- Module      : Interpreters.Common.Unify
-- Description : Unification
--
-- Unification of types: up to α-equivalence (with pairs of bound names),
-- up to reductions, with Miller's pattern fragment for substitutions, with
-- postponement of the problems that wait for a metavariable, and with
-- scope checks, so that a bound variable cannot escape its binder.
module Interpreters.Common.Unify where

import Control.Monad (forM_, unless, when)
import Data.List (intersect, nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Interpreters.Common.InferMonad
import Interpreters.Common.Reduce
import Interpreters.Common.Terms
import qualified Types

-- | Run an action; if it fails, record the failure (at the given path, with
--   the current premises) and carry on with 'Nothing'.
--
--   Then try the postponed unifications again, because the action can have
--   solved a metavariable that they wait for.
attempt :: Types.Ctx -> [Int] -> Infer a -> Infer (Maybe a)
attempt ctx path action = do
  result <- atPlace path Nothing ((Just <$> action) `catchErr` \err -> Nothing <$ recordFailure ctx err)
  retryPostponed
  pure result

-- | Try the postponed unifications again. One that is still stuck is
--   postponed again. One that fails is recorded where it was postponed.
retryPostponed :: Infer ()
retryPostponed = do
  postponed <- getsI inferPostponed
  modifyI (\s -> s {inferPostponed = []})
  forM_ (reverse postponed) $ \(Postponed ctx bound expected got _ path frames) ->
    atPlace path (Just frames) (unifyUnder ctx bound expected got `catchErr` recordFailure ctx)

-- | At the end of inference, try the postponed unifications again until
--   that solves no more metavariables. The ones that are still stuck are
--   failures.
settle :: Infer ()
settle = do
  pending <- getsI inferPostponed
  unless (null pending) $ do
    solved <- getsI (M.size . inferSubst)
    retryPostponed
    solved' <- getsI (M.size . inferSubst)
    if solved' > solved
      then settle
      else do
        stuck <- getsI inferPostponed
        modifyI (\s -> s {inferPostponed = []})
        forM_ (reverse stuck) $ \p ->
          atPlace (postponed'path p) (Just (postponed'frames p)) (recordFailure (postponed'ctx p) (Types.StuckSubstitution (postponed'stuck p)))

-- | Unify an expected type with an inferred one, extending the substitution.
--
--   Only metavariables (see 'isMeta') get bound. Terms compare up to
--   α-equivalence (see 'unifyArgs'). When the terms do not match, and a
--   reduction applies to either of them, both are reduced to weak head
--   normal form and compared again, so types are equal up to computation.
--   A unification with a substitution @u[x := e]@, where @u@ is not solved,
--   is solved if it is a pattern (see 'solvePattern'), or else
--   waits until @u@ is solved (see 'postpone').
unify :: Types.Ctx -> Types.Type -> Types.Type -> Infer ()
unify ctx = unifyUnder ctx []

-- | Unify under binders. Each pair is a name bound on the expected side and
--   the name bound at the same place on the inferred side, innermost first.
--   Two rigid variables are equal when they are bound by the same pair, or
--   when both are free and have the same name. A solution for a
--   metavariable is first written with the bound names of the
--   metavariable's own side.
unifyUnder :: Types.Ctx -> [(String, String)] -> Types.Type -> Types.Type -> Infer ()
unifyUnder ctx bound expected got = do
  s <- getsI inferSubst
  case (forceTerm ctx (applySubst s expected), forceTerm ctx (applySubst s got)) of
    (Types.TVar a, Types.TVar b) | sameVar a b -> pure ()
    (Types.TVar a, t) | isMeta a -> bindMeta ctx a (toExpected t)
    (t, Types.TVar a) | isMeta a -> bindMeta ctx a (toGot t)
    (e@(Types.TCon en ets), g@(Types.TCon gn gts))
      | en == gn && length ets == length gts -> do
          before <- getsI id
          unifyArgs ctx bound en ets gts `catchErr` \err -> convert e g (modifyI (const before)) (throwErr err)
    (e@(Types.Subst body x r), g) -> stuck body x r (toExpected g) e g e
    (e, g@(Types.Subst body x r)) -> stuck body x r (toGot e) e g g
    (e, g) -> convert e g (pure ()) (throwErr (Types.Mismatch e g))
  where
    sameVar a b = case (lookup a bound, lookup b [(r, l) | (l, r) <- bound]) of
      (Nothing, Nothing) -> a == b
      (inner, inner') -> inner == Just b && inner' == Just a
    -- A term from the other side, with the bound names of this side.
    toExpected = renameFree ctx (M.fromList [(r, l) | (l, r) <- reverse bound, l /= r])
    toGot = renameFree ctx (M.fromList [(l, r) | (l, r) <- reverse bound, l /= r])
    -- A substitution whose body is a metavariable: solve it if it is a
    -- pattern (see 'solvePattern'), or wait for the metavariable. Another
    -- substitution cannot be applied.
    stuck body x r other e g term = case body of
      Types.TVar m | isMeta m -> do
        solved <- solvePattern ctx m x r other
        unless solved (postpone ctx bound e g term)
      _ -> throwErr (Types.StuckSubstitution term)
    -- If a reduction applies to either side, reset the state and unify the
    -- weak head normal forms. Otherwise, give up.
    convert e g reset giveUp
      | null (Types.ctx'reductions ctx) = giveUp
      | otherwise = do
          e' <- fromEither (whnf ctx e)
          g' <- fromEither (whnf ctx g)
          if e' == e && g' == g then giveUp else reset >> unifyUnder ctx bound e' g'

-- | Solve @?m[x := y] = t@ if it is a pattern (Miller's pattern fragment):
--   the replacement @y@ is a rigid variable outside the scope of @?m@. Then
--   the only solution is @?m := t[y := x]@, if @x@ is not already free in
--   @t@. If @y@ is in the scope of @?m@ (or the replacement is not a
--   variable), @?m := t@ could also be a solution, so the problem is not
--   solved here. Returns whether it was solved.
solvePattern :: Types.Ctx -> String -> Types.Term -> Types.Term -> Types.Type -> Infer Bool
solvePattern ctx m x r t = case (x, r) of
  (Types.TVar xv, Types.TVar yv)
    | not (isMeta yv) -> do
        s <- getsI inferSubst
        scope <- getsI (M.lookup m . inferScopes)
        rigid <- getsI inferRigid
        let resolve v = case applySubst s (Types.TVar v) of
              Types.TVar n -> n
              _ -> v
            outside = maybe False (\names -> yv `notElem` fmap resolve names <> rigid) scope
            free = freeVars ctx t
        if not outside || (xv /= yv && xv `elem` free)
          then pure False
          else True <$ bindMeta ctx m (if xv == yv then t else renameFree ctx (M.singleton yv xv) t)
  _ -> pure False

-- | Record a solution for a metavariable, after the occurs check and the
--   scope check (see 'checkScope').
bindMeta :: Types.Ctx -> String -> Types.Type -> Infer ()
bindMeta ctx a t
  | t == Types.TVar a = pure ()
  | a `elem` freeTypeVars t = throwErr (Types.InfiniteType (Types.TVar a) t)
  | otherwise = do
      checkScope ctx a t
      modifyI $ \st ->
        st {inferSubst = M.insert a t (applySubst (M.singleton a t) <$> inferSubst st)}

-- | Unify the arguments of two applications of the same constructor.
--
--   A binder position (see 'binders') is not compared like the other
--   arguments. A metavariable there takes the other side's name. Two rigid
--   names make a new pair for the bodies that they bind (see
--   'unifyUnder'). Thus @Pi(x, Nat, Vec(x, a))@ and @Pi(y, Nat, Vec(y, a))@
--   unify.
unifyArgs :: Types.Ctx -> [(String, String)] -> String -> [Types.Type] -> [Types.Type] -> Infer ()
unifyArgs ctx bound name ets gts = do
  pairs <- catMaybes <$> traverse pairAt (nub (fmap fst bs))
  sequence_
    [ unifyUnder ctx ([pair | (i, pair) <- pairs, (i, k) `elem` bs] <> bound) e g
      | (k, e, g) <- zip3 [0 ..] ets gts,
        k `notElem` fmap fst pairs
    ]
  where
    bs = binders ctx name (length ets)
    pairAt i = do
      s <- getsI inferSubst
      case (applySubst s (ets !! i), applySubst s (gts !! i)) of
        (Types.TVar x, Types.TVar y) -> do
          if x /= y && isMeta x
            then bindMeta ctx x (Types.TVar y)
            else when (x /= y && isMeta y) (bindMeta ctx y (Types.TVar x))
          s' <- getsI inferSubst
          pure (Just (i, (resolveName s' x, resolveName s' y)))
        _ -> pure Nothing
    resolveName s v = case applySubst s (Types.TVar v) of
      Types.TVar n -> n
      _ -> v

-- | Give new metavariables their scopes: the variables in scope where they
--   are made (the hypotheses in the context), and the names that are bound
--   around all their places in the given terms (see 'boundAround').
declareScopes :: Types.Ctx -> [Types.Type] -> [([String], Types.Term)] -> Infer ()
declareScopes ctx metas places =
  modifyI $ \st -> st {inferScopes = M.fromList scopes <> inferScopes st}
  where
    around = boundAround ctx places
    scopes = [(m, M.keys (Types.ctx'env ctx) <> M.findWithDefault [] m around) | Types.TVar m <- metas]

-- | For each variable in some terms, the names that are bound around all
--   of its places. Each term comes with the names that are bound around it.
--   A variable in a binder position, or in the variable position of a
--   substitution, is bound around itself.
--
--   For example, in @Pi(x, t, u)@ the names around @u@ are @[x]@. If @u@ is
--   also used alone, its names are @[]@: a solution of @u@ cannot mention
--   @x@.
boundAround :: Types.Ctx -> [([String], Types.Term)] -> Map String [String]
boundAround ctx = M.fromListWith intersect . concatMap (uncurry go)
  where
    go bound = \case
      Types.Var v -> [(v, bound)]
      Types.Con _ c ts ->
        let bs = binders ctx c (length ts)
            boundIn k = [v | (i, j) <- bs, j == k, Types.Var v <- [ts !! i]]
         in concat
              [ case t of
                  Types.Var v | k `elem` fmap fst bs -> [(v, v : bound)]
                  _ -> go (boundIn k <> bound) t
                | (k, t) <- zip [0 ..] ts
              ]
      Types.Subst body x e ->
        go (exprVars x <> bound) body
          <> [(v, v : bound) | Types.Var v <- [x]]
          <> go bound e

-- | Check a solution @t@ for the metavariable @a@ before it is recorded.
--   Every rigid variable that is free in @t@ must be in the scope of @a@.
--   Otherwise the variable would escape its binder, e.g. in the rule
--   conclusion @Weird(x, t, body) : u@, where @body : u@ is checked with
--   @x@ in scope. Then the metavariables in @t@ get the same limit.
checkScope :: Types.Ctx -> String -> Types.Type -> Infer ()
checkScope ctx a t = do
  scopes <- getsI inferScopes
  rigid <- getsI inferRigid
  -- With the new solution, so that a scope can name a binder through a
  -- metavariable, also @a@ itself.
  s <- M.insert a t <$> getsI inferSubst
  forM_ (M.lookup a scopes) $ \scope -> do
    let resolve v = case applySubst s (Types.TVar v) of
          Types.TVar n -> Just n
          _ -> Nothing
        allowed = mapMaybe resolve scope <> rigid
    case [x | x <- freeVars ctx t, not (isMeta x), x `notElem` allowed] of
      x : _ -> throwErr (Types.ScopeEscape x t)
      [] -> do
        let local = binderNames ctx t
            keep v = case resolve v of
              Just n | not (isMeta n) -> n `elem` allowed || n `elem` local
              _ -> True
        modifyI $ \st -> st {inferScopes = foldr (M.adjust (filter keep)) (inferScopes st) (metaVars t)}
