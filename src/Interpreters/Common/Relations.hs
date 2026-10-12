-- |
-- Module      : Interpreters.Common.Relations
-- Description : Judgments of other forms than e : T
--
-- A rule whose conclusion is not a typing judgment @e : T@ defines a
-- relation: subtyping (@a <: b@ or @Sub(a, b)@), kinding (@t :: k@), type
-- equality, effects, and so on. A relation judgment is a pair whose subject
-- is 'Types.relationSubject' and whose type is the relation term.
--
-- PAL derives a relation judgment by backward search with the rules of the
-- relation, like Prolog: it tries the rules in order of definition, unifies
-- the conclusion with the goal, and derives the premises in order. If a
-- premise fails, it goes back and tries the next possibility. The first
-- derivation found is kept.
module Interpreters.Common.Relations
  ( deriveRelation,
    relationRules,
    relationDepth,
  )
where

import Interpreters.Common.InferMonad
import Interpreters.Common.Terms
import Interpreters.Common.Unify
import qualified Types

-- | The deepest derivation of a relation, in rules.
relationDepth :: Int
relationDepth = 64

-- | The most steps for one derivation, so that a search stops in time.
relationSteps :: Int
relationSteps = 20000

-- | The rules of a relation (a name and a number of arguments), in order
--   of definition.
relationRules :: Types.Ctx -> String -> Int -> [Types.TypingRule]
relationRules ctx name n =
  [ rule
    | rule@(Types.TypingRule _ _ conclusion@(_, t)) <- reverse (Types.ctx'rules ctx),
      Types.isRelation conclusion,
      Types.relationName t == Just (name, n)
  ]

-- | Derive a relation judgment (the relation term). The unifications of the
--   first derivation found are kept; if there is none, the error is
--   'Types.NotDerivable'.
deriveRelation :: Types.Ctx -> Types.Type -> Infer Types.Derivation
deriveRelation ctx goal = do
  st <- getsI id
  case search goal relationDepth st relationSteps (\d st' _ -> Right (d, st')) of
    Right (d, st') -> modifyI (const st') >> pure d
    Left _ -> do
      s <- getsI inferSubst
      throwErr (Types.NotDerivable (forceTerm ctx (applySubst s goal)))
  where
    -- Search for a derivation of g; give each one to the continuation, and
    -- go back if the continuation fails. Left is the steps that are left.
    search g depth st steps k
      | steps <= 0 = Left 0
      | depth <= 0 = Left steps
      | otherwise = tryRules candidates (steps - 1)
      where
        g' = forceTerm ctx (applySubst (inferSubst st) g)
        candidates = maybe [] (uncurry (relationRules ctx)) (Types.relationName g')
        tryRules [] left = Left left
        tryRules (rule : rest) left = case useRule rule left of
          Right x -> Right x
          Left left' -> tryRules rest left'
        useRule rule left = case runInfer (freshen (ruleTypeVars rule)) st of
          (st1, Right metas) ->
            let Types.TypingRule name premises (_, conclusion) = renameRule metas rule
             in case runInfer (unify ctx conclusion g') st1 of
                  (st2, Right ()) -> premisesThen name [snd j | Types.Premise _ j <- premises] [] st2 left
                  _ -> Left left
          _ -> Left left
        -- Derive the premises in order; then the node for g.
        premisesThen name [] done st' left = k (Types.Derivation Types.relationSubject g (Types.ByRule name) [] (reverse done)) st' left
        premisesThen name (p : ps) done st' left =
          search p (depth - 1) st' left (\d st'' left' -> premisesThen name ps (d : done) st'' left')
