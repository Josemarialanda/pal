-- |
-- Module      : Parser.Data
-- Description : The statements that a data declaration stands for
--
-- A declaration of an inductive type, e.g.
--
-- > data List<a> = Nil | Cons(a, List<a>)
--
-- stands for ordinary statements:
--
-- * the type: @type List\<a\>@;
-- * the constructors: @expr Nil : List\<a\>@ and a rule for @Cons@;
-- * the recursor @RecList@, which does case analysis and structural
--   recursion: for each constructor a branch, with the fields in scope and,
--   for each field of the type itself (a recursive field), the result of
--   the recursion on it;
-- * a reduction for each constructor, and a @value@ pattern.
--
-- For the example:
--
-- > rule RecList:
-- >   s : List<a>
-- >   nil : r
-- >   cons_1 : a, cons_2 : List<a>, cons_rec2 : r |- cons : r
-- > ->
-- >   RecList(s, nil, cons_1, cons_2, cons_rec2, cons) : r
-- >
-- > reduce RecListNil:  RecList(Nil, …) ~> nil
-- > reduce RecListCons: RecList(Cons(v1, v2), …) ~> cons[cons_1 := v1][cons_2 := v2][cons_rec2 := RecList(v2, …)]
module Parser.Data (expandData) where

import Data.Char (toLower)
import Data.List (foldl')
import Parser.Types (PalStmt (..))
import Types

-- | The statements for @data name<params> = constructors@, where each
--   constructor has a name and the types of its fields.
expandData :: String -> [String] -> [(String, [Type])] -> [PalStmt]
expandData name params constructors =
  [SType (TypeDecl name params)]
    <> fmap constructor constructors
    <> [SRule recursor]
    <> fmap reduction constructors
    <> fmap value constructors
  where
    self = TCon name (fmap TVar params)
    isRecursive = \case
      TCon n _ -> n == name
      _ -> False
    -- Names that do not clash with the parameters or with each other.
    result = TVar (fresh params "r")
    taken = params <> [n | TVar n <- [result]]
    scrutinee = fresh taken "s"
    branchName c = fresh (scrutinee : taken) (lowerFirst c)
    fieldName c i = fresh taken (lowerFirst c <> "_" <> show i)
    recName c i = fresh taken (lowerFirst c <> "_rec" <> show i)
    argName i = fresh (scrutinee : taken) ("v" <> show i)
    recursorName = "Rec" <> name
    -- The arguments of the recursor for one constructor: its fields, the
    -- results for its recursive fields, and its branch.
    branchArgs (c, fields) =
      [EVar (fieldName c i) | i <- [1 .. length fields]]
        <> [EVar (recName c i) | (i, t) <- zip [1 ..] fields, isRecursive t]
        <> [EVar (branchName c)]
    allBranchArgs = concatMap branchArgs constructors
    constructor (c, []) = SExpr (ExprDecl c self)
    constructor (c, fields) =
      SRule (TypingRule c [premise (EVar (fieldName c i)) t | (i, t) <- zip [1 ..] fields] (ECon c [EVar (fieldName c i) | i <- [1 .. length fields]], self))
    recursor =
      TypingRule
        recursorName
        (premise (EVar scrutinee) self : fmap branchPremise constructors)
        (ECon recursorName (EVar scrutinee : allBranchArgs), result)
    branchPremise (c, fields) =
      let hyps =
            [hyp (EVar (fieldName c i)) t | (i, t) <- zip [1 ..] fields]
              <> [hyp (EVar (recName c i)) result | (i, t) <- zip [1 ..] fields, isRecursive t]
       in Premise hyps (EVar (branchName c), result)
    reduction (c, fields) =
      let args = [EVar (argName i) | i <- [1 .. length fields]]
          lhs = ECon recursorName (ECon c args : allBranchArgs)
          recursion i = ECon recursorName (EVar (argName i) : allBranchArgs)
          substitutions =
            [(fieldName c i, a) | (i, a) <- zip [1 ..] args]
              <> [(recName c i, recursion i) | (i, t) <- zip [1 ..] fields, isRecursive t]
          rhs = foldl' (\body (x, e) -> Subst body (EVar x) e) (EVar (branchName c)) substitutions
       in SReduce (Reduction (recursorName <> c) lhs rhs)
    value (c, []) = SValue (ECon c [])
    value (c, fields) = SValue (ECon c [EVar (argName i) | i <- [1 .. length fields]])

-- | The name, or the name with a number, that is not taken.
fresh :: [String] -> String -> String
fresh taken base = case [n | n <- base : [base <> show i | i <- [1 :: Int ..]], n `notElem` taken] of
  n : _ -> n
  [] -> base -- unreachable: the list of candidates is infinite

lowerFirst :: String -> String
lowerFirst = \case
  c : rest -> toLower c : rest
  [] -> []
