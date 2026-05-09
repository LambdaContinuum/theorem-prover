module CNF where

import PropLanguage
import PrettyPrinter
import Data.List (intercalate)
import Control.Monad.State

type Literal = Prop
type Clause  = [Literal]
type CNF     = [Clause]

elimIff :: Prop -> Prop
elimIff (Iff p q)     = And (Implies (elimIff p) (elimIff q))
                            (Implies (elimIff q) (elimIff p))
elimIff (Not p)       = Not     (elimIff p)
elimIff (And p q)     = And     (elimIff p) (elimIff q)
elimIff (Or  p q)     = Or      (elimIff p) (elimIff q)
elimIff (Implies p q) = Implies (elimIff p) (elimIff q)
elimIff (Forall x p)  = Forall x (elimIff p)
elimIff (Exists x p)  = Exists x (elimIff p)
elimIff p             = p

negates :: Prop -> Prop -> Bool
negates (Not p) q = p == q
negates p (Not q) = p == q
negates _ _       = False

elimImpl :: Prop -> Prop
elimImpl (Implies p q) = Or (Not (elimImpl p)) (elimImpl q)
elimImpl (Not p)       = Not    (elimImpl p)
elimImpl (And p q)     = And    (elimImpl p) (elimImpl q)
elimImpl (Or  p q)     = Or     (elimImpl p) (elimImpl q)
elimImpl (Forall x p)  = Forall x (elimImpl p)
elimImpl (Exists x p)  = Exists x (elimImpl p)
elimImpl p             = p


toNNF :: Prop -> Prop
-- Double negation
toNNF (Not (Not p))      = toNNF p
-- De Morgan
toNNF (Not (And p q))    = Or  (toNNF (Not p)) (toNNF (Not q))
toNNF (Not (Or  p q))    = And (toNNF (Not p)) (toNNF (Not q))
-- Quantifier duality
toNNF (Not (Forall x p)) = Exists x (toNNF (Not p))
toNNF (Not (Exists x p)) = Forall x (toNNF (Not p))
-- Boolean constants under negation
toNNF (Not (Con b))      = Con (not b)
-- Negation already on an atom — leave it
toNNF (Not p)            = Not p
-- Recurse into everything else
toNNF (And p q)          = And     (toNNF p) (toNNF q)
toNNF (Or  p q)          = Or      (toNNF p) (toNNF q)
toNNF (Forall x p)       = Forall x (toNNF p)
toNNF (Exists x p)       = Exists x (toNNF p)
toNNF p                  = p

type SkolemM = State Int

fresh :: SkolemM String
fresh = do { n <- get; modify (+1); return ("sk" ++ show n) }

-- Substitute term variable x with term t everywhere in a proposition
substProp :: String -> Term -> Prop -> Prop
substProp x t (Pred s ts)    = Pred s (map (substTerm x t) ts)
substProp x t (Not p)        = Not (substProp x t p)
substProp x t (And p q)      = And (substProp x t p) (substProp x t q)
substProp x t (Or  p q)      = Or  (substProp x t p) (substProp x t q)
substProp x t (Forall y p)
  | x == y    = Forall y p                      -- x is shadowed, stop
  | otherwise = Forall y (substProp x t p)
substProp x t (Exists y p)
  | x == y    = Exists y p
  | otherwise = Exists y (substProp x t p)
substProp _ _ p              = p                -- Var, Con untouched

substTerm :: String -> Term -> Term -> Term
substTerm x t (TVar y)      = if x == y then t else TVar y
substTerm x t (TFunc f ts)  = TFunc f (map (substTerm x t) ts)

-- univs: universally-bound variables currently in scope
skolemize :: [String] -> Prop -> SkolemM Prop
skolemize univs (Forall x p) = Forall x <$> skolemize (univs ++ [x]) p
skolemize univs (Exists x p)  = do
  name <- fresh
  let skolemTerm = TFunc name (map TVar univs)   -- constant when univs = []
  skolemize univs (substProp x skolemTerm p)
skolemize univs (And p q)     = And <$> skolemize univs p <*> skolemize univs q
skolemize univs (Or  p q)     = Or  <$> skolemize univs p <*> skolemize univs q
skolemize univs (Not p)       = Not <$> skolemize univs p
skolemize _     p             = return p

runSkolem :: Prop -> Prop
runSkolem p = evalState (skolemize [] p) 0

dropUniversals :: Prop -> Prop
dropUniversals (Forall _ p) = dropUniversals p
dropUniversals (And p q)    = And (dropUniversals p) (dropUniversals q)
dropUniversals (Or  p q)    = Or  (dropUniversals p) (dropUniversals q)
dropUniversals (Not p)      = Not (dropUniversals p)
dropUniversals p            = p

distribute :: Prop -> Prop
distribute (And p q) = And (distribute p) (distribute q)
distribute (Or  p q) = distOr (distribute p) (distribute q)
distribute p         = p

-- Distribute one level of OR, then recurse since new ANDs may appear
distOr :: Prop -> Prop -> Prop
distOr (And p q) r = And (distOr p r) (distOr q r)
distOr p (And q r) = And (distOr p q) (distOr p r)
distOr p q         = Or p q

toLiteral :: Prop -> Literal
toLiteral p@(Var _)           = p
toLiteral p@(Pred _ _)        = p
toLiteral p@(Not (Var _))     = p
toLiteral p@(Not (Pred _ _))  = p
toLiteral p = error $ "toLiteral: not a literal: " ++ show p

collectClause :: Prop -> Clause
collectClause (Or p q) = collectClause p ++ collectClause q
collectClause p        = [toLiteral p]

collectCNF :: Prop -> CNF
collectCNF (And p q) = collectCNF p ++ collectCNF q
collectCNF p         = [collectClause p]

isTautology :: Clause -> Bool
isTautology clause = any (\l -> any (negates l) clause) clause

simplifyCNF :: CNF -> CNF
simplifyCNF = filter (not . isTautology)

toCNF :: Prop -> CNF
toCNF = simplifyCNF
      . collectCNF
      . distribute
      . dropUniversals
      . runSkolem
      . toNNF
      . elimImpl
      . elimIff


prettyClause :: Clause -> String
prettyClause [] = "⊥"
prettyClause ls = intercalate " or " (map prettyProp ls)

prettyCNF :: CNF -> String
prettyCNF [] = "⊤"
prettyCNF cs = intercalate ", " (map prettyClause cs)
