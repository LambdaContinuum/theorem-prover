module PrettyPrinter (prettyProp, prettyTerm) where

import Data.List (intercalate)
import PropLanguage

prec :: Prop -> Int
prec (Forall _ _)  = 0
prec (Exists _ _)  = 0
prec (Iff    _ _)  = 1
prec (Implies _ _) = 2
prec (Or     _ _)  = 3
prec (And    _ _)  = 4
prec (Not    _)    = 5
prec _             = 10

prettyProp :: Prop -> String
prettyProp = go 0
  where
    go :: Int -> Prop -> String
    go minP p
      | prec p < minP = "(" ++ render p ++ ")"
      | otherwise     =        render p

    render :: Prop -> String
    render (Con True)     = "true"
    render (Con False)    = "false"
    render (Var s)        = s
    render (Pred s ts)    = s ++ "(" ++ intercalate ", " (map prettyTerm ts) ++ ")"
    render (And l r)      = go 4 l ++ " ∧ "   ++ go 5 r
    render (Or  l r)      = go 3 l ++ " ∨ "   ++ go 4 r
    render (Implies l r)  = go 3 l ++ " → "  ++ go 2 r
    render (Iff     l r)  = go 2 l ++ " ↔ " ++ go 1 r
    render (Not p)        = "¬" ++ go 6 p
    render (Forall x b)   = "∀" ++ x ++ ". " ++ go 0 b
    render (Exists x b)   = "∃" ++ x ++ ". " ++ go 0 b

prettyTerm :: Term -> String
prettyTerm (TVar s)    = s
prettyTerm (TFunc s ts) = s ++ "(" ++ intercalate ", " (map prettyTerm ts) ++ ")"

