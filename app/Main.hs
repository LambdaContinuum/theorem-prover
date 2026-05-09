module Main where

import PropParser
import PrettyPrinter
import Text.Megaparsec (errorBundlePretty)
import Data.List (intercalate)

colWidth :: Int
colWidth = 50

pad :: String -> String
pad s = take colWidth (s ++ repeat ' ')

header :: String -> IO ()
header s = do
  putStrLn ""
  putStrLn $ "\x1b[1;34m" ++ "── " ++ s ++ " " ++ replicate (73 - length s) '─' ++ "\x1b[0m"

runProp :: String -> String -> IO ()
runProp label input = do
  let col1 = pad ("  " ++ input)
  case parseProp input of
    Left err ->
      putStrLn $ col1 ++ "  \x1b[31mFAIL\x1b[0m  " ++ takeWhile (/= '\n') (errorBundlePretty err)
    Right p  ->
      putStrLn $ col1 ++ "  \x1b[32m=>\x1b[0m  " ++ prettyProp p

runTerm :: String -> String -> IO ()
runTerm label input = do
  let col1 = pad ("  " ++ input)
  case parseTerm input of
    Left err ->
      putStrLn $ col1 ++ "  \x1b[31mFAIL\x1b[0m  " ++ takeWhile (/= '\n') (errorBundlePretty err)
    Right t  ->
      putStrLn $ col1 ++ "  \x1b[32m=>\x1b[0m  " ++ prettyTerm t

main :: IO ()
main = do
  putStrLn $ replicate 76 '─'
  putStrLn $ pad "  INPUT" ++ "  RESULT"
  putStrLn $ replicate 76 '─'

  header "atomic formulas"
  runProp "parses single predicate with one variable" "P(x)"
  runProp "parses binary predicate" "Q(x, y)"
  runProp "parses ternary predicate" "R(x, y, z)"
  runProp "parses predicate with constants" "Likes(alice, bob)"
  runProp "parses predicate with function term" "Equal(succ(zero), one)"
  runProp "parses predicate with nested function terms" "Greater(add(x, y), z)"
  runProp "parses predicate applied to function" "F(f(x))"
  runProp "parses predicate applied to nested functions" "P(g(f(x)))"
  header "negation"
  runProp "parses single negation" "¬P(x)"
  runProp "parses negation of binary predicate" "¬Q(x, y)"
  runProp "parses double negation" "¬¬P(x)"
  runProp "parses triple negation" "¬¬¬R(x, y, z)"
  runProp "parses negation with constant args" "¬Likes(alice, bob)"
  runProp "parses negation with function args" "¬Equal(f(x), g(y))"
  header "binary connectives"
  runProp "parses conjunction" "P(x) ∧ Q(x)"
  runProp "parses disjunction" "P(x) ∨ Q(x)"
  runProp "parses implication" "P(x) → Q(x)"
  runProp "parses biconditional" "P(x) ↔ Q(x)"
  runProp "parses conjunction with negated right" "P(x) ∧ ¬Q(x)"
  runProp "parses disjunction of negations" "¬P(x) ∨ ¬Q(x)"
  runProp "parses right-nested implication" "P(x) → (Q(x) → R(x))"
  runProp "parses left-nested implication" "(P(x) → Q(x)) → R(x)"
  runProp "parses triple conjunction" "P(x) ∧ Q(x) ∧ R(x)"
  runProp "parses triple disjunction" "P(x) ∨ Q(x) ∨ R(x)"
  runProp "parses conjunction distributing over disjunction" "P(x) ∧ (Q(x) ∨ R(x))"
  runProp "parses distributive law expansion" "(P(x) ∨ Q(x)) ∧ (P(x) ∨ R(x))"
  runProp "parses biconditional with conjunction body" "P(x) ↔ (Q(x) ∧ R(x))"
  runProp "parses biconditional of conjunctions" "(P(x) ∧ Q(x)) ↔ (R(x) ∧ S(x))"
  runProp "parses negation of conjunction" "¬(P(x) ∧ Q(x))"
  runProp "parses negation of disjunction" "¬(P(x) ∨ Q(x))"
  runProp "parses negation of implication" "¬(P(x) → Q(x))"
  header "universal quantifier"
  runProp "parses simple ∀" "∀ x. P(x)"
  runProp "parses ∀ with free variable in body" "∀ x. Q(x, y)"
  runProp "parses nested ∀" "∀ x. ∀ y. Q(x, y)"
  runProp "parses triple nested ∀" "∀ x. ∀ y. ∀ z. R(x, y, z)"
  runProp "parses ∀ implication" "∀ x. (P(x) → Q(x))"
  runProp "parses ∀ conjunction" "∀ x. (P(x) ∧ Q(x))"
  runProp "parses ∀ negation" "∀ x. ¬P(x)"
  runProp "parses ∀ curried implication" "∀ x. (P(x) → (Q(x) → R(x)))"
  runProp "parses symmetry of Likes" "∀ x. ∀ y. (Likes(x, y) → Likes(y, x))"
  runProp "parses mortality axiom" "∀ x. (Human(x) → Mortal(x))"
  runProp "parses subset axiom" "∀ x. (Dog(x) → Animal(x))"
  runProp "parses transitivity schema" "∀ x. ∀ y. ∀ z. (R(x, y) ∧ R(y, z) → R(x, z))"
  runProp "parses ∀ biconditional" "∀ x. (P(x) ↔ ¬Q(x))"
  header "existential quantifier"
  runProp "parses simple ∃" "∃ x. P(x)"
  runProp "parses ∃ with free variable" "∃ x. Q(x, y)"
  runProp "parses nested ∃" "∃ x. ∃ y. Q(x, y)"
  runProp "parses ∃ conjunction" "∃ x. (P(x) ∧ Q(x))"
  runProp "parses ∃ negation" "∃ x. ¬P(x)"
  runProp "parses ∃ with two predicates" "∃ x. (Human(x) ∧ Mortal(x))"
  runProp "parses asymmetric Likes witness" "∃ x. ∃ y. (Likes(x, y) ∧ ¬Likes(y, x))"
  runProp "parses ∃ conjunction with negation" "∃ x. (P(x) ∧ ¬Q(x))"
  runProp "parses two distinct witnesses" "∃ x. ∃ y. (P(x) ∧ P(y))"
  header "mixed quantifiers"
  runProp "parses ∀ ∃" "∀ x. ∃ y. Likes(x, y)"
  runProp "parses ∃ ∀" "∃ x. ∀ y. Likes(x, y)"
  runProp "parses ∀ ∃ with implication" "∀ x. ∃ y. (P(x) → Q(y))"
  runProp "parses ∃ ∀ with implication" "∃ x. ∀ y. (P(y) → Q(x))"
  runProp "parses ∀ ∃ Less" "∀ x. ∃ y. Less(x, y)"
  runProp "parses addition closure" "∀ x. ∀ y. ∃ z. Add(x, y, z)"
  runProp "parses alternating four quantifiers" "∀ x. ∃ y. ∀ z. (R(x, y) ∧ (R(y, z) → R(x, z)))"
  header "quantifiers interacting with connectives"
  runProp "parses ∀ implies ∃" "∀ x. P(x) → ∃ x. P(x)"
  runProp "parses law of excluded middle" "∀ x. (P(x) ∨ ¬P(x))"
  runProp "parses de Morgan: not-∃ vs ∀-not" "¬∃ x. P(x) ↔ ∀ x. ¬P(x)"
  runProp "parses de Morgan: not-∀ vs ∃-not" "¬∀ x. P(x) ↔ ∃ x. ¬P(x)"
  runProp "parses conjunction of ∀s" "∀ x. P(x) ∧ ∀ x. Q(x)"
  runProp "parses disjunction of ∃" "∃ x. P(x) ∨ ∃ x. Q(x)"
  runProp "parses chain of ∀ implications" "∀ x. (P(x) → Q(x)) ∧ ∀ x. (Q(x) → R(x))"
  runProp "parses modus ponens schema" "(∃ x. P(x)) ∧ (∀ x. (P(x) → Q(x))) → ∃ x. Q(x)"
  header "nested function terms"
  runProp "parses doubly nested function" "P(f(f(x)))"
  runProp "parses composed functions" "P(f(g(x)))"
  runProp "parses equality of composed functions" "Equal(f(g(x)), g(f(x)))"
  runProp "parses predicate with three function args" "R(f(x), g(y), h(z))"
  runProp "parses triply nested function" "P(f(f(f(x))))"
  runProp "parses Peano addition step" "Equal(add(succ(x), y), succ(add(x, y)))"
  runProp "parses ∀ inverse law" "∀ x. Equal(f(g(x)), x)"
  runProp "parses Peano recursive addition axiom" "∀ x. ∀ y. Equal(add(x, succ(y)), succ(add(x, y)))"
  runProp "parses ∃ fixed point" "∃ x. Equal(f(x), g(x))"
  header "equality axioms"
  runProp "parses reflexivity" "∀ x. Equal(x, x)"
  runProp "parses symmetry" "∀ x. ∀ y. (Equal(x, y) → Equal(y, x))"
  runProp "parses transitivity" "∀ x. ∀ y. ∀ z. (Equal(x, y) ∧ Equal(y, z) → Equal(x, z))"
  runProp "parses substitution" "∀ x. ∀ y. (Equal(x, y) → (P(x) → P(y)))"
  runProp "parses existence" "∃ x. Equal(x, x)"
  runProp "parses zero is not a successor" "∀ x. ¬Equal(succ(x), zero)"
  runProp "parses injectivity of succ" "∀ x. ∀ y. (Equal(succ(x), succ(y)) → Equal(x, y))"
  runProp "parses Peano addition base case" "∀ x. Equal(add(x, zero), x)"
  runProp "parses Peano addition recursive case" "∀ x. ∀ y. Equal(add(x, succ(y)), succ(add(x, y)))"
  header "edge cases and parenthesisation"
  runProp "parses vacuously true antecedent" "∀ x. (P(x) ∧ ¬P(x) → Q(x))"
  runProp "parses four-quantifier alternation" "∀ x. ∃ y. ∀ z. ∃ w. R(x, y, z, w)"
  runProp "parses quantifier with scoped conjunction" "∀ x. (P(x) ∧ Q(y))"
  runProp "parses tautology schema" "∀ x. (P(x) → P(x))"
  runProp "parses excluded middle" "∀ x. (P(x) ∨ ¬P(x))"
  runProp "parses deeply nested unary function" "P(f(g(h(x))))"
  runProp "parses formula with mixed bound and free vars" "∀ x. ∃ y. R(x, y, z)"
  runProp "parses left-nested conjunction" "((P(x) ∧ Q(x)) ∧ R(x)) ∧ S(x)"
  runProp "parses right-nested disjunction" "P(x) ∨ (Q(x) ∨ (R(x) ∨ S(x)))"
  runProp "parses biconditional of biconditionals" "(P(x) ↔ Q(x)) ↔ (R(x) ↔ S(x))"
  runProp "parses negated quantified biconditional" "¬(∀ x. (P(x) ↔ Q(x)))"
  runProp "parses constant-only predicate" "Likes(alice, bob)"
  runProp "parses ternary constant predicate" "R(alice, bob, carol)"

  header "boolean literals"
  runProp "parses true" "true"
  runProp "parses false" "false"
  runProp "parses negation of true" "¬true"
  runProp "parses negation of false" "¬false"
  runProp "parses double negation of true" "¬¬true"
  runProp "parses true ∧ false" "true ∧ false"
  runProp "parses true ∨ false" "true ∨ false"
  runProp "parses true → false" "true → false"
  runProp "parses true ↔ true" "true ↔ true"
  runProp "parses true ∧ P(x)" "true ∧ P(x)"
  runProp "parses P(x) ∨ false" "P(x) ∨ false"
  runProp "parses false → P(x)" "false → P(x)"
  runProp "parses true inside quantifier" "∀ x. (P(x) ∨ true)"
  runProp "parses false inside quantifier" "∃ x. (P(x) ∧ false)"
  runProp "parses true in implication body" "∀ x. (P(x) → true)"
  runProp "parses false in antecedent" "∀ x. (false → P(x))"
  runProp "parses nested boolean expression" "(true ∧ false) ∨ P(x)"
  runProp "parses true ∧ true ∧ true" "true ∧ true ∧ true"

  -- Terms
  header "terms in isolation (terms)"
  runTerm "parses lowercase variable" "x"
  runTerm "parses nullary constant" "zero"
  runTerm "parses unary function" "f(x)"
  runTerm "parses binary function" "add(x, y)"
  runTerm "parses nested function term" "f(g(x))"
  runTerm "parses succ of zero" "succ(zero)"
  runTerm "parses deeply nested term" "f(g(h(x)))"

  putStrLn ""
  putStrLn $ replicate 76 '─' 
