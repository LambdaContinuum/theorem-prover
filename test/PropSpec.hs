module PropSpec where

import Test.Hspec
import Data.Either (isLeft)
import Text.Megaparsec (errorBundlePretty)
import PropParser
import PrettyPrinter


shouldFail :: String -> Expectation
shouldFail s = parseProp s `shouldSatisfy` isLeft

shouldFailWith :: String -> String -> Expectation
shouldFailWith input fragment =
  case parseProp input of
    Right p  -> expectationFailure $
      "Expected parse failure for " ++ show input ++
      " but succeeded with: " ++ show p
    Left err -> errorBundlePretty err `shouldContain` fragment


shouldFailTerm :: String -> Expectation
shouldFailTerm s = parseTerm s `shouldSatisfy` isLeft

shouldParse :: String -> Expectation
shouldParse s = parseProp s `shouldSatisfy` either (const False) (const True)

shouldParseTerm :: String -> Expectation
shouldParseTerm s = parseTerm s `shouldSatisfy` either (const False) (const True)


spec :: Spec
spec = do
 
  describe "empty and whitespace" $ do
    it "rejects empty string"                             $ shouldFail ""
    it "rejects whitespace only"                          $ shouldFail "   "
    it "rejects tab only"                                 $ shouldFail "\t"

  describe "dangling binary operators" $ do
    it "rejects leading ∧"                                $ shouldFail "∧ Q(x)"
    it "rejects trailing ∧"                               $ shouldFail "P(x) ∧"
    it "rejects leading ∨"                                $ shouldFail "∨ Q(x)"
    it "rejects trailing ∨"                               $ shouldFail "P(x) ∨"
    it "rejects leading →"                                $ shouldFail "→ Q(x)"
    it "rejects trailing →"                               $ shouldFail "P(x) →"
    it "rejects leading ↔"                                $ shouldFail "↔ Q(x)"
    it "rejects trailing ↔"                               $ shouldFail "P(x) ↔"
    it "rejects consecutive ∧"                            $ shouldFail "P(x) ∧ ∧ Q(x)"
    it "rejects consecutive →"                            $ shouldFail "P(x) → → Q(x)"
    it "rejects consecutive ∨"                            $ shouldFail "P(x) ∨ ∨ Q(x)"

  describe "dangling negation" $ do
    it "rejects bare ¬"                                   $ shouldFail "¬"
    it "rejects ¬ with empty parens"                      $ shouldFail "¬()"
    it "rejects ¬ with no operand after ∧"                $ shouldFail "P(x) ∧ ¬"

  describe "mismatched parentheses" $ do
    it "rejects unclosed left paren"                      $ shouldFail "(P(x) ∧ Q(x)"
    it "rejects extra right paren"                        $ shouldFail "P(x) ∧ Q(x))"
    it "rejects double-opened paren"                      $ shouldFail "((P(x) ∧ Q(x))"
    it "rejects empty parens"                             $ shouldFail "()"
    it "rejects paren with only whitespace"               $ shouldFail "(   )"

  describe "wrong case" $ do
    it "rejects bare lowercase identifier"                $ shouldFail "p"
    it "rejects lowercase as left operand"                $ shouldFail "p ∧ Q(x)"
    it "rejects lowercase predicate"                      $ shouldFail "pred(x)"

  describe "trailing garbage" $ do
    it "rejects prop followed by bare identifier"         $ shouldFail "P(x) Q(x)"
    it "rejects prop followed by stray symbol"            $ shouldFail "P(x) ∧ Q(x) ."
    it "rejects prop followed by stray dot"               $ shouldFail "P(x)."
    it "rejects prop followed by stray comma"             $ shouldFail "P(x),"

  describe "bare operators as propositions" $ do
    it "rejects bare ∧"                                   $ shouldFail "∧"
    it "rejects bare ∨"                                   $ shouldFail "∨"
    it "rejects bare →"                                   $ shouldFail "→"
    it "rejects bare ↔"                                   $ shouldFail "↔"
    it "rejects bare ¬"                                   $ shouldFail "¬"
    it "rejects bare ∀"                                   $ shouldFail "∀"
    it "rejects bare ∃"                                   $ shouldFail "∃"

  describe "malformed quantifiers" $ do
    it "rejects ∀ with no variable"                       $ shouldFail "∀. P(x)"
    it "rejects ∀ with no dot"                            $ shouldFail "∀ x P(x)"
    it "rejects ∀ with no body"                           $ shouldFail "∀ x."
    it "rejects ∃ with no variable"                       $ shouldFail "∃. P(x)"
    it "rejects ∃ with no dot"                            $ shouldFail "∃ x P(x)"
    it "rejects ∃ with no body"                           $ shouldFail "∃ x."
    it "rejects ∀ with uppercase variable"                $ shouldFail "∀ X. P(X)"
    it "rejects ∃ with uppercase variable"                $ shouldFail "∃ X. P(X)"
    it "rejects nested quantifier missing inner body"     $ shouldFail "∀ x. ∃ y."
    it "rejects ∀ with digit variable"                    $ shouldFail "∀ 1x. P(x)"
    it "rejects ∀ with reserved word as variable"         $ shouldFail "∀ true. P(x)"

  describe "malformed predicate argument lists" $ do
    it "rejects leading comma in args"                    $ shouldFail "P(, x)"
    it "rejects trailing comma in args"                   $ shouldFail "P(x,)"
    it "rejects double comma in args"                     $ shouldFail "P(x,, y)"
    it "rejects args without comma separator"             $ shouldFail "P(x y)"
    it "rejects unclosed argument list"                   $ shouldFail "P(x, y"

  describe "invalid identifiers" $ do
    it "rejects digit-starting identifier"                $ shouldFail "1P"
    it "rejects identifier with @"                        $ shouldFail "P@Q"
    it "rejects identifier starting with underscore"      $ shouldFail "_P"
    it "rejects identifier with hyphen"                   $ shouldFail "P-Q"

  describe "reserved words as term variables" $ do
    it "rejects 'true' as term variable"    $ "P(true)"  `shouldFailWith` "'true' is a reserved keyword"
    it "rejects 'false' as term variable"   $ "P(false)" `shouldFailWith` "'false' is a reserved keyword"

  describe "quantifier structure error messages" $ do
    it "reports missing lowercase variable after ∀"  $ "∀ X. P(x)" `shouldFailWith` "bound variable (must be lowercase)"
    it "reports missing lowercase variable after ∃"  $ "∃ X. P(x)" `shouldFailWith` "bound variable (must be lowercase)"
    it "reports missing dot after ∀ variable"        $ "∀ x P(x)"  `shouldFailWith` "'.' after bound variable"
    it "reports missing dot after ∃ variable"        $ "∃ x P(x)"  `shouldFailWith` "'.' after bound variable"
    it "reports missing body after ∀ dot"            $ "∀ x."      `shouldFailWith` "proposition body after '.'"
    it "reports missing body after ∃ dot"            $ "∃ x."      `shouldFailWith` "proposition body after '.'"

  describe "missing operand error messages" $ do
    it "reports missing right operand for ∧"  $ "P(x) ∧"   `shouldFailWith` "proposition"
    it "reports missing right operand for ∨"  $ "P(x) ∨"   `shouldFailWith` "proposition"
    it "reports missing right operand for →"  $ "P(x) →"   `shouldFailWith` "proposition"
    it "reports missing right operand for ↔"  $ "P(x) ↔"   `shouldFailWith` "proposition"
    it "reports missing operand for bare ¬"   $ "¬"         `shouldFailWith` "proposition"
    it "reports missing operand for ¬ after ∧" $ "P(x) ∧ ¬" `shouldFailWith` "proposition"

  describe "malformed terms in isolation" $ do
    it "rejects uppercase term variable"                  $ shouldFailTerm "X"
    it "rejects term starting with digit"                 $ shouldFailTerm "1x"
    it "rejects empty term"                               $ shouldFailTerm ""
    it "rejects function with trailing comma"             $ shouldFailTerm "f(x,)"
    it "rejects function with no closing paren"           $ shouldFailTerm "f(x, y"
    it "rejects function with leading comma"              $ shouldFailTerm "f(, x)"
    it "rejects function with double comma"               $ shouldFailTerm "f(x,, y)"

  describe "atomic formulas" $ do
    it "parses single predicate with one variable"        $ shouldParse "P(x)"
    it "parses binary predicate"                          $ shouldParse "Q(x, y)"
    it "parses ternary predicate"                         $ shouldParse "R(x, y, z)"
    it "parses predicate with constants"                  $ shouldParse "Likes(alice, bob)"
    it "parses predicate with function term"              $ shouldParse "Equal(succ(zero), one)"
    it "parses predicate with nested function terms"      $ shouldParse "Greater(add(x, y), z)"
    it "parses predicate applied to function"             $ shouldParse "F(f(x))"
    it "parses predicate applied to nested functions"     $ shouldParse "P(g(f(x)))"
 
  describe "negation" $ do
    it "parses single negation"                           $ shouldParse "¬P(x)"
    it "parses negation of binary predicate"              $ shouldParse "¬Q(x, y)"
    it "parses double negation"                           $ shouldParse "¬¬P(x)"
    it "parses triple negation"                           $ shouldParse "¬¬¬R(x, y, z)"
    it "parses negation with constant args"               $ shouldParse "¬Likes(alice, bob)"
    it "parses negation with function args"               $ shouldParse "¬Equal(f(x), g(y))"
 
  describe "binary connectives" $ do
    it "parses conjunction"                               $ shouldParse "P(x) ∧ Q(x)"
    it "parses disjunction"                               $ shouldParse "P(x) ∨ Q(x)"
    it "parses implication"                               $ shouldParse "P(x) → Q(x)"
    it "parses biconditional"                             $ shouldParse "P(x) ↔ Q(x)"
    it "parses conjunction with negated right"            $ shouldParse "P(x) ∧ ¬Q(x)"
    it "parses disjunction of negations"                  $ shouldParse "¬P(x) ∨ ¬Q(x)"
    it "parses right-nested implication"                  $ shouldParse "P(x) → (Q(x) → R(x))"
    it "parses left-nested implication"                   $ shouldParse "(P(x) → Q(x)) → R(x)"
    it "parses triple conjunction"                        $ shouldParse "P(x) ∧ Q(x) ∧ R(x)"
    it "parses triple disjunction"                        $ shouldParse "P(x) ∨ Q(x) ∨ R(x)"
    it "parses conjunction distributing over disjunction" $ shouldParse "P(x) ∧ (Q(x) ∨ R(x))"
    it "parses distributive law expansion"                $ shouldParse "(P(x) ∨ Q(x)) ∧ (P(x) ∨ R(x))"
    it "parses biconditional with conjunction body"       $ shouldParse "P(x) ↔ (Q(x) ∧ R(x))"
    it "parses biconditional of conjunctions"             $ shouldParse "(P(x) ∧ Q(x)) ↔ (R(x) ∧ S(x))"
    it "parses negation of conjunction"                   $ shouldParse "¬(P(x) ∧ Q(x))"
    it "parses negation of disjunction"                   $ shouldParse "¬(P(x) ∨ Q(x))"
    it "parses negation of implication"                   $ shouldParse "¬(P(x) → Q(x))"
 
  describe "universal quantifier" $ do
    it "parses simple ∀"                             $ shouldParse "∀ x. P(x)"
    it "parses ∀ with free variable in body"         $ shouldParse "∀ x. Q(x, y)"
    it "parses nested ∀"                             $ shouldParse "∀ x. ∀ y. Q(x, y)"
    it "parses triple nested ∀"                      $ shouldParse "∀ x. ∀ y. ∀ z. R(x, y, z)"
    it "parses ∀ implication"                        $ shouldParse "∀ x. (P(x) → Q(x))"
    it "parses ∀ conjunction"                        $ shouldParse "∀ x. (P(x) ∧ Q(x))"
    it "parses ∀ negation"                           $ shouldParse "∀ x. ¬P(x)"
    it "parses ∀ curried implication"                $ shouldParse "∀ x. (P(x) → (Q(x) → R(x)))"
    it "parses symmetry of Likes"                         $ shouldParse "∀ x. ∀ y. (Likes(x, y) → Likes(y, x))"
    it "parses mortality axiom"                           $ shouldParse "∀ x. (Human(x) → Mortal(x))"
    it "parses subset axiom"                              $ shouldParse "∀ x. (Dog(x) → Animal(x))"
    it "parses transitivity schema"                       $ shouldParse "∀ x. ∀ y. ∀ z. (R(x, y) ∧ R(y, z) → R(x, z))"
    it "parses ∀ biconditional"                      $ shouldParse "∀ x. (P(x) ↔ ¬Q(x))"
 
  describe "existential quantifier" $ do
    it "parses simple ∃"                             $ shouldParse "∃ x. P(x)"
    it "parses ∃ with free variable"                 $ shouldParse "∃ x. Q(x, y)"
    it "parses nested ∃"                             $ shouldParse "∃ x. ∃ y. Q(x, y)"
    it "parses ∃ conjunction"                        $ shouldParse "∃ x. (P(x) ∧ Q(x))"
    it "parses ∃ negation"                           $ shouldParse "∃ x. ¬P(x)"
    it "parses ∃ with two predicates"                $ shouldParse "∃ x. (Human(x) ∧ Mortal(x))"
    it "parses asymmetric Likes witness"                  $ shouldParse "∃ x. ∃ y. (Likes(x, y) ∧ ¬Likes(y, x))"
    it "parses ∃ conjunction with negation"          $ shouldParse "∃ x. (P(x) ∧ ¬Q(x))"
    it "parses two distinct witnesses"                    $ shouldParse "∃ x. ∃ y. (P(x) ∧ P(y))"
 
  describe "mixed quantifiers" $ do
    it "parses ∀ ∃"                             $ shouldParse "∀ x. ∃ y. Likes(x, y)"
    it "parses ∃ ∀"                             $ shouldParse "∃ x. ∀ y. Likes(x, y)"
    it "parses ∀ ∃ with implication"            $ shouldParse "∀ x. ∃ y. (P(x) → Q(y))"
    it "parses ∃ ∀ with implication"            $ shouldParse "∃ x. ∀ y. (P(y) → Q(x))"
    it "parses ∀ ∃ Less"                        $ shouldParse "∀ x. ∃ y. Less(x, y)"
    it "parses addition closure"                          $ shouldParse "∀ x. ∀ y. ∃ z. Add(x, y, z)"
    it "parses alternating four quantifiers"              $ shouldParse "∀ x. ∃ y. ∀ z. (R(x, y) ∧ (R(y, z) → R(x, z)))"
 
  describe "quantifiers interacting with connectives" $ do
    it "parses ∀ implies ∃"                     $ shouldParse "∀ x. P(x) → ∃ x. P(x)"
    it "parses law of excluded middle"                    $ shouldParse "∀ x. (P(x) ∨ ¬P(x))"
    it "parses de Morgan: not-∃ vs ∀-not"       $ shouldParse "¬∃ x. P(x) ↔ ∀ x. ¬P(x)"
    it "parses de Morgan: not-∀ vs ∃-not"       $ shouldParse "¬∀ x. P(x) ↔ ∃ x. ¬P(x)"
    it "parses conjunction of ∀s"                    $ shouldParse "∀ x. P(x) ∧ ∀ x. Q(x)"
    it "parses disjunction of ∃"                     $ shouldParse "∃ x. P(x) ∨ ∃ x. Q(x)"
    it "parses chain of ∀ implications"              $ shouldParse "∀ x. (P(x) → Q(x)) ∧ ∀ x. (Q(x) → R(x))"
    it "parses modus ponens schema"                       $ shouldParse "(∃ x. P(x)) ∧ (∀ x. (P(x) → Q(x))) → ∃ x. Q(x)"
 
  describe "nested function terms" $ do
    it "parses doubly nested function"                    $ shouldParse "P(f(f(x)))"
    it "parses composed functions"                        $ shouldParse "P(f(g(x)))"
    it "parses equality of composed functions"            $ shouldParse "Equal(f(g(x)), g(f(x)))"
    it "parses predicate with three function args"        $ shouldParse "R(f(x), g(y), h(z))"
    it "parses triply nested function"                    $ shouldParse "P(f(f(f(x))))"
    it "parses Peano addition step"                       $ shouldParse "Equal(add(succ(x), y), succ(add(x, y)))"
    it "parses ∀ inverse law"                        $ shouldParse "∀ x. Equal(f(g(x)), x)"
    it "parses Peano recursive addition axiom"            $ shouldParse "∀ x. ∀ y. Equal(add(x, succ(y)), succ(add(x, y)))"
    it "parses ∃ fixed point"                        $ shouldParse "∃ x. Equal(f(x), g(x))"
 
  describe "terms in isolation" $ do
    it "parses lowercase variable"                        $ shouldParseTerm "x"
    it "parses nullary constant"                          $ shouldParseTerm "zero"
    it "parses unary function"                            $ shouldParseTerm "f(x)"
    it "parses binary function"                           $ shouldParseTerm "add(x, y)"
    it "parses nested function term"                      $ shouldParseTerm "f(g(x))"
    it "parses succ of zero"                              $ shouldParseTerm "succ(zero)"
    it "parses deeply nested term"                        $ shouldParseTerm "f(g(h(x)))"
 
  describe "equality axioms" $ do
    it "parses reflexivity"                               $ shouldParse "∀ x. Equal(x, x)"
    it "parses symmetry"                                  $ shouldParse "∀ x. ∀ y. (Equal(x, y) → Equal(y, x))"
    it "parses transitivity"                              $ shouldParse "∀ x. ∀ y. ∀ z. (Equal(x, y) ∧ Equal(y, z) → Equal(x, z))"
    it "parses substitution"                              $ shouldParse "∀ x. ∀ y. (Equal(x, y) → (P(x) → P(y)))"
    it "parses existence"                                 $ shouldParse "∃ x. Equal(x, x)"
    it "parses zero is not a successor"                   $ shouldParse "∀ x. ¬Equal(succ(x), zero)"
    it "parses injectivity of succ"                       $ shouldParse "∀ x. ∀ y. (Equal(succ(x), succ(y)) → Equal(x, y))"
    it "parses Peano addition base case"                  $ shouldParse "∀ x. Equal(add(x, zero), x)"
    it "parses Peano addition recursive case"             $ shouldParse "∀ x. ∀ y. Equal(add(x, succ(y)), succ(add(x, y)))"
 
  describe "edge cases and parenthesisation" $ do
    it "parses vacuously true antecedent"                 $ shouldParse "∀ x. (P(x) ∧ ¬P(x) → Q(x))"
    it "parses four-quantifier alternation"               $ shouldParse "∀ x. ∃ y. ∀ z. ∃ w. R(x, y, z, w)"
    it "parses quantifier with scoped conjunction"        $ shouldParse "∀ x. (P(x) ∧ Q(y))"
    it "parses tautology schema"                          $ shouldParse "∀ x. (P(x) → P(x))"
    it "parses excluded middle"                           $ shouldParse "∀ x. (P(x) ∨ ¬P(x))"
    it "parses deeply nested unary function"              $ shouldParse "P(f(g(h(x))))"
    it "parses formula with mixed bound and free vars"    $ shouldParse "∀ x. ∃ y. R(x, y, z)"
    it "parses left-nested conjunction"                   $ shouldParse "((P(x) ∧ Q(x)) ∧ R(x)) ∧ S(x)"
    it "parses right-nested disjunction"                  $ shouldParse "P(x) ∨ (Q(x) ∨ (R(x) ∨ S(x)))"
    it "parses biconditional of biconditionals"           $ shouldParse "(P(x) ↔ Q(x)) ↔ (R(x) ↔ S(x))"
    it "parses negated quantified biconditional"          $ shouldParse "¬(∀ x. (P(x) ↔ Q(x)))"
    it "parses constant-only predicate"                   $ shouldParse "Likes(alice, bob)"
    it "parses ternary constant predicate"                $ shouldParse "R(alice, bob, carol)"

  describe "boolean literals" $ do
    it "parses true"                                      $ shouldParse "true"
    it "parses false"                                     $ shouldParse "false"
    it "parses negation of true"                          $ shouldParse "¬true"
    it "parses negation of false"                         $ shouldParse "¬false"
    it "parses double negation of true"                   $ shouldParse "¬¬true"
    it "parses true ∧ false"                              $ shouldParse "true ∧ false"
    it "parses true ∨ false"                              $ shouldParse "true ∨ false"
    it "parses true → false"                              $ shouldParse "true → false"
    it "parses true ↔ true"                               $ shouldParse "true ↔ true"
    it "parses true ∧ P(x)"                               $ shouldParse "true ∧ P(x)"
    it "parses P(x) ∨ false"                              $ shouldParse "P(x) ∨ false"
    it "parses false → P(x)"                              $ shouldParse "false → P(x)"
    it "parses true inside quantifier"                    $ shouldParse "∀ x. (P(x) ∨ true)"
    it "parses false inside quantifier"                   $ shouldParse "∃ x. (P(x) ∧ false)"
    it "parses true in implication body"                  $ shouldParse "∀ x. (P(x) → true)"
    it "parses false in antecedent"                       $ shouldParse "∀ x. (false → P(x))"
    it "parses nested boolean expression"                 $ shouldParse "(true ∧ false) ∨ P(x)"
    it "parses true ∧ true ∧ true"                        $ shouldParse "true ∧ true ∧ true"
