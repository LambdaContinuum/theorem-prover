module CNFSpec where

import Test.Hspec
import CNF
import PropParser
import PropLanguage

spec :: Spec
spec = describe "toCNF" $ do

  describe "atoms" $ do
    it "propositional variable" $
      toCNF (Var "P") `shouldBe`
        [[Var "P"]]
    it "negated variable" $
      toCNF (Not (Var "P")) `shouldBe`
        [[Not (Var "P")]]
    it "predicate" $
      toCNF (Pred "P" [TVar "x"]) `shouldBe`
        [[Pred "P" [TVar "x"]]]
    it "negated predicate" $
      toCNF (Not (Pred "P" [TVar "x"])) `shouldBe`
        [[Not (Pred "P" [TVar "x"])]]

  describe "connectives" $ do
    it "conjunction becomes two unit clauses" $
      toCNF (And (Var "P") (Var "Q")) `shouldBe`
        [[Var "P"], [Var "Q"]]
    it "disjunction becomes one clause" $
      toCNF (Or (Var "P") (Var "Q")) `shouldBe`
        [[Var "P", Var "Q"]]
    it "implication" $
      toCNF (Implies (Var "P") (Var "Q")) `shouldBe`
        [[Not (Var "P"), Var "Q"]]
    it "biconditional produces two clauses" $
      toCNF (Iff (Var "P") (Var "Q")) `shouldBe`
        [[Not (Var "P"), Var "Q"], [Not (Var "Q"), Var "P"]]

  describe "negation normal form" $ do
    it "double negation eliminates" $
      toCNF (Not (Not (Var "P"))) `shouldBe`
        [[Var "P"]]
    it "de morgan over and" $
      toCNF (Not (And (Var "P") (Var "Q"))) `shouldBe`
        [[Not (Var "P"), Not (Var "Q")]]
    it "de morgan over or" $
      toCNF (Not (Or (Var "P") (Var "Q"))) `shouldBe`
        [[Not (Var "P")], [Not (Var "Q")]]
    it "negated implication" $
      toCNF (Not (Implies (Var "P") (Var "Q"))) `shouldBe`
        [[Var "P"], [Not (Var "Q")]]
    it "negated biconditional" $
      toCNF (Not (Iff (Var "P") (Var "Q"))) `shouldBe`
        [[Var "P", Var "Q"], [Not (Var "Q"), Not (Var "P")]]

  describe "distribution" $ do
    it "distributes or over and on the right" $
      toCNF (Or (Var "P") (And (Var "Q") (Var "R"))) `shouldBe`
        [[Var "P", Var "Q"], [Var "P", Var "R"]]
    it "distributes or over and on the left" $
      toCNF (Or (And (Var "P") (Var "Q")) (Var "R")) `shouldBe`
        [[Var "P", Var "R"], [Var "Q", Var "R"]]
    it "distributes over nested and" $
      toCNF (Or (Or (Var "P") (Var "Q")) (And (Var "R") (Var "S"))) `shouldBe`
        [[Var "P", Var "Q", Var "R"], [Var "P", Var "Q", Var "S"]]

  describe "flattening" $ do
    it "nested conjunction flattens" $
      toCNF (And (And (Var "P") (Var "Q")) (Var "R")) `shouldBe`
        [[Var "P"], [Var "Q"], [Var "R"]]
    it "nested disjunction flattens into one clause" $
      toCNF (Or (Or (Var "P") (Var "Q")) (Var "R")) `shouldBe`
        [[Var "P", Var "Q", Var "R"]]
    it "implication chain" $
      toCNF (Implies (Var "P") (Implies (Var "Q") (Var "R"))) `shouldBe`
        [[Not (Var "P"), Not (Var "Q"), Var "R"]]
    it "already in CNF is unchanged" $
      toCNF (And (Or (Var "P") (Var "Q")) (Or (Not (Var "P")) (Var "R"))) `shouldBe`
        [[Var "P", Var "Q"], [Not (Var "P"), Var "R"]]

  describe "quantifiers and skolemization" $ do
    it "universal quantifier is dropped" $
      toCNF (Forall "x" (Pred "P" [TVar "x"])) `shouldBe`
        [[Pred "P" [TVar "x"]]]
    it "existential becomes a skolem constant" $
      toCNF (Exists "x" (Pred "P" [TVar "x"])) `shouldBe`
        [[Pred "P" [TFunc "sk0" []]]]
    it "existential under universal becomes a skolem function" $
      toCNF (Forall "x" (Exists "y" (Pred "Loves" [TVar "x", TVar "y"]))) `shouldBe`
        [[Pred "Loves" [TVar "x", TFunc "sk0" [TVar "x"]]]]
    it "multiple universals produce a skolem function of all of them" $
      toCNF (Forall "x" (Forall "y" (Exists "z" (Pred "P" [TVar "x", TVar "y", TVar "z"])))) `shouldBe`
        [[Pred "P" [TVar "x", TVar "y", TFunc "sk0" [TVar "x", TVar "y"]]]]
    it "negated universal becomes a skolem constant" $
      toCNF (Not (Forall "x" (Pred "P" [TVar "x"]))) `shouldBe`
        [[Not (Pred "P" [TFunc "sk0" []])]]
    it "negated existential becomes a universally quantified negation" $
      toCNF (Not (Exists "x" (Pred "P" [TVar "x"]))) `shouldBe`
        [[Not (Pred "P" [TVar "x"])]]
    it "two separate existentials get distinct skolem constants" $
      toCNF (And (Exists "x" (Pred "P" [TVar "x"]))
                 (Exists "y" (Pred "Q" [TVar "y"]))) `shouldBe`
        [[Pred "P" [TFunc "sk0" []]], [Pred "Q" [TFunc "sk1" []]]]
