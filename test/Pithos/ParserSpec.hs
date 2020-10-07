module Pithos.ParserSpec where

import Pithos
import Test.Hspec

spec :: Spec
spec
  = describe "parse" $ do
      it "should parse propositional atoms" $ do
        parse [TokAtom "a"] `shouldBe` (Atom "a")
      it "should parse nots" $ do
        parse [TokOp Not, TokAtom "a"] `shouldBe` (Op Not [Atom "a"])
      it "should parse conjunctions" $ do
        parse [TokOp And, TokAtom "a", TokAtom "b"] `shouldBe`
          (Op And [Atom "a", Atom "b"])
      it "should parse disjunctions" $ do
        parse [TokOp Or, TokAtom "a", TokAtom "b"] `shouldBe`
          (Op Or [Atom "a", Atom "b"])
      it "should parse implies" $ do
        parse [TokOp If, TokAtom "a", TokAtom "b"] `shouldBe`
          (Op If [Atom "a", Atom "b"])
      it "should parse if and only ifs" $ do
        parse [TokOp Iff, TokAtom "a", TokAtom "b"] `shouldBe`
          (Op Iff [Atom "a", Atom "b"])
      it "should parse truth and falsity" $ do
        parse [TokOp And, TokTruth, TokFalsity] `shouldBe`
          (Op And [Truth, Falsity])
      it "should parse complex outputs from tokenize" $ do
        (parse $ tokenize "a & (b -> F) | ~c") `shouldBe`
          (Op Or
            [ Op And [Atom "a", Op If [Atom "b", Falsity]]
            , Op Not [Atom "c"]
            ]
          )
