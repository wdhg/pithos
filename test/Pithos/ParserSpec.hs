module Pithos.ParserSpec where

import Pithos
import Test.Hspec

a, b, c :: Formula
a = Atom "a"
b = Atom "b"
c = Atom "c"

spec :: Spec
spec
  = describe "parse" $ do
      it "should parse propositional atoms" $ do
        parse "a" `shouldBe` a
      it "should parse nots" $ do
        parse "~a" `shouldBe` Not a
      it "should parse conjunctions" $ do
        parse "a&b" `shouldBe` And a b
      it "should parse disjunctions" $ do
        parse "a|b" `shouldBe` Or a b
      it "should parse implies" $ do
        parse "a->b" `shouldBe` If a b
      it "should parse if and only ifs" $ do
        parse "a<->b" `shouldBe` Iff a b
      it "should parse truth and falsity" $ do
        parse "T&F" `shouldBe` And Truth Falsity
      it "should parse bracketed subformulas" $ do
        parse "(a<->b)&(b->c)" `shouldBe` And (Iff a b) (If b c)
      it "should parse formulas with whitespace" $ do
        parse " ( a <-> b ) & ( b -> c ) " `shouldBe` And (Iff a b) (If b c)
