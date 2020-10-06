module Pithos.LexerSpec where

import Pithos.Lexer
import Test.Hspec

spec :: Spec
spec
  = describe "tokenize" $ do
      it "should return [] on empty" $ do
        tokenize "" `shouldBe` []
      it "should return [] on whitespace" $ do
        tokenize "   " `shouldBe` []
      it "should tokenize nots" $ do
        tokenize "!a" `shouldBe` [TokNot, TokVar "a"]
      it "should tokenize conjuntions" $ do
        tokenize "a & b" `shouldBe` [TokVar "a", TokAnd, TokVar "b"]
      it "should tokenize disjunctions" $ do
        tokenize "a | b" `shouldBe` [TokVar "a", TokOr, TokVar "b"]
      it "should tokenize implications" $ do
        tokenize "a -> b" `shouldBe` [TokVar "a", TokIf, TokVar "b"]
      it "should tokenize if and only if" $ do
        tokenize "a <-> b" `shouldBe` [TokVar "a", TokIff, TokVar "b"]
      it "should tokenize more complex identifiers" $ do
        tokenize "cat_eats_food <-> cat_is_alive" `shouldBe`
          [TokVar "cat_eats_food", TokIff, TokVar "cat_is_alive"]
