module Pithos.LexerSpec where

import Pithos
import Test.Hspec

spec :: Spec
spec
  = describe "tokenize" $ do
      it "should return [] on empty" $ do
        tokenize "" `shouldBe` []
      it "should return [] on whitespace" $ do
        tokenize "   " `shouldBe` []
      it "should tokenize truths" $ do
        tokenize "T" `shouldBe` [TokTruth]
      it "should tokenize Falsities" $ do
        tokenize "F" `shouldBe` [TokFalsity]
      it "should tokenize nots" $ do
        tokenize "~a" `shouldBe` [TokOp Not, TokVar "a"]
      it "should tokenize conjuntions" $ do
        tokenize "a & b" `shouldBe` [TokVar "a", TokOp And, TokVar "b"]
      it "should tokenize disjunctions" $ do
        tokenize "a | b" `shouldBe` [TokVar "a", TokOp Or, TokVar "b"]
      it "should tokenize implications" $ do
        tokenize "a -> b" `shouldBe` [TokVar "a", TokOp If, TokVar "b"]
      it "should tokenize if and only if" $ do
        tokenize "a <-> b" `shouldBe` [TokVar "a", TokOp Iff, TokVar "b"]
      it "should tokenize more complex identifiers" $ do
        tokenize "cat_eats_food <-> cat_is_alive" `shouldBe`
          [TokVar "cat_eats_food", TokOp Iff, TokVar "cat_is_alive"]
      it "should tokenize more complex formulae" $ do
        tokenize "(a & T) -> (b <-> ~F)" `shouldBe`
          [TokLParen, TokVar "a", TokOp And, TokTruth, TokRParen, TokOp If
          , TokLParen, TokVar "b", TokOp Iff, TokOp Not, TokFalsity, TokRParen]
