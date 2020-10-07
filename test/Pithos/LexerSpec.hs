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
        tokenize "~a" `shouldBe` [TokOp Not, TokAtom "a"]
      it "should tokenize conjuntions" $ do
        tokenize "a & b" `shouldBe` [TokAtom "a", TokOp And, TokAtom "b"]
      it "should tokenize disjunctions" $ do
        tokenize "a | b" `shouldBe` [TokAtom "a", TokOp Or, TokAtom "b"]
      it "should tokenize implications" $ do
        tokenize "a -> b" `shouldBe` [TokAtom "a", TokOp If, TokAtom "b"]
      it "should tokenize if and only if" $ do
        tokenize "a <-> b" `shouldBe` [TokAtom "a", TokOp Iff, TokAtom "b"]
      it "should tokenize more complex identifiers" $ do
        tokenize "cat_eats_food <-> cat_is_alive" `shouldBe`
          [TokAtom "cat_eats_food", TokOp Iff, TokAtom "cat_is_alive"]
      it "should tokenize more complex formulae" $ do
        tokenize "(a & T) -> (b <-> ~F)" `shouldBe`
          [TokLParen, TokAtom "a", TokOp And, TokTruth, TokRParen, TokOp If
          , TokLParen, TokAtom "b", TokOp Iff, TokOp Not, TokFalsity, TokRParen]
        tokenize "a & (b -> F) | ~c" `shouldBe`
          [TokAtom "a", TokOp And, TokLParen, TokAtom "b", TokOp If, TokFalsity
          , TokRParen, TokOp Or, TokOp Not, TokAtom "c"]
