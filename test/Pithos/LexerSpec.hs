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
      it "should tokenize conjuntions" $ do
        tokenize "a & b" `shouldBe` [TokId "a", TokAnd, TokId "b"]
      it "should tokenize disjunctions" $ do
        tokenize "a | b" `shouldBe` [TokId "a", TokOr, TokId "b"]
      it "should tokenize implications" $ do
        tokenize "a -> b" `shouldBe` [TokId "a", TokIf, TokId "b"]
      it "should tokenize if and only if" $ do
        tokenize "a <-> b" `shouldBe` [TokId "a", TokIff, TokId "b"]
      it "should tokenize for all quantifier" $ do
        tokenize "@cat [has_tail(cat)]" `shouldBe`
          [TokForAll, TokId "cat", TokLBracket, TokId "has_tail", TokLParen
          , TokId "cat", TokRParen, TokRBracket]
      it "should tokenize more complex identifiers" $ do
        tokenize "cat_eats_food <-> cat_is_alive" `shouldBe`
          [TokId "cat_eats_food", TokIff, TokId "cat_is_alive"]
