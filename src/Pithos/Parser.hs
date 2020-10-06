module Pithos.Parser (parse) where

import Pithos.Base
import Pithos.Lexer

lowerPrecedence :: Token -> Token -> Bool
lowerPrecedence (TokOp op0) (TokOp op1)
  = op0 < op1

-- PRE: Token is a TokOp
sortOperator :: Token -> ([Token], [Token]) -> ([Token], [Token])
sortOperator t (output, [])
  = (output, [t])
sortOperator t@(TokOp _) (output, stack)
  = let (ops, stack') = span (lowerPrecedence t) stack
     in (ops ++ output, t : stack)

sortRParen :: ([Token], [Token]) -> ([Token], [Token])
sortRParen (output, stack)
  = (output, TokRParen : stack)

sortLParen :: ([Token], [Token]) -> ([Token], [Token])
sortLParen (output, stack)
  = let (ops, stack') = break (== TokRParen) stack
     in (ops ++ output, tail stack')

sortToOutput :: Token -> ([Token], [Token]) -> ([Token], [Token])
sortToOutput t (output, stack)
  = (t : output, stack)

-- shunting yard into Polish notation
sortTokens :: [Token] -> ([Token], [Token])
sortTokens []
  = ([], [])
sortTokens (op@(TokOp _):ts)
  = sortOperator op $ sortTokens ts
sortTokens (TokRParen:ts)
  = sortRParen $ sortTokens ts
sortTokens (TokLParen:ts)
  = sortLParen $ sortTokens ts
sortTokens (t:ts)
  = sortToOutput t $ sortTokens ts

parse :: [Token] -> Formula
parse
  = undefined
