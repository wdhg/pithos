module Pithos.Parser where

import Pithos.Base
import Pithos.Lexer

lowerPrecedence :: Token -> Token -> Bool
lowerPrecedence _ TokRParen
  = False
lowerPrecedence (TokOp op0) (TokOp op1)
  = op0 < op1

sortOperator :: Token -> ([Token], [Token]) -> ([Token], [Token])
sortOperator t@(TokOp _) (output, stack)
  = let (ops, stack') = span (lowerPrecedence t) stack
     in ((reverse ops) ++ output, t : stack')

sortRParen :: ([Token], [Token]) -> ([Token], [Token])
sortRParen (output, stack)
  = (output, TokRParen : stack)

sortLParen :: ([Token], [Token]) -> ([Token], [Token])
sortLParen (output, stack)
  = let (ops, _:stack') = break (== TokRParen) stack
     in ((reverse ops) ++ output, stack')

sortToOutput :: Token -> ([Token], [Token]) -> ([Token], [Token])
sortToOutput t (output, stack)
  = (t : output, stack)

sortToken :: Token -> ([Token], [Token]) -> ([Token], [Token])
sortToken t
  = case t of
      (TokOp _) -> sortOperator t
      TokRParen -> sortRParen
      TokLParen -> sortLParen
      _         -> sortToOutput t

-- shunting yard into Polish notation
sortTokens' :: [Token] -> ([Token], [Token])
sortTokens' []
  = ([], [])
sortTokens' (t:ts)
  = sortToken t $ sortTokens' ts

sortTokens :: [Token] -> [Token]
sortTokens ts
  = let (output, stack) = sortTokens' ts
     in (reverse stack) ++ output

parseOp :: Operation -> [Formula] -> [Formula]
parseOp _ []
  = error "cannot parse an operator with no formulas after it"
parseOp Not (f:fs)
  = (Op Not [f]) : fs
parseOp op (f0:f1:fs)
  = (Op op [f0,f1]) : fs

parse' :: [Token] -> [Formula]
parse' []
  = []
parse' ((TokAtom var):ts)
  = (Atom var) : parse' ts
parse' (TokTruth:ts)
  = Truth : parse' ts
parse' (TokFalsity:ts)
  = Falsity : parse' ts
parse' (TokOp op:ts)
  = parseOp op $ parse' ts

parse :: [Token] -> Formula
parse
  = head . parse' . sortTokens
