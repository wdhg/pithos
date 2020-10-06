module Pithos.Lexer
  ( tokenize
  , Token(..)
  ) where

import Pithos.Base

data Token
  = TokTruth
  | TokFalsity
  | TokOp Operation
  | TokLParen -- (
  | TokRParen -- )
  | TokVar String
    deriving (Eq, Show)

isIdentifier :: Char -> Bool
isIdentifier c
  = let valid = '_' : ['a'..'z'] ++ ['A'..'Z']
     in c `elem` valid

tokenize :: String -> [Token]
tokenize ""
  = []
tokenize (' ':cs)
  = tokenize cs
tokenize ('T':cs)
  = TokTruth : tokenize cs
tokenize ('F':cs)
  = TokFalsity : tokenize cs
tokenize ('~':cs)
  = TokOp Not : tokenize cs
tokenize ('&':cs)
  = TokOp And : tokenize cs
tokenize ('|':cs)
  = TokOp Or : tokenize cs
tokenize ('-':'>':cs)
  = TokOp If : tokenize cs
tokenize ('<':'-':'>':cs)
  = TokOp Iff : tokenize cs
tokenize ('(':cs)
  = TokLParen : tokenize cs
tokenize (')':cs)
  = TokRParen : tokenize cs
tokenize cs
  = let (var, cs') = span isIdentifier cs
     in TokVar var : tokenize cs'
