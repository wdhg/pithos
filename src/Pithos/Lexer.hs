module Pithos.Lexer
  ( tokenize
  , Token(..)
  ) where

data Token
  = TokNot
  | TokAnd
  | TokOr
  | TokIf
  | TokIff
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
tokenize ('~':cs)
  = TokNot : tokenize cs
tokenize ('&':cs)
  = TokAnd : tokenize cs
tokenize ('|':cs)
  = TokOr : tokenize cs
tokenize ('-':'>':cs)
  = TokIf : tokenize cs
tokenize ('<':'-':'>':cs)
  = TokIff : tokenize cs
tokenize ('(':cs)
  = TokLParen : tokenize cs
tokenize (')':cs)
  = TokRParen : tokenize cs
tokenize cs
  = let (var, cs') = span isIdentifier cs
     in TokVar var : tokenize cs'
