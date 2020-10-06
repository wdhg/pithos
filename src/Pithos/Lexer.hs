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
  | TokForAll
  | TokExists
  | TokLParen -- (
  | TokRParen -- )
  | TokLBracket -- [
  | TokRBracket -- ]
  | TokComma
  | TokId String
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
tokenize ('!':cs)
  = TokNot : tokenize cs
tokenize ('&':cs)
  = TokAnd : tokenize cs
tokenize ('|':cs)
  = TokOr : tokenize cs
tokenize ('-':'>':cs)
  = TokIf : tokenize cs
tokenize ('<':'-':'>':cs)
  = TokIff : tokenize cs
tokenize ('@':cs)
  = TokForAll : tokenize cs
tokenize ('#':cs)
  = TokExists : tokenize cs
tokenize ('(':cs)
  = TokLParen : tokenize cs
tokenize (')':cs)
  = TokRParen : tokenize cs
tokenize ('[':cs)
  = TokLBracket : tokenize cs
tokenize (']':cs)
  = TokRBracket : tokenize cs
tokenize (',':cs)
  = TokComma : tokenize cs
tokenize cs
  = let (word, cs') = span isIdentifier cs
     in TokId word : tokenize cs'
