module Pithos.Parser (parse) where

import Pithos.Base
import Pithos.Lexer

parse' :: [Token] -> [Token] -> Formula
parse'
  = undefined

parse :: [Token] -> Formula
parse
  = parse' []
