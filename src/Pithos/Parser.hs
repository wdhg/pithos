{-# LANGUAGE OverloadedStrings #-}

module Pithos.Parser (parse) where

import Pithos.Base
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, parseOnly, skipSpace, string,
  takeWhile1, try)
import Data.Char (isLower)
import Data.Functor (($>))
import Data.Text (Text, pack, unpack)

truth :: Parser Formula
truth = char 'T' $> Truth

falsity :: Parser Formula
falsity = char 'F' $> Falsity

atom :: Parser Formula
atom = Atom . unpack <$> takeWhile1 isLower

notAtom :: Parser Formula
notAtom
  = do
    char '~'
    Not <$> atom

singular :: Parser Formula
singular = truth <|> falsity <|> notAtom <|> atom

opIff, opIf, opOr, opAnd :: Parser (Formula -> Formula -> Formula)
opIff = string "<->" $> Iff
opIf = string "->" $> If
opOr = string "|" $> Or
opAnd = string "&" $> And

-- left associative
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op
  = do
    skipSpace
    lhs <- p
    skipSpace
    f <- chain' p op <|> pure id
    return $ f lhs

chain' :: Parser a -> Parser (a -> a -> a) -> Parser (a -> a)
chain' p op
  = do
    f <- op
    rhs <- chain p op <|> p
    return (`f` rhs)

formula :: Parser Formula
formula = foldl chain (singular <|> parens formula) [opAnd, opOr, opIf, opIff]

parens :: Parser a -> Parser a
parens parser
  = do
    char '('
    p <- parser
    char ')'
    return p

parse :: String -> Formula
parse input
  = case parseOnly formula $ pack input of
      Left e  -> error e
      Right f -> f
