module Pithos.Base where

data Operation
  = Iff | If | Or | And | Not deriving (Show, Eq, Ord)

data Formula
  = Atom String
  | Truth
  | Falsity
  | Op Operation [Formula]
    deriving (Show, Eq)
