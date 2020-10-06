module Pithos.Base where

newtype Var
  = Var String

data Operation
  = Iff | If | Or | And | Not deriving (Show, Eq, Ord)

data Formula
  = Atom Var
  | Truth
  | Falsity
  | Op Operation [Formula]
