module Pithos.Base where

newtype Var
  = Var String

data Connective
  = Iff | If | Or | And | Not deriving (Show, Eq, Ord)

data Formula
  = Atom Var
  | Truth
  | Falsity
  | Op Connective [Formula]
