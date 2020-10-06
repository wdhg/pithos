module Pithos.Base where

newtype Var
  = Var String

data Formula
  = Atom Var
  | Truth
  | Falsity
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | If Formula Formula
  | Iff Formula Formula
