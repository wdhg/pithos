module Pithos.Base where

data Formula
  = Truth
  | Falsity
  | Atom String
  | Iff Formula Formula
  | If Formula Formula
  | Or Formula Formula
  | And Formula Formula
  | Not Formula
    deriving (Show, Eq)
