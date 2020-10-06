module Pithos.Base where

newtype Var
  = Var String

newtype Const
  = Const String

newtype Arity
  = Arity Int

data RelationSymbol
  = RelationSymbol String Arity

data Term
  = TVar Var
  | TConst Const

data Atom
  = Relation RelationSymbol [Term]
  | Equal Term Term
  | Truth
  | Falsity

data Formula
  = Atomic Atom
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | If Formula Formula
  | Iff Formula Formula
  | ForAll Var Formula
  | Exists Var Formula
