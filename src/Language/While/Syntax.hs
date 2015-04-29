module Language.While.Syntax where

type Name = String

data AExp
  = Lit Int
  | Var Name
  | Minus AExp AExp
  | Multiply AExp AExp
  deriving (Show, Eq)

data BExp
  = Tr
  | LEQ AExp AExp
  | Not BExp
  | And BExp BExp
  deriving (Show, Eq)

data Com
  = Skip
  | Assignment Name AExp
  | Sequence Com Com
  | If BExp Com Com
  | While BExp Com
  deriving (Show, Eq)
