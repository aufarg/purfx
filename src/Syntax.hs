module Syntax where

type Name = String

data Expression
  = Integer Integer
  | Double Double
  | String String
  | BinaryOperator Name Expression Expression
  | Conditional Expression Expression Expression
  | Variable String
  | Call Name [Expression]
  deriving (Eq, Ord, Show)

data Definition
  = Pure Name [Name] Expression
  | Effect Name [Name] Expression
  | Extern Name [Name]
  deriving (Eq, Ord, Show)

type Module = [Definition]
