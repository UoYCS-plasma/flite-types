module Flite.RedSyntax where

type Arity = Int

type Index = Int

data Atom =
    INT Int
  | ARG Int
  | PTR Int
  | CON Arity Index
  | FUN Arity Index
  | PRI String
  | TAB Index Int Int
  deriving (Show, Read)

type App = [Atom]

type Template = (Arity, App, [App])

type Prog = [Template]
