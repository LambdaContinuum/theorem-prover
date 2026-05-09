module PropLanguage where

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void String

data Prop = Var String 
          | Con Bool 
          | Not Prop 
          | And Prop Prop 
          | Or Prop Prop 
          | Implies Prop Prop 
          | Iff Prop Prop 
          | Forall String Prop
          | Exists String Prop
          | Pred String [Term]
          deriving (Show, Eq)

data Term = TVar String | TFunc String [Term] deriving (Show, Eq)


