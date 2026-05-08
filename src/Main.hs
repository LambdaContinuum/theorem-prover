

-- Data Types representing Propositions
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

-- Data Type representing Terms (Variables or Functions)
data Term = TVar String | TFunc String [Term] deriving (Show, Eq)

x :: Prop
x = Iff (Var "a") (Var "b")

main :: IO ()
main = print x 
