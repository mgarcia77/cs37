{-

Implementation of the Boolean Expression language.  

In this version, the second version we did in class, TRUE and FALSE are
replaced with "Boolean True" and "Boolean False".

-}

data Boolexp = 
    Boolean Bool
  | Not Boolexp
  | And Boolexp Boolexp
  | Or Boolexp Boolexp
  | Implies Boolexp Boolexp
   deriving (Show)

exp1 = Implies (And (Boolean True) (Boolean False)) 
               (And (Not (Boolean True)) (Boolean False))

eval :: Boolexp -> Boolexp
eval exp@(Boolean _) = exp
eval (And exp1 exp2) = case (eval exp1, eval exp2) of
                         (Boolean b1, Boolean b2) -> Boolean (b1 && b2)
eval (Or exp1 exp2) = case (eval exp1, eval exp2) of
                         (Boolean b1, Boolean b2) -> Boolean (b1 || b2)
eval (Implies exp1 exp2) = case (eval exp1, eval exp2) of
                         (Boolean b1, Boolean b2) -> Boolean (not b1 || b2)
eval (Not exp) = case (eval exp) of
     	       	   (Boolean b) -> Boolean (not b)
