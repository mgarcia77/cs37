{-

Implementation of the Boolean Expression language.  

In this version, the first version we did in class, TRUE and FALSE are
valid expressions in the language.

-}

data Boolexp = 
    TRUE 
  | FALSE
  | Not Boolexp
  | And Boolexp Boolexp
  | Or Boolexp Boolexp
  | Implies Boolexp Boolexp
   deriving (Show)

exp1 = Implies (And TRUE FALSE) (And (Not TRUE) FALSE)

eval :: Boolexp -> Boolexp

eval TRUE = TRUE
eval FALSE = FALSE
eval (And exp1 exp2) = case (eval exp1, eval exp2) of
                         (TRUE, TRUE) -> TRUE
                         (_, _) -> FALSE
eval (Or exp1 exp2) = case (eval exp1, eval exp2) of
                        (FALSE, FALSE) -> FALSE
                        (_, _) -> TRUE
eval (Implies exp1 exp2) = case (eval exp1, eval exp2) of
                            (TRUE, FALSE) -> FALSE
                            (_, _) -> TRUE
eval (Not exp) = case (eval exp) of
                   TRUE -> FALSE
                   _ -> TRUE

