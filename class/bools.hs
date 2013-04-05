
data Boolexp = 
--    TRUE 
--  | FALSE 
    Boolean Bool  -- using other built-in types
  | Not Boolexp
  | And Boolexp Boolexp
  | Or Boolexp Boolexp
  | Implies Boolexp Boolexp
  deriving (Show)


eval :: Boolexp -> Boolexp
{-
eval TRUE = TRUE
eval FALSE = FALSE
eval (And e1 e2) = case (eval e1, eval e2) of  -- case statements evaluate
                     (TRUE, TRUE) -> TRUE
                     (_, _) -> FALSE
eval (Or e1 e2) = case (eval e1, eval e2) of
                    (FALSE, FALSE) -> FALSE
                    (_, _) -> TRUE
eval (Implies e1 e2) = case (eval e1, eval e2) of
                         (TRUE, FALSE) -> FALSE
                         (_, _) -> TRUE
eval (Not e) = case (eval e) of
                 TRUE -> FALSE
                 FALSE -> TRUE
                 -}
eval e@(Boolean b) = e
eval (And e1 e2) = case (eval e1, eval e2) of  -- case statements evaluate
                    (Boolean v1, Boolean v2) -> (Boolean (v1 && v2))
                    -- use built-in &&
eval (Or e1 e2) = case (eval e1, eval e2) of
                    (Boolean v1, Boolean v2) -> (Boolean (v1 || v2))
eval (Implies e1 e2) = case (eval e1, eval e2) of
                       (Boolean v1, Boolean v2) -> Boolean ((not v1) || v2)
eval (Not e) = case (eval e) of
                 (Boolean v) -> Boolean (not v)

