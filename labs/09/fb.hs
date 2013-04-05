
data Ident = Ident String
  deriving (Show, Eq)

data Expr = 
  Var Ident             -- Var (Ident "x")
  | Function (Ident,Expr)
        -- Function (Ident "x", Plus (Var (Ident "x"), (Integer 2)))
  | Appl (Expr,Expr)    
        -- Appl (Var (Ident "f"), Integer 10)
  | Letrec (Ident,Ident,Expr,Expr)
  | Plus (Expr,Expr)
  | Minus (Expr,Expr)
  | Equal (Expr,Expr)
  | And (Expr,Expr)
  | Or (Expr,Expr)
  | Not Expr
  | If (Expr,Expr,Expr)
  | Integer Int
  | Boolean Bool
    deriving (Show, Eq)


{- 

  Exercise 1.

  Pages 12 and 13 give many example converting concrete syntax into
  abstract syntax for the boolean language we worked on last
  week. Convert the following conrete syntax expressions into
  abstract syntax. Your answers should be interpretable by Haskell as
  being of the type expr which we just defined above.


  1a.  Subtract 1 from 10

  (Function x -> x - 1) 10

  
  1b.  A possible representation for the tuple (x,y) with selector z
  
  Function x -> Function y -> Function z -> If z = 0 Then x Else y

-}

-- 1a
-- Function (Ident "x", Plus (Var (Ident "x"), (Integer 2)))

q1a = Appl ((Function (Ident "x", Minus (Var (Ident "x"), (Integer 1)))), (Integer 10))

-- 1b

q1b = (Function (Ident "x", 
                (Function (Ident "y", 
                          (Function (Ident "z", 
                                    If 
                                    ((Equal (Var (Ident "z"), Integer 0)),
                                    (Var (Ident "x")),
                                    (Var (Ident "y")))))))))

{- 

In the above exercises, you had to type Ident("x") and Var(Ident("x"))
more than once.  You will continue to have to type similar expressions
over and over throughout the assignment, and this will get tiring
quickly.  Instead, we will define two shortcut functions, i and v,
which should be self explanatory.

-}

i :: String -> Ident
i s = Ident s

v :: String -> Expr
v s = Var (Ident s) 

{-

One nice thing about the Haskell parser is that when it is clear from
context, you are allowed to omit spaces between the function name and
the parameter.  So, to write (Ident "x"), you can just write i"x", and
to write (Var (Ident "x")), you can just write v"x".

-}


{-

Exercise 2.

Definition 2.8 (page 18) defines Free Occurrence. This is a very
important concept and one that you will need to use in order to get
your Fb evaluator to work properly.

You will write a function called occursFree which takes two
parameters, an expression (Expr) and an indentifier (Ident), and
returns True if the identifier occurs free in the expression, or False
otherwise:

  occursFree :: Expr -> Ident -> Bool

For example, the following, taken from example 2.6 in the text (p.18),
tests whether or not x occurs free in Function x -> x + 1:

  occursFree (Function(i"x",Plus(v"x",Integer 1))) (i"x") -- False

Taken from example 2.11, this tests whether or not z occurs free in
Function x -> Function y -> x + y + z:

  occursFree (Function(i"x",Function(i"y",Plus(v"x",Plus(v"y",v"z"))))) (i"z")

The above should return True.

NOTE: You should be able to write this function making judicious use
of pattern matching, avoiding if statements. In fact, pattern matching
will make this function much easier to write than if you try to use if
statements. If you don't know how to use pattern matching here, ask
for help.

data Expr = 
  Var Ident             -- Var (Ident "x")
  | Function (Ident,Expr)
        -- Function (Ident "x", Plus (Var (Ident "x"), (Integer 2)))
  | Appl (Expr,Expr)    
        -- Appl (Var (Ident "f"), Integer 10)
  | Letrec (Ident,Ident,Expr,Expr)
  | Plus (Expr,Expr)
  | Minus (Expr,Expr)
  | Equal (Expr,Expr)
  | And (Expr,Expr)
  | Or (Expr,Expr)
  | Not Expr
  | If (Expr,Expr,Expr)
  | Integer Int
  | Boolean Bool
    deriving (Show, Eq)

-}

-- The type declaration for occursFree is correct.
-- You must write the proper implementation.
occursFree :: Expr -> Ident -> Bool
occursFree (Var (Ident x)) (Ident i) = (x /= i) 
        -- got to base var, did we find i?
occursFree (Function ((Ident x), e)) (Ident i) = 
        (x /= i) && occursFree e (Ident i)
occursFree (Appl (e1,e2)) (Ident i) = 
        occursFree e1 (Ident i) && occursFree e2 (Ident i)
occursFree (Letrec ((Ident x), (Ident y), e1, e2)) (Ident i) =
        (x /= i) && (y /= i) &&
        occursFree e1 (Ident i) && occursFree e2 (Ident i)
occursFree (Plus (e1, e2)) i =
        occursFree e1 i && occursFree e2 i
occursFree (Minus (e1, e2)) i =
        occursFree e1 i && occursFree e2 i
occursFree (Equal (e1, e2)) i =
        occursFree e1 i && occursFree e2 i
occursFree (And (e1, e2)) i =
        occursFree e1 i && occursFree e2 i
occursFree (Or (e1, e2)) i =
        occursFree e1 i && occursFree e2 i
occursFree (Not e) i = 
        occursFree e i
occursFree (If (e1, e2, e3)) i =
        occursFree e1 i && occursFree e2 i && occursFree e3 i
occursFree _ ide = False  -- anything else

-- NEEDS TESTING, NOT WORKING YET


{-

Test cases: You should definitely write more cases. If this function
is incorrect you can have LOTS of problems later.

-}

twoA = -- should be False
    occursFree
      (Function(i"x",Plus(v"x",Integer 1))) 
      (i"x") 

twoB = -- should be True
  occursFree 
    (Function(i"x",Function(i"y",Plus(v"x",Plus(v"y",v"z"))))) (i"z")



{-

Exercise 3.

Definition 2.9 (page 19) defines Closed Expression. This is another
important concept and one that you will need to use in order to
get your Fb evaluator to work properly.

You will write a function called isClosed which takes one parameter,
an expression, and returns True if the expression is closed; False
otherwise.

  isClosed :: Expr -> Bool

Definition 2.9 says: An expression e is closed if it contains no free
variable occurrences.

To implement isClosed, we will first make a list of all the variables
in the expression e. For each variable in this list, we will ask
whether or not it occursFree in e. If none of these variables
occursFree in e, isClosed will return True; otherwise, it returns
False.

3a.

Definition 2.6 (p.18) says "A variable x occurs in e if x appears
somewhere in e. Note we refer only to variable uses, not definitions."
In Fb, "variable uses" are Var expressions, whereas "variable
definitions" Ident types.

To help in writing isClosed, we will first write listOfVars which will
take an expression as a parameter and return a list of identifiers
which occur in the expression.

Do not worry about having the same identifier appearing multiple times
in this list.

  listOfVars :: Expr -> [Ident]

For example:

  listOfVars(Function(i"x",Plus(v"y",Integer 5)))
  -- returns: [Ident "y"]

  listOfVars(Function(i"x",Plus(v"x",v"x")))
  -- returns: [Ident "x"; Ident "x"]

-}

-- 3a.
-- The type declaration for listOfVars is correct.
-- You must write the proper implementation.
listOfVars :: Expr -> [Ident]
listOfVars expr = []

{-

3b. 

Now that we have a list of all the identifiers in the expression, we
want to write a function called checkVars which takes an expression
and a list of identifiers as parameters and returns True if none of
the identifiers occursFree in the expression.

  checkVars :: Expr -> [Ident] -> Bool

-}

-- 3b.
-- The type declaration for checkVars is correct.
-- You must write the proper implementation.
checkVars :: Expr -> [Ident] -> Bool
checkVars expr _ = False



{-

3c.

Finally, we can write isClosed.  Recall that an expression e is closed
if none of the identifiers in e occur free in e.  You should be able
to write the definition of isClosed in one line, making use of the
checkVars function you wrote in 3b.  Here are some examples:

  isClosed(Function(i"x",Plus(v"x",Integer 5)))  -- True
  isClosed(Function(i"x",Plus(v"x",v"y")))       -- False

-}

-- 3c.
-- The type declaration for isClosed is correct.
-- You must write the proper implementation.
isClosed :: Expr -> Bool
isClosed expr = False






{-

Exercise 4.

Definition 2.10 (Variable Substitution, p.21) describes a procedure to
replace all free occurrences of a specific variable (x) in one
expression (e) with another expression (e').  You will write a
function which performs this substitution.

This function, called subst, takes three parameters, e, e', and x.
This function returns a new expression with the appropriate
substitutions performed.  "The variable substitution of x for e' in e,
denoted e[e'/x], is the expression resulting from the operation of
replacing all free occurrences of x in e with e'."

The inductive definition of substitution listed on page 21 is complete
enough for you to able to complete the subst function, but you may
find this additional information helpful.  Pay close attention to the
variable names in each row and the conditions at the end of the row.

(Letrec f y = e1 in e2)[v/x] = (Letrec f y = e1 in e2)           if f == x 
(Letrec f y = e1 in e2)[v/x] = (Letrec f y = e1 in e2[v/x])      if y == x
(Letrec f y = e1 in e2)[v/x] = (Letrec f y = e1[v/x] in e2[v/x]) otherwise

One final note before you begin: we don't want to assume that e' is a
closed expression (as stated in Definition 2.10) -- we want to ensure
that e' is a closed expression before we go ahead with the
substitution.  So, the real work of implementing this function will be
implementing the helper function subhelp shown below.  I have filled
in the necessary code to ensure that e' is a closed expression.  All
of your recursive calls should be made to the subhelp function, not
subst.

-}


-- 4.
-- The type declaration for subst is correct.
-- You must write the proper implementation.
subst :: Expr -> Expr -> Ident -> Expr

subst e e' x
  | (isClosed e')  = subhelp e
  | otherwise      = error "Expression not closed"
  where 
    subhelp e = e -- You need to implement this


-- Here are some tests you can run in GHCI to test your subst function:
{- 
subst (v"x") (Integer 5) (i"x")   -- Integer 5 
subst (v"y") (Integer 5) (i"x")   -- Var (Ident "y")
subst (Integer 2) (Integer 5) (i"x")  -- Integer 2 

subst (Function(i"x", Plus(v"x", Integer 1))) (Integer 5) (i"x")
-- The function remains unchanged

subst (Function(i"y", Plus(v"x", Integer 1))) (Integer 5) (i"x")
-- v"x" becomes Integer 5 

let a = Letrec(i"f", i"x", v"x", Appl(v"f",v"y"))
subst a (Integer 5) (i"x")
-- Unchanged

let a = Letrec(i"f", i"x", v"x", Appl(v"f",v"y"))
subst a (Integer 5) (i"y")
-- v"y" becomes Integer 5 
-}

-- End of subst tests


