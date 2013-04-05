import Data.List (insert)
-- This is the Python equivalent of "from Data.List import insert";
-- The 'insert' function is used in Exercise 7.


{-

Exercises 1-4: Credit card validator

If you didn't work on it last week, the first part of the lab
(Exercises 1-4) is to complete the credit card validator. That file is
in labs/08/credit.hs.  (If you started working on it already, just
move your file from the class/w08 directory to the labs/08 directory.)

-}


{-
Some of the following questions are adapted from
http://courses.cms.caltech.edu/cs11/material/haskell/lab2/lab2.hs
-}

-- --------------------------------------

{-

Exercise 5.

Last week, the first exercise was to write a recursive function that
summed a list recursively.  Here is one possible solution.

-}

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = (+) x (sumList xs)

{- 5a. Rewrite the sumList function using foldr. The type declaration
and a stub are given below. -}

sumList' :: Num a => [a] -> a
sumList' lst = foldr (+) 0 lst

-- sumList = foldr (+) 0  --  same thing (by currying)

{- 5b. Write the prodList function using foldr. prodList will multiply
the elements of a list, returning 1 if the list is empty. The type
declaration and a stub are given below. -}

prodList :: Num a => [a] -> a
prodList lst = foldr (*) 1 lst

-- cpy [] = []
-- cpy (x:xs) = x : (cpy xs)
--
-- cpy lst = foldr (:) [] lst
--
-- plus3 lst = foldr fn [] lst
--   where fn elt lst = (elt + 3) : lst
-- fn :: a -> [a] -> [a]
-- --------------------------------------

{- Exercise 6. You will write two versions of a function that will
append two lists. The first version will be a recursive function, the
second version will be a function that makes use of foldr. -}

{- 6a. Write a recursive version of append. A type declaration and a
stub are given below. -}

append :: [a] -> [a] -> [a]
append x [] = x
append [] y = y
append (x:xs) y = x : (append xs y)


{- 6b. Write a version of append that uses foldr. A type declaration and a
stub are given below. -}

append' :: [a] -> [a] -> [a]
append' x y = foldr (:) y x


{- The function 'exists' has the following type:

exists :: (a -> Bool) -> [a] -> Bool

The 'exists' function takes a function and a list of type 'a' and
returns True if there exists at least one element that causes the
function to return True. For example,

exists odd [1,2,3,4,5] -- returns True because there exists at least one
                       -- odd element in the list.
exists null [[1],[2],[3]] -- returns False because there does not exist at
                          -- least one empty list in this list of lists.

  6c. Write exists using recursion.
  6d. Write exists' using foldr.

-}

--6c.
exists :: (a -> Bool) -> [a] -> Bool
exists _ [] = False
exists f (x:xs)
  | f x == True = True
  | otherwise = exists f xs


--6d.
exists' :: (a -> Bool) -> [a] -> Bool
exists' f lst = foldr fn False lst
  where fn elt False = f elt
        fn elt True = True

-- note: the 'any' function in Haskell is the same as the 'exists' function
-- you just wrote!

-- --------------------------------------


{-

Exercise 7. Insertion Sort

In this exercise, you will write a function that sorts a list using
insertion sort. (All the items in the list need to be of typeclass Ord
so that you can use < and > on the items.)  Here is the algorithm for
insertion sort:

1) Sort the sublist consisting of the tail of the list.

2) Insert the first element (the head of the list) into the sorted
sublist at the correct place.

Note that there already exists a function called 'insert' in the
Data.List module, but you can also write an insert function
yourself. Here is the type declaration for insert:

insert :: Ord a => a -> [a] -> [a]

-}

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- --------------------------------------


{- 

Exercise 8.

8a. Write a version of map (which we'll call map2) that works on two
lists.  In other words, if we have a list s=[s1,s2,...] and a list
t=[t1, t2,...], then:

map2 f s t = [(f s1 t1), (f s2 t2), ...]

If one list becomes empty before the other, end the recursion.

-}

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = (f x y) : (map2 f xs ys)


{-

8b. Since Haskell allows for infinite lists (due to lazy evaluation),
we can implement some of the other tricks we did when we worked with
Racket streams.  Create an infinite list of factorials using map2
(starting with 0!). If you are unsure where to start, take a look at
class/w07/streams.rkt for a (big) hint!

-} 

-- in racket: (define factorials (scons 1 (mul-streams factorials positives)

factorial :: [Integer]
factorial = 1 : (map2 (*) factorial [1..])


-- --------------------------------------

{-

Exercise 9. (The actual question is quite a bit further down, but we
begin with some explanation.)

Now we will begin working with algebraic data types. First, an example
using a List data type.  In the example below, 'a' is a type variable.
A List is defined as either being Null, or the result of calling Cons
on a value of type 'a' and List.  We derive Show so that the result
can easily be displayed.  

-}

data List a = Null | Cons a (List a)
  deriving (Show)

-- Now we can define the List [1, 2, 3] as follows:
numList = Cons 1 $ Cons 2 $ Cons 3 Null
--  or, = Cons 1  (Cons 2  (Cons 3 Null))


-- And we can use pattern matching to write functions on List types:
doubleEach :: Num a => List a -> List a
doubleEach Null        = Null
doubleEach (Cons x xs) = Cons (x*2) (doubleEach xs)

totalList :: Num a => List a -> a
totalList Null        = 0
totalList (Cons x xs) = x + (totalList xs)


{-

We can use a similar construction to define a binary tree, made up of
a node (with some value of type 'a'), a left subtree, and a right
subtree. Again, we derive Show so that the Tree can be displayed
easily.

-}

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

{-

Here is a binary tree (note that it is not necessarily a binary
*search* tree).

        1
       / \
      /   \ 
     2     4
    / \   / \
   3   * *   *
  / \
  *  *


We can define that tree using our data type as follows:
-}

exampleTree :: Tree Integer
exampleTree = Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 Leaf Leaf)

-- --------------------------------------

{-

Exercise 9. (Finally!) Write a function that counts the number of Leaf
nodes in a Tree. A type declaration and a stub are given below.

-}
-- not functional yet
countLeaves :: Tree a -> Integer
countLeaves Leaf = 1
countLeaves (Node x y z) = (+) (countLeaves y) (countLeaves z)

-- --------------------------------------

{-

Exercise 10. Write a function that maps a function onto every Node in
the Tree. A type declaration and a stub are given below.

-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree f (Node x y z) = (Node (f x) (mapTree f y) (mapTree f z))

-- --------------------------------------


{-

Exercise 11. 


11a. Write foldTree that takes performs an operation similar
to that of foldl or foldr. A type declaration and a stub are given
below.


sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = (+) x (sumList xs)


foldTree f base tree -> <type of base>
-}

foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree _ b Leaf = b
foldTree f b (Node x y z) = f x (f (foldTree f b y) (foldTree f b z))


{-

If we had a tree with type Tree [Integer], we could use foldTree to
append all of the elements. 

-}

listTree :: Tree [Integer]
listTree = Node [1] (Node [2] (Node [3] Leaf Leaf) (Node [5] Leaf Leaf))
                    (Node [4] Leaf Leaf)

appendedElements = foldTree (++) [] listTree

{-

11b. 

Are your appended elements in:
1. a pre-order traversal, "root-left-right": [1,2,3,5,4]?
2. an in-order traversal, "left-root-right": [3,2,5,1,4]?
3. a post-order traversal, "left-right-root": [3,5,2,4,1]?

My elements are in pre-order traversal.

11c. *Challenge* Can you write all three versions of foldTree
(including the one you already wrote) that perform each of the three
traversals shown in 11b?

-}

foldTreeIn :: (a -> a -> a) -> a -> Tree a -> a
foldTreeIn _ b Leaf = b
foldTreeIn f b (Node x y z) = f (foldTreeIn f b y) (f x (foldTreeIn f b z))

foldTreePost :: (a -> a -> a) -> a -> Tree a -> a
foldTreePost _ b Leaf = b
foldTreePost f b (Node x y z) = f (f (foldTreePost f b y) 
                                        (foldTreePost f b z)) x

-- --------------------------------------

{-

Exercice 12.

Let's define an simple Expr type to represent integers,
addition of two expressions, and multiplication of two expressions.

-}

data Expr = Intgr Integer
          | Add Expr Expr
          | Mul Expr Expr
  deriving (Show)

{-

Some valid expressions are:

Intgr 3  -- the Integer 3
Add (Intgr 2) (Mul (Intgr 3) (Intgr 4))  -- 2 + (3*4) -> 14

Write a function called 'eval' that takes an Expr as its only
parameter and returns an Integer result. A type declaration and a stub
are given below.

-}

eval :: Expr -> Integer
eval (Intgr x) = x
eval (Add x y) = (+) (eval x) (eval y)
eval (Mul x y) = (*) (eval x) (eval y)

