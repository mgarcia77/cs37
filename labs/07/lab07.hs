
{-

Using lists with recursion and iteration
  
There are a total of 12 questions below.  The first 9 questions
require a recursive process solution.  The final 3 questions require
an iterative process solution.
  
You can assume that each of the functions will be called with
arguments of the proper data type (e.g. a list, an integer, etc).
  
You will probably recognize a lot of these questions from a previous
lab.  These questions are meant to serve as practice in Haskell and
you are welcome to refer to any Racket solutions to help you out.


Write your solutions in this file.  To load them into Haskell,
run "ghci" from the command line, then load in this file into the
Haskell interpreter:

: caraway; ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l lab07.hs
[1 of 1] Compiling Main             ( lab07.hs, interpreted )
Ok, modules loaded: Main.
*Main> 

You can use up and down arrows to cycle through previous commands,
including those from previous sessions. Use Control-D to exit.

To run the tests, just type main at the ghci prompt:

*Main> main
 Q1 Failed
 Q2 Failed
 Q3 Failed
 Q4 Failed
 Q5 Failed
 Q6 Failed
 Q7 Failed
 Q8 Failed
 Q9 Failed
Q10 Failed
Q11 Failed
Q12 Failed

-}

{-

1. Write a function called sumList that takes a list of numbers and
returns the sum of all the numbers in the list.  Return 0 if the list
is empty.

-}


-- The type declaration for sumList 
sumList :: Num a => [a] -> a
sumList [] = 0
sumList xs = head xs + sumList (tail xs)


{-
2. Write a function called multiplyList that takes a list of
numbers and returns the product of all the numbers in the list.
Return 1 if the list is empty.
-}

-- The type declaration for multiplyList
multiplyList :: Num a => [a] -> a
multiplyList [] = 1
multiplyList xs = head xs * multiplyList (tail xs)




{-
3. Write a function called xerox that takes an item x and a number
n and returns a list containing n copies of x.  If n is 0, xerox
returns the empty list.
-}


-- The type declaration for xerox
xerox :: a -> Integer -> [a]
xerox x n
  | n <= 0 = []
  | otherwise = x:(xerox x (n - 1))


{-
4. Write a function called elem' which tests to see if an element is a
member of a list.  You may not use the built-in function elem in your
solution. (Note the apostrophe at the end of the name of the function
you are defining.)
-}

-- The type declaration for elem'
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
  | e == x = True
  | otherwise = elem' e xs



{-
5. Write a function called insertBeforeFirst which inserts an
item into a list immediately before the first occurrence of a
specified item. 
-}

-- The type declaration for insertBeforeFirst
insertBeforeFirst :: Eq a => a -> a -> [a] -> [a]
insertBeforeFirst _ _ [] = []
insertBeforeFirst elt ins lst@(x:xs)
  | elt == x = ins:lst
  | otherwise = x:(insertBeforeFirst ins elt xs)



{-
6. Write a function called collapseFirstPair which locates the first
time that the same symbol appears twice in a row and removes one of
them.  Add your own test cases to the one case I am providing you
with (at the end of the file).
-}

-- The type declaration for collapseFirstPair
collapseFirstPair :: Eq a => [a] -> [a]
collapseFirstPair (x:y:xs)
  | x == y = x:xs
  | otherwise = x : (collapseFirstPair (y:xs))
collapseFirstPair lst = lst  -- if lst doesn't have at least 2 elts


{-
7. Write a function called collapseAllPairs which collapses all
pairs found in a list.  It should not use the collapseFirstPair
function you wrote above as part of the solution.  Add your own
test cases.
-}

-- The type declaration for collapseFirstPair
collapseAllPairs :: Eq a => [a] -> [a]
collapseAllPairs (x:y:xs)
  | x == y = collapseAllPairs (x:xs)
  | otherwise = x : (collapseAllPairs (y:xs))
collapseAllPairs lst = lst -- if lst doesn't have at least 2 elts


{-
8. Write a function called swap that takes a symbol x, a symbol y,
and a list and returns a new list with all occurrences of x
replaced by y and all occurrences of y replaced by x.
-}

-- The type declaration for swap
swap :: Eq a => a -> a -> [a] -> [a]
swap _ _ [] = []
swap x y (z:zs)
  | z == x = y : (swap x y zs)
  | z == y = x : (swap x y zs)
  | otherwise = z : (swap x y zs)



{-
9. Write a function called listReplace that takes a list of pairs
and a list and returns a new list with the first element of each
pair replaced by the second element of each pair.  This problem is
more difficult than it may initially seem.  Be sure your function
can handle both test cases below.  (The second test case is the
harder one.  See the end of the file for test cases.)
-}

-- The type declaration for listReplace
listReplace :: Eq a => [[a]] -> [a] -> [a]
listReplace _ [] = []
listReplace sub lst@(x:xs) = (findReplacement sub x) : (listReplace sub xs)
  where findReplacement [] x = x
        findReplacement ((a:b:cs):ds) x
          | x == a = b
          | otherwise = findReplacement ds x


{-
10. Rewrite the sumList function from Question 1 as an iterative
process solution. 
-}

-- The type declaration for sumListIter
sumListIter :: Num a => [a] -> a
sumListIter lst = sumHelper lst 0
  where sumHelper [] total = total
        sumHelper (x:xs) total = sumHelper xs (total + x)



{-
11. Rewrite the xerox function from Question 3 as an iterative
process solution (rather than a recursive process solution).
-}

-- The type declaration for xeroxIter
xeroxIter :: a -> Integer -> [a]
xeroxIter x n = xeroxHelper x n []

xeroxHelper :: a -> Integer -> [a] -> [a]
xeroxHelper x n xeroxed
  | n <= 0 = xeroxed
  | otherwise = xeroxHelper x (n - 1) (x:xeroxed)




{-
12. Rewrite the swap function from Question 8 as an iterative
process solution.  You should write your own reverse function.
For reference, the built-in reverse function is called reverse.
swap :: Eq a => a -> a -> [a] -> [a]
swap _ _ [] = []
swap x y (z:zs)
  | z == x = y : (swap x y zs)
  | z == y = x : (swap x y zs)
  | otherwise = z : (swap x y zs)
-}

swapIter :: Eq a => a -> a -> [a] -> [a]
swapIter x y z = swapHelper x y z []

swapHelper :: Eq a => a -> a -> [a] -> [a] -> [a]
swapHelper _ _ [] swapped = reverse' swapped
swapHelper x y (z:zs) swapped
  | z == x = swapHelper x y zs (y:swapped)
  | z == y = swapHelper x y zs (x:swapped)
  | otherwise = swapHelper x y zs (z:swapped)

reverse' :: [a] -> [a]
reverse' lst = reverseHelper lst []

reverseHelper :: [a] -> [a] -> [a]
reverseHelper [] reversed = reversed
reverseHelper (x:xs) reversed = reverseHelper xs (x:reversed)


{-

13. Write the type declaration and the function called filterOut that
takes two parameters.  The first parameter is a list and the second
parameter is something you'd like to remove from that list. The result
is that all appearances of the item you'd like to filter out are
removed.

-- filterOut [1,2,1,3,1,4,1] 1 == [2,3,4]
-- filterOut [1,2,1,3,1,4,1] 5 == [1,2,1,3,1,4,1]
-- filterOut [] 5 == [] 

-}

filterOut :: Eq a => [a] -> a -> [a]
filterOut [] _ = []
filterOut (x:xs) a
  | x == a = filterOut xs a
  | otherwise = x : (filterOut xs a)


{- 

14. Write the type declaration and the function called allInRange that
has three parameters.  The first parameter is a number representing
the low value of the range. The second parameter is a number
representing the high value of the range. The third parameter is a
list of numbers. The function returns true only if every element of
the list is greater than or equal to the low value of the range and
less than or equal to the high value of the range. Return True if the
list is empty.

-- allInRange 1 10 [1,3,5] == True
-- allInRange 1 10 [5,10,15] == False
-- allInRange 1 10 [] == True

-}

allInRange :: Ord a => a -> a -> [a] -> Bool
allInRange _ _ [] = True
allInRange low high (x:xs)
  | x < low = False
  | x > high = False
  | otherwise = allInRange low high xs


{-

15. Write the type declaration and the function called addPairs that
takes a list of numbers and adds up consecutive items in the list. If
there is only one element left in the list, just add 0 to it (or just
return it). If there are no elements in the list, this evaluates to
the empty list.

-- addPairs [8,6,7,5,3,0,9] == [14,12,3,9]
-- addPairs [] == []

-}

addPairs :: Num a => [a] -> [a]
addPairs [] = []
addPairs (x:y:zs) = (x + y) : (addPairs zs)
addPairs z = z     -- if only 1 elt left, return it


{-

16. Write the type declaration and the function called merge that
takes two lists as parameters and merges them together by first taking
one element from the first list, then one from the second list,
etc. Once one of the lists is exhausted, just add all the remaining
elements from the non-empty list to the end.

-- Note: these are just [Char], e.g. ['y','y','z'] == "yyz"
-- Your function should work on any lists, including these.

-- merge "abacab" "yyz" == "aybyazcab"

-}

merge :: [a] -> [a] -> [a]
merge lstA [] = lstA
merge [] lstB = lstB
merge (x:xs) (y:ys) = x : y : (merge xs ys)


{-

17. *Challenge* Write a function called unmerge that takes one list as
a parameter and splits it into a list of two lists. The end result
can be described by saying the elements in odd numbered positions from
the original list end up in the first list, and elements in even
numbered positions end up in the second list, though that is not
necessarily the algorithm you would use.

-- unmerge "aybyazcab" == ["abacb","yyza"]
-- unmerge [] == [[],[]]

-}

unmerge :: [a] -> [[a]]
unmerge [] = [[],[]]
unmerge elts@(x:xs) = (oddElts elts) : (oddElts xs) : []

oddElts :: [a] -> [a]
oddElts [] = []
oddElts (x:y:zs) = x : (oddElts zs)  -- if at least 2 elts in lst
oddElts x = x   -- if only 1 elt in lst, just return that lst

{- ---------------------------------------- -}

{- Tests -}
main :: IO()
main = do
  -- Q1
  if (sumList [1, 0, 2, -1, 3] == 5) && (sumList [] == 0)
  then putStrLn " Q1 Passed" 
  else putStrLn " Q1 Failed"

  -- Q2
  if (multiplyList [1, 2, -5, 3] == -30) && (multiplyList [] == 1) 
  then putStrLn " Q2 Passed" 
  else putStrLn " Q2 Failed" 

  --Q3
  if (xerox "paper" 4 == ["paper", "paper", "paper", "paper"]) && 
     (xerox [1, 2, 3] 2 == [[1, 2, 3], [1, 2, 3]])
  then putStrLn " Q3 Passed"
  else putStrLn " Q3 Failed"

  -- Q4
  if (elem' 2 [1, 2, 3] == True) &&
     (elem' 4 [] == False) &&
     (elem' 5 [1, 2, 3] == False)
  then putStrLn " Q4 Passed" 
  else putStrLn " Q4 Failed"

  -- Q5
  if (insertBeforeFirst "pod" "pea" ["a", "green", "pod"] == 
        ["a","green","pea","pod"]) &&
     (insertBeforeFirst "pod" "pea" ["pod", "pod", "pod"] ==
        ["pea", "pod", "pod", "pod"]) &&
     (insertBeforeFirst "pod" "pea" ["a", "green", "leaf"] ==
        ["a","green","leaf"])
  then putStrLn " Q5 Passed"
  else putStrLn " Q5 Failed"

  -- Q6
  if (collapseFirstPair ["a", "b", "c", "d", "c", "c", "d", "d"] ==
        ["a", "b", "c", "d", "c", "d", "d"]) &&
     (collapseFirstPair ["a", "a", "a", "a", "a"] ==
        ["a", "a", "a", "a"]) &&
     (collapseFirstPair ["a", "b", "c", "d", "e", "f"] ==
        ["a", "b", "c", "d", "e", "f"])
  then putStrLn " Q6 Passed"
  else putStrLn " Q6 Failed"

  -- Q7
  if (collapseAllPairs ["a","b","b","b","c","c","c","d","d","e","f","f"] ==
        ["a", "b", "c", "d", "e", "f"]) &&
     (collapseAllPairs ["a", "a", "a", "a", "a"] ==
        ["a"]) &&
     (collapseAllPairs ["a", "b", "c", "d", "e", "f"] ==
        ["a", "b", "c", "d", "e", "f"])
  then putStrLn " Q7 Passed" 
  else putStrLn " Q7 Failed"

  -- Q8
  if (swap "red" "blue" ["red","fish","blue","fish","red"] ==
        ["blue", "fish", "red", "fish", "blue"]) &&
     (swap "red" "blue" ["red","fish","red","fish","red"] ==
        ["blue", "fish", "blue", "fish", "blue"]) &&
     (swap "red" "blue" ["fish", "fish", "fish", "fish"] ==
        ["fish", "fish", "fish", "fish"])
  then putStrLn " Q8 Passed"
  else putStrLn " Q8 Failed"

  -- Q9
  if (listReplace 
       [["carrots", "peas"], ["fork", "spoon"]]
        ["eat", "your", "carrots", "with", "a", "fork"] ==
       ["eat", "your", "peas", "with", "a", "spoon"]) &&     
     (listReplace
       [["carrots", "peas"], ["peas", "potatoes"]]
        ["eat", "your", "carrots", "and", "peas"] ==
       ["eat", "your", "peas", "and", "potatoes"])
  then putStrLn " Q9 Passed" 
  else putStrLn " Q9 Failed"

  -- Q10
  if (sumListIter [1, 0, 2, -1, 3] == 5) && (sumListIter [] == 0)
  then putStrLn "Q10 Passed" 
  else putStrLn "Q10 Failed"

  -- Q11
  if (xeroxIter "paper" 4 == ["paper", "paper", "paper", "paper"]) && 
     (xeroxIter [1, 2, 3] 2 == [[1, 2, 3], [1, 2, 3]])
  then putStrLn "Q11 Passed"
  else putStrLn "Q11 Failed"

  -- Q12
  if (swapIter "red" "blue" ["red","fish","blue","fish","red"] ==
        ["blue", "fish", "red", "fish", "blue"])
  then putStrLn "Q12 Passed"
  else putStrLn "Q12 Failed"


  -- Q13
  if (filterOut [1,2,1,3,1,4,1] 1 == [2,3,4]) && 
     (filterOut [1,2,1,3,1,4,1] 5 == [1,2,1,3,1,4,1]) &&
     (filterOut [] 5 == [])
  then putStrLn "Q13 successful"
  else putStrLn "Q13 unsuccessful"

  -- Q14
  if (allInRange 1 10 [1,3,5] == True) &&
     (allInRange 1 10 [5,10,15] == False) &&
     (allInRange 1 10 [] == True)
  then putStrLn "Q14 successful"
  else putStrLn "Q14 unsuccessful"

  -- Q15
  if (addPairs [8,6,7,5,3,0,9] == [14,12,3,9]) &&
     (addPairs [] == [])
  then putStrLn "Q15 successful"
  else putStrLn "Q15 unsuccessful"

  -- Q16
  if (merge "abacab" "yyz" == "aybyazcab") &&
     (merge [1,2,3,4] [5,6,7,8] == [1,5,2,6,3,7,4,8]) &&
     (merge [] [1,2] == [1,2])
  then putStrLn "Q16 successful"
  else putStrLn "Q16 unsuccessful"

  -- Q17
  if (unmerge "aybyazcab" == ["abacb","yyza"]) &&
     (unmerge [1,2,3,4,5,6] == [[1,3,5],[2,4,6]])
  then putStrLn "Q17 successful"
  else putStrLn "Q17 unsuccessful"

