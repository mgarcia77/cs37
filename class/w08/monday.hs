-- Notes from Monday

-- This is a single line comment
{-
  And this is a block comment!
-}


-- cube takes an argument of type 'a', which is a member of the Num typeclass,
-- and returns an argument of the same type 'a'.  

cube :: Num a => a -> a
cube n = n*n*n

cubeInt :: Integer -> Integer
cubeInt n = n*n*n


-- One way to write factorial is to use if statements as follows:
factorial :: Integer -> Integer
factorial n = if n == 0 
              then 1
              else n * (factorial (n-1))


-- But a nicer way to write factorial is to use pattern matching:

factorial' :: Integer -> Integer
factorial' 0 = 1
factorial' n = n * (factorial' (n-1))


{- We can use pattern matching on lists, too. In this example, length'
   takes a list of type 'a' and returns an Integer representing the legnth
   of the list.  Notice that the base case is represented as the first
   pattern. In the recursive step, we match a pattern such as (x:xs) against
   the list. Doing so binds 'x' to the head ("car") of the list and 'xs'
   to the tail ("cdr") of the list. Below, since we don't actually use the
   head of the list in the definition of the function, we substitute the
   underscore to represent that we don't care what the head of the list is,
   we just care that the list has a head and a tail. -}

length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + (length' xs)


-- range takes two Integers representing the low and high values in a range.
-- The result is a list of Integers from low to high (inclusive).  We could
-- write this using an if statement:
range :: Integer -> Integer -> [Integer]
range low high = if high < low
                 then []
                 else low:(range (low+1) high)


-- Alternatively, we could write range' using guards to represent
-- each of the cases we're concerned with:

range' :: Integer -> Integer -> [Integer]
range' low high
  | high < low  = []
  | otherwise   = low:(range' (low+1) high)

