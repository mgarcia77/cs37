-- Adapted from http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf

{-
A credit card number is a series of 16 digits. However, not all 
16 digit sequences are valid credit card numbers. Below, you will
implement the credit card validation algorithm which uses these steps:

- Double the value of every second digit beginning from the *right*.
That is, the last digit is unchanged; the second-to-last digit is 
doubled; the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].

- Add the digits of the doubled values and the undoubled digits from
the original number. For example, [2,3,16,6] becomes 2+3+1+6+6 = 18

- Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8. If the result equals 0, 
then the number is valid.

-}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- Exercise 1. Deﬁne these functions to convert an Integer
-- into a list of digits.

{- converts a positive Integer into a list of digits.
   returns an empty list if the input is <= 0 -}
toDigits :: Integer -> [Integer]
toDigits _ = []  -- stub

-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []

{- converts a positive Integer into a reversed list
    of digits. Inputs <= 0 return an empty list. -}
toDigitsRev :: Integer -> [Integer]
toDigitsRev _ = []  -- stub

-- toDigitsRev 1234 == [4,3,2,1]
-- toDigitsRev 0 == []

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- Exercise 2. Double every other Integer in a [Integer],
-- beginning from the **right**:

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther _ = []  -- stub

-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- Exercise 3. Add of all the individual digits in an [Integer].

sumDigits :: [Integer] -> Integer
sumDigits _ = 0  -- stub

-- sumDigits [16,7,12,5] == 1+6 + 7 + 1+2 + 5


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

-- Exercise 4. Define a function that determines whether an 
-- Integer is a valid credit card number, using the functions
-- defined above.

validate :: Integer -> Bool
validate _ = False  -- stub

-- validate 4012888888881881 == True
-- validate 4012888888881882 == False

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

{- Tests -}
main :: IO()
main = do 
  if (toDigits 1234 == [1,2,3,4]) && (toDigits 0 == []) &&
	 (toDigitsRev 1234 == [4,3,2,1]) && (toDigitsRev 0 == []) 
  then putStrLn " Q1 Passed"
  else putStrLn " Q1 Failed"


  if (doubleEveryOther [8,7,6,5] == [16,7,12,5]) &&
  	 (doubleEveryOther [1,2,3] == [1,4,3])
  then putStrLn " Q2 Passed"
  else putStrLn " Q2 Failed"

  if (sumDigits [16,7,12,5] == 22)
  then putStrLn " Q3 Passed"
  else putStrLn " Q3 Failed"

  if (validate 4012888888881881 == True) && 
  	 (validate 4012888888881882 == False)
  then putStrLn " Q4 Passed"
  else putStrLn " Q4 Failed"

