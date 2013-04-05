
type StudentName = [Char]

isJack :: StudentName -> Bool
isJack "Jack" = True
isJack _ = False

data Suit = Club | Heart | Diamond | Spade
  deriving (Show, Eq, Ord, Enum)  -- using default for printing, comparing

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
        | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum)

type Card = (Value, Suit)
cardLessThan :: Card -> Card -> Bool
cardLessThan (v1, s1) (v2, s2)
  | v1 < v2 = True
  | s1 < s2 = True
  |otherwise = False

type Deck = [Card]

isRed :: Suit -> Bool
isRed Heart = True
isRed Diamond = True
isRed _ = False


--------------------

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x


div' :: Integral a => a -> a -> Maybe a
div' _ 0 = Nothing
div' a b = Just $ div a b   -- operate all to right of $, then apply


--------------------

-- currying
-- functions take 1 param and return another function of 1 param...
-- until eventually returning a result (no more params)
hasThree :: [Integer] -> Bool
hasThree = elem 3

addThree :: Num a => a -> a
addThree = (+) 3

divThree :: Integral a => a -> a
--divThree = (`div` 3)   -- curry the second arg (denom)
divThree = flip div 3   -- flip param order of function
