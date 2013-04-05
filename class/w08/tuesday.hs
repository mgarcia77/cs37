-- Notes from Tuesday

{-
We can use the 'type' keyword to create 'type synonyms', alternate names 
for existing types. This can help us better describe the kinds of data
we're expecting into and out of our functions.

Notice the following which is already defined, shown below in ghci:

Prelude> :i String
type String = [Char] -- Defined in `GHC.Base'

We see that the name String is already defined as a synonym for [Char].
-}

-- We can define our own type synonyms

type FirstName = [Char]

-- And we can write functions that use these synonyms:

isJack :: FirstName -> Bool
isJack "Jack" = True
isJack _ = False

{-

In addition to creating type synonyms, we can create our own new data
type. Returning to ghci, we see the following (only the first line is
shown below):

Prelude> :i Bool
data Bool = False | True -- Defined in `GHC.Types'

Notice that Bool is a data type comprised of two members, False and True.

False and True are 'constructors' for the Bool type.
-}

-- We can define our own data types:

data Suit = Club | Heart | Diamond | Spade
  deriving (Show, Eq, Ord, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
     | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum)

{- 
 Notes on typeclasses:

   - deriving Show allows our data type to be converted into a String
   so that it can be printed out.  By default, simply deriving Show
   means that the text of our constructor (and any arguments -- here
   there are none) are used as the output String.

   - deriving Eq allows us to determine if two members of our data
   type are equal to one another.  By default, simply deriving Eq
   means that two items are the same if their constructor (and any
   arguments -- here there are none) are equal.

   - deriving Ord allows us to determine the ordering of two members
   of our data (e.g. less than, greater than).  By default, simply
   deriving Ord means that the order we list the items in our type
   definition determines the ordering of the items.  For example, this
   means that Club is less than Spade and Jack is less than Ace. 

   - deriving Enum allows us to enumerate items in our data type. This
   let's use do things like create a list of all the values: 
   [Two .. Ace]
   By default, the ordering of the items in Enum is, like Ord,
   determined by the order you define them in in the type definition.
-}

-- We can make a type synonym for a playing Card so that it's just a tuple
-- of a Value and a Suit. We use a tuple because it allows mixed types, 
-- unlike lists which only allow homogeneous types.

type Card = (Value, Suit)
cardLessThan :: Card -> Card -> Bool
cardLessThan (v1,s1) (v2,s2)
  | v1 /= v2  = v1 < v2       --  /= is how we write "not equal" in Haskell
  | s1 < s2   = True
  | otherwise = False

-- We can also make a type synonym for a Deck of cards, which is simply
-- a list of Cards.
type Deck = [Card]

-- We can write functions that use these new data types:

isRed :: Suit -> Bool
isRed Heart = True
isRed Diamond = True
isRed _ = False

-- And use these type synonyms

isRedCard :: Card -> Bool
isRedCard (_,s) = isRed s


-- --------------------------------------

{- 

Another built in data type (like Bool, shown above) is Maybe (only the
first line is shown):

Prelude> :i Maybe
data Maybe a = Nothing | Just a -- Defined in `Data.Maybe'

Notice first that the name of the data type isn't simple 'Bool' or
'Card' or 'Value'.  Here, it is 'Maybe a': it has a variable type as
part of the definition of the type.  Also, notice this is more than
just a list of two constructors: the first constructor, Nothing, takes
no arguments, like False, True, Clubs, Spades, Jack and Ace above.
The second constructor, Just a, takes a type variable 'a'. When we
want to use the Just constructor, we must also give it an
argument. For example, we could write:

Prelude> Just 'a'
Just 'a'
Prelude> :t (Just 'x')
(Just 'x') :: Maybe Char

Notice the type variable 'a' has been replaced by Char since we passed
a Char (here, 'x') to the Just constructor.  
-}

-- We can use 'Maybe a' to rewrite partial functions (such as head, which
-- crashes on some set of its inputs, namely the empty list) as total
-- functions:

head' :: [a] -> Maybe a
head' [] = Nothing      -- instead of crashing, return Nothing
head' (x:_) = Just x    -- pass the head of the list to the Just constructor

-- We can do the same thing for the partial function div (which fails when
-- we try to divide by 0):

div' :: Integral a => a -> a -> Maybe a
div' _ 0 = Nothing
div' a b = Just $ div a b

-- --------------------------------------

-- Function currying

{-

The type definition for the function 'not' is as follows:

Prelude> :t not
not :: Bool -> Bool

This shows that 'not' takes a Bool parameter and returns a Bool.

The type definition for logical and (&&) is as follows:

Prelude> :t (&&)
(&&) :: Bool -> Bool -> Bool

One way to read this is that (&&) takes two Bool parameters and
returns a Bool.  However, another way to read this is that (&&) takes
a Bool as a parameter and returns a function that takes a single Bool
parameter which returns a Bool.  Below, the parentheses are helping to
illustrate that:

(&&) :: Bool -> (Bool -> Bool)

Providing a Bool argument returns a function of one Bool that returns
a Bool.

In Haskell, all functions work this way: they can accept some of the
parameters, returning a function that accepts the rest of the
parameters. This idea is called 'currying'.

The elem function has the following type

Prelude> :t elem
elem :: Eq a => a -> [a] -> Bool

It takes an item of type 'a' and a list of type 'a' and returns true if
the item is in the list.  We can partially apply this function as follows: 

-}

containsX :: [Char] -> Bool 
containsX = elem 'X'

{- 
Notice that the containsX function above is the partially applied
elem function.  elem has taken the 'X' as the first parameter and is
awaiting a second parameter:

Prelude> containsX "Xray"
True
Prelude> containsX "helix"
False
-}

-- We can curry infix functions such as (/) by providing either the
-- first argument...

twelveDividedBy = (12/)      -- twelveDividedBy 6 == 2.0

-- or the second argument

dividedByTwelve = (/12)      -- dividedByTwelve 108 = 9.0

-- If we want to curry a two argument function such as elem but we
-- wish to provide the second argument, we simply have to use the
-- function in its infix form:

in1st100Positives = (`elem` [1..100])    
-- in1st100Positive 6 == True
-- in1st100Positive 104 == False

{- 
Or we can use the flip function that turns the parameters around:

Prelude> :t flip
flip :: (a -> b -> c) -> b -> a -> c
-}

twelveIntegerDivBy = div 12       -- twelveIntegerDivBy 4 = 3

integerDivByTwleve = flip div 12  -- integerDivByTwelve 108 = 9



