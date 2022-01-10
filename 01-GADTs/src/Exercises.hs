{-# LANGUAGE GADTs #-}
module Exercises where

import Data.Foldable (fold)

{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
import GHC.IO.Handle.Text (memcpy)
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CLNil :: CountableList
  CLCons :: Countable a => a -> CountableList -> CountableList

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CLNil = 0
countList (CLCons x xs) = count x + countList xs


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CLNil = CLNil
dropZero (CLCons x xs) | count x > 0 = CLCons x $ dropZero xs
dropZero (CLCons _ xs) = dropZero xs

-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = undefined





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  ANil :: AnyList
  ACons :: a -> AnyList -> AnyList

anyFromList :: [a] -> AnyList
anyFromList = foldr ACons ANil

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList xs = rev xs id
  where
    rev ANil f = f ANil
    rev (ACons x xs) f = rev xs (ACons x . f)

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

lengthAnyList :: AnyList -> Int
lengthAnyList ANil = 0
lengthAnyList (ACons _ xs) = 1 + lengthAnyList xs

foldAnyList :: Monoid m => AnyList -> m
-- This operation implementation by definition is always equal to mempty
-- So it's useless
-- foldAnyList ANil = mempty 
-- foldAnyList (ACons x xs) = mappend mempty $ foldAnyList xs 
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList ANil = True 
isEmptyAnyList _ = False

-- We don't have access to Show constraint, so we can only
-- print values that we now for sure: shape and Nil
instance Show AnyList where
  show (ACons x xs) = "_:" ++ show xs
  show ANil = "()"


{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq out => Eq (TransformableTo out) where
  (TransformWith lt lv) == (TransformWith rt rv) = lt lv == rt rv

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap g (TransformWith f input) = TransformWith (g . f) input

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

isEq :: EqPair -> Bool
isEq (EqPair x y) = x == y

isNotEq :: EqPair -> Bool
isNotEq = not . isEq

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox x _)) = x

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ rest) = 1 + countLayers rest
countLayers (StringBox _ rest) = 1 + countLayers rest
countLayers (BoolBox _ rest) = 1 + countLayers rest

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- Cannot do


{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

head :: HList (a, b) -> a
head (HCons x _) = x

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- Cannot beacuse we only construct tuples with length 4
-- With set of constructors given above
patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?





{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: HTree left -> centre -> HTree right
    -> HTree (Branch left center right)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

withoutLeft :: HTree (Branch left center right) -> HTree (Branch Empty center right)
withoutLeft (HBranch _ c r) = HBranch HEmpty c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!
{-
instance Eq (HTree Empty) where
  _ == _ = True

instance (Eq (HTree left), Eq centre, Eq (HTree right))
    => Eq (HTree (Branch left centre right)) where
  HBranch l x r == HBranch l' x' r' = l == l' && x == x' && r == r'
-}
{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  AlNil :: AlternatingList a b
  AlCons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts (AlCons a (AlCons _ as)) = a : getFirsts as
getFirsts AlNil = []

getSeconds :: AlternatingList a b -> [b]
getSeconds (AlCons _ (AlCons b as)) = b : getSeconds as
getSeconds AlNil = []

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
-- foldValues x = (fold $ getFirsts x, fold $ getSeconds x)
foldValues AlNil = (mempty, mempty)
foldValues (AlCons a rest) =
  let (b, a') = foldValues rest
  in (mappend a a', b)



{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool
  Func      :: (a -> Expr b)                    -> Expr (a -> b)
  App       :: Expr (a -> b) -> Expr a          -> Expr b

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals x y) = eval x == eval y
eval (Add x y) = eval x + eval y
eval (If cond t f) = if eval cond then eval t else eval f
eval (App f arg) = eval f $ eval arg
eval (Func f) = eval . f
eval (IntValue x) = x
eval (BoolValue b) = b

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse = error "Implement me"

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  AppNil :: TypeAlignedList a a
  AppCons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c


-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs xs AppNil = xs
composeTALs xs (AppCons y ys) = AppCons y (composeTALS xs ys)

