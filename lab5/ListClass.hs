module ListClass where

import Prelude (Show(..), (<>), undefined)
import qualified Data.List as List

import MyPrelude
import BoolClass
import MaybeClass
import NatClass
import PairClass
 
-- | The class of list-like types (types having a notion of
-- 'nil', 'cons'tructor, and aggregation --- 'foldr').
-- Instances should satisfy the following:
--
-- [Fold nil] @'foldr' f i 'nil' = i@
-- [Fold cons]  @'foldr' f i ('cons' x l) = f x ('foldr' f i l)@
data ListClass l a = ListClass { nil :: l a, cons :: a -> l a -> l a, foldr :: forall b. (a -> b -> b) -> b -> l a -> b }

list :: ListClass [] a
list = ListClass
  { nil = []
  , cons = (:)
  , foldr = List.foldr
  }

-- | Append two lists
(++) :: (ListClass l) => l a -> l a -> l a
(++) l = flip (foldr l (cons l))

-- >>> cons 2 (cons 3 nil) ++ cons 1 (cons 4 (cons 0 nil)) :: [CNat]
-- [C2,C3,C1,C4,C0]

-- | Returns the length of a list as a 'NatClass'
length :: ListClass l => l a -> CNat
length l = foldr l (const succ) zero

-- >>> length (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C3

-- | Test whether the list is empty. 
isNull :: ListClass l => l a -> CBool
isNull l = foldr l (const (const false)) true

-- >>> isNull (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CFalse

-- | 'map' @f@ @xs@ is the list obtained by applying @f@ to each element of @xs@
map :: ListClass l => (a -> b) -> l a -> l b
map la lb f = foldr la (cons lb . f) (nil lb)

-- >>> map (mul 2) (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- [C2,C8,C0]

-- | Applied to a predicate and a list, returns the list of those elements that satisfy the predicate
filter :: ListClass l => (a -> CBool) -> l a -> l a
filter la p = foldr la (\a l -> bool l (cons la a l) (p a)) (nil la)

-- >>> filter (not . isZero :: CNat -> CBool) (cons 1 (cons 4 (cons 0 nil))) :: [CNat]
-- [C1,C4]

-- | Left-associative fold of a list.
-- @foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn@
foldl :: (ListClass l) => (b -> a -> b) -> b -> l a -> b
foldl l agg def xs = foldr l (\a f acc -> f (agg acc a)) id xs def -- hard; use foldr to compute a function which we then apply on z

-- >>> foldl exp 2 (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C1

-- | Decompose a list into its head and tail ('nothing' for the empty list). 
uncons :: ListClass l => l a -> CMaybe (CPair a (l a))
uncons l = foldr l (\a -> just . pair a . maybe (nil l) (uncurry (cons l))) nothing

-- >>> uncons (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CJust <(C1,[C4,C0])>

-- | Extract the first element of a list into a 'MaybeClass' ('nothing' for the empty list)
head :: ListClass l => l a -> CMaybe a
head l = maybeFMap fst . uncons l

-- >>> head (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CJust C1

-- | Extract the elements after the head of a list into a 'MaybeClass' ('nothing' for the empty list)
tail :: ListClass l => l a -> CMaybe (l a)
tail l = maybeFMap snd . uncons l

-- >>> tail (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CJust [C4,C0]

-- | returns the elements of a list in reverse order.
reverse :: ListClass l => l a -> l a
reverse l = foldl l (flip (cons l)) (nil l)

-- >>> reverse (cons 1 (cons 4 (cons 0 nil))) :: [CNat]
-- [C0,C4,C1]

-- | The 'sum' function computes the sum of a list of numbers.
sum :: ListClass l => l CNat -> CNat
sum l = foldr l add zero
-- >>> sum (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C5

-- | The 'product' function computes the product of a list of numbers.
product :: ListClass l => l CNat -> CNat
product l = foldr l mul one

-- >>> product (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C0

-- | 'maximum' returns the maximum value from a list of naturals ('zero' for the empty list),
maximum :: ListClass l => l CNat -> CNat
maximum l = foldr l max zero

-- >>> maximum (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C4

-- | @'natlist' n@ generates a list containing the numbers from @1@ to @n@, in reverse order.
natToList :: ListClass l => CNat -> l CNat
natToList lnat = iter (\l -> maybe (cons lnat one (nil lnat)) (\p -> cons lnat (succ (fst p)) l) (uncons lnat l)) (nil lnat)


-- >>> natToList 10 :: [CNat]
-- [C10,C9,C8,C7,C6,C5,C4,C3,C2,C1]

newtype CList a = CList { getCList :: forall b . (a -> b -> b) -> b -> b}

clist :: ListClass CList a
clist = ListClass
  { foldr = \f i l -> getCList l f i
  , nil = CList (\f i -> i)
  , cons = \x l -> CList (\f i -> f x (getCList l f i))
  }

-- | converting between different instances of 'ListClass'
fromListClass :: (ListClass l1, ListClass l2) => l1 a -> l2 a
fromListClass l1 l2 = map l1 l2 id

-- | 'Show' instance for 'CList' (via transformation into Haskell lists)
instance Show a => Show (CList a) where
  show cl = "{" <> show (fromListClass cl :: [a]) <> "}"

-- | computes the factorial of a number
factorial :: CNat -> CNat
factorial l = product l . natToList l

-- >>> factorial 5
-- C120

-- >>> cons 2 (cons 3 nil) ++ cons 1 (cons 4 (cons 0 nil)) :: CList CNat
-- {[C2,C3,C1,C4,C0]}

-- >>> length (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C3

-- >>> isNull (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CFalse

-- >>> map (mul 2) (cons 1 (cons 4 (cons 0 nil))) :: CList CNat
-- {[C2,C8,C0]}

-- >>> filter (not . isZero :: CNat -> CBool) (cons 1 (cons 4 (cons 0 nil))) :: CList CNat
-- {[C1,C4]}

-- >>> foldl exp 2 (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C1

-- >>> uncons (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CJust <(C1,{[C4,C0]})>

-- >>> head (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CJust C1

-- >>> tail (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CJust {[C4,C0]}

-- >>> reverse (cons 1 (cons 4 (cons 0 nil))) :: CList CNat
-- {[C0,C4,C1]}

-- >>> sum (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C5

-- >>> product (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C0

-- >>> maximum (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C4

-- >>> natToList 10 :: CList CNat
-- {[C10,C9,C8,C7,C6,C5,C4,C3,C2,C1]}