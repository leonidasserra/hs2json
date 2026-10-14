module QuickTestes where

import Data.List (sort, (\\))
import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum' :: Ord a => [a] -> Property
prop_minimum' xs = not (null xs) ==> take 1 (qsort xs) == [minimum xs]

prop_ordered :: Ord a => [a] -> Bool
prop_ordered xs = ordered (qsort xs)
    where ordered (x:y:ys) = x <= y && ordered (y:ys)
          ordered _        = True

prop_permutation :: Ord a => [a] -> Bool
prop_permutation xs = permutation xs (qsort xs)
    where permutation as' bs = null (as' \\ bs) && null (bs \\ as')

prop_maximum :: Ord a => [a] -> Property
prop_maximum xs =
    not (null xs) ==>
        take 1 (reverse (qsort xs)) == [maximum xs]

prop_append :: Ord a => [a] -> [a] -> Property
prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        take 1 (qsort (xs ++ ys)) == [min (minimum xs) (minimum ys)]

prop_sort_model :: Ord a => [a] -> Bool
prop_sort_model xs = sort xs == qsort xs

data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq, Show)

instance Arbitrary Ternary where
    arbitrary = elements [Yes, No, Unknown]