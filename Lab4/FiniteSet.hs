import Data.List (sort, nub, intersect, union, (\\))


class FiniteSet a b where
  isMember :: b -> a b -> Bool
  toList :: a b -> [b]
  fromList :: [b] -> a b
  intersection :: a b -> a b -> a b
  unionSets :: a b -> a b -> a b
  difference :: a b -> a b -> a b


data OrderedNaturalSet a = OrderedNaturalSet [a] deriving Show

instance FiniteSet OrderedNaturalSet Int where
  isMember x (OrderedNaturalSet xs) = x `elem` xs
  toList (OrderedNaturalSet xs) = xs
  fromList xs = OrderedNaturalSet (sort (nub xs))
  intersection (OrderedNaturalSet xs) (OrderedNaturalSet ys) = OrderedNaturalSet (intersect xs ys)
  unionSets (OrderedNaturalSet xs) (OrderedNaturalSet ys) = OrderedNaturalSet (union xs ys)
  difference (OrderedNaturalSet xs) (OrderedNaturalSet ys) = OrderedNaturalSet (xs \\ ys)


data FlagSet a = FlagSet [a] deriving Show

instance FiniteSet FlagSet Bool where
  isMember x (FlagSet flags) = x `elem` flags
  toList (FlagSet flags) = flags
  fromList xs = FlagSet xs
  intersection (FlagSet flags1) (FlagSet flags2) = FlagSet (zipWith (&&) flags1 flags2)
  unionSets (FlagSet flags1) (FlagSet flags2) = FlagSet (zipWith (||) flags1 flags2)
  difference (FlagSet flags1) (FlagSet flags2) = FlagSet (zipWith (\x y -> x && not y) flags1 flags2)


-- Examples
set1 = fromList [1, 1, 1, 1, 2, 2, 3, 4, 5] :: OrderedNaturalSet Int
set2 = fromList [3, 4, 5, 6, 7, 8, 9, 10] :: OrderedNaturalSet Int
set3 = fromList [7, 8] :: OrderedNaturalSet Int


set4 = fromList [True, True, True, False, False, True] :: FlagSet Bool
set5 = fromList [True, False, True, False, True, False] :: FlagSet Bool
set6 = fromList [True, False, False] :: FlagSet Bool
set7 = fromList [True] :: FlagSet Bool