module StepikLists where
  import Data.List
  import Data.Char

  oddsOnly :: Integral a => [a] -> [a]
  oddsOnly xs = filter xs where
    filter [] = []
    filter (x:xs)
      | odd x = x : (filter xs)
      | otherwise = filter xs

  sum3 :: Num a => [a] -> [a] -> [a] -> [a]
  sum3 [] [] [] = []
  sum3 xs ys zs = sum' xs ys zs where
    sum' xs ys zs = ((getInt xs) + (getInt ys) + (getInt zs)) :
      (
        sum3 (safeTail xs) (safeTail ys) (safeTail zs)
      )
    safeTail [] = []
    safeTail (x:xs) = xs
    getInt [] = 0
    getInt (x:xs) = x

  groupElems :: Eq a => [a] -> [[a]]
  groupElems [] = []
  groupElems xs =
    let (streak, rest) = span ((==) (head xs)) xs
    in streak : groupElems rest

  filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
  filterDisj f1 f2 xs = filter (\x -> f1 x || f2 x) xs

  qsort :: Ord a => [a] -> [a]
  qsort [] = []
  qsort (x:xs) = less ++ [x] ++ more where
    less = qsort $ filter (< x) xs
    more = qsort $ filter (>= x) xs

  squares'n'cubes :: Num a => [a] -> [a]
  squares'n'cubes list = concatMap (\x -> [x ^ 2, x ^ 3]) list

  permutations' :: [a] -> [[a]]
  permutations' list = foldr (\x y -> concatMap (insertEverywhere x) y) [[]] list where
    insertEverywhere :: a -> [a] -> [[a]]
    insertEverywhere x [] = [[x]]
    insertEverywhere x l@(y:ys) = (x:l) : map (y:) (insertEverywhere x ys)

  delAllUpper :: String -> String
  delAllUpper = unwords . filter (not . isUpperWord) . words where
    isUpperWord = all isUpper

  max3 :: Ord a => [a] -> [a] -> [a] -> [a]
  max3 = zipWith3 (\x y z -> maximum [x, y, z])
