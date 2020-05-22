module Fibonacci where
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)
  | n > 0 = fibonacci (n - 2) + fibonacci (n - 1)

fibLin :: Integer -> Integer
fibLin n
  | n == 0 = 0
  | otherwise = fibLin' n (0, 1)

fibLin' n nums
  | n == 1 = snd nums
  | n == 0 = fst nums
  | n < 0 = fibLin' (n + 1) (snd nums, (fst nums) - (snd nums))
  | n > 0 = fibLin' (n - 1) (snd nums, (fst nums) + (snd nums))

seqA :: Integer -> Integer
seqA n
  | n == 0 = 1
  | n == 1 = 2
  | n == 2 = 3
  | otherwise = let
      seqA' (a0, a1, a2) 2 = a2
      seqA' (a0, a1, a2) n = seqA' (a1, a2, (a2 + a1 - 2 * a0)) (n - 1)
    in seqA' (1, 2, 3) n


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sum, lngth) where
  str = show $ abs x :: String
  lngth = toInteger $ length str
  sum = calcSum str 0
  calcSum "" acc = acc
  calcSum str acc = calcSum (tail str) (acc + (read $ take 1 str :: Integer))

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = (sum squeres) where
  n = 1000.0
  deltaX = (b - a) / n
  squareN x1 x2 = ((f x1 + f x2) / 2.0) * deltaX
  squeres = [
              squareN x1 x2 | i <- [0..n - 1],
              let x1 = a + i * deltaX,
              let x2 = x1 + deltaX
            ]


mono :: Char -> Char
mono x = x

semiMono :: Char -> a -> Char
semiMono x y = x
