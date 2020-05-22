module Binary where
  import Data.Char

  charToBinary :: Char -> [Char]
  charToBinary char = padTilByte $ intToBitString $ ord char where
    padTilByte str = (replicate (8 - (length str)) '0') ++ str

  intToBitString :: Int -> [Char]
  intToBitString int = result int where
    result 1 = "1"
    result 0 = "0"
    result int = (div' $! divMod int 2) $ ""
    div' (1, 0) binStr = "10" ++ binStr
    div' (1, 1) binStr = "11" ++ binStr
    div' (div, 0) binStr = (div' $! divMod div 2) $ '0' : binStr
    div' (div, 1) binStr = (div' $! divMod div 2) $ '1' : binStr

  bitStringToChar :: [Char] -> Char
  bitStringToChar bitString = chr $ toInt where
    toInt = bitStringToInt bitString

  bitStringToInt :: [Char] -> Int
  bitStringToInt bitString = foldr (+) 0 (zipWith (\x y -> (digitToInt x) * (2 ^ y)) bitString bitIndices) where
    strLen = length bitString
    bitIndices = [(strLen - 1), (strLen - 2)..0]

  upCase :: Char -> Char
  upCase char = bitStringToChar $ bitwise chrAND (charToBinary char) logicBits where
    logicBits = ['1', '1', '0', '1', '1', '1', '1', '1']

  downCase :: Char -> Char
  downCase char = bitStringToChar $ bitwise chrXOR (charToBinary char) logicBits where
    logicBits = ['0', '0', '1', '0', '0', '0', '0', '0']

  intStrToInt :: [Char] -> Int
  intStrToInt intStr = result where
    result = foldl (\x y -> x * 10 + y) 0 (map intCharToInt intStr)
    intCharToInt char = bitStringToInt $ bitwise chrAND (charToBinary char) ['0', '0', '0', '0', '1', '1', '1', '1']

  bitwise :: (Char -> Char -> Char) -> [Char] -> [Char] -> [Char]
  bitwise f bits logicBits = zipWith (\x y -> f x y) bits logicBits

  toDate :: Int -> Int -> Int -> [Char]
  toDate month day year = monthBits ++ dayBits ++ yearBits where
    monthBits = intToBitString month
    dayBits = intToBitString day
    yearBits = intToBitString $ mod year 100

  extractDay :: [Char] -> Int
  extractDay [_, _, _, _, d4, d3, d2, d1, d0, _, _, _, _, _, _, _] = bitStringToInt $ d4 : d3 : d2 : d1 : d0 : []
  extractDay bitStr
    | length bitStr >= 16 || length bitStr < 16 = 0

  extractMonth :: [Char] -> Int
  extractMonth [d3, d2, d1, d0, _, _, _, _, _, _, _, _, _, _, _, _] = bitStringToInt $ d3 : d2 : d1 : d0 : []
  extractMonth bitStr
    | length bitStr >= 16 || length bitStr < 16 = 0

  extractYear :: [Char] -> Int
  extractYear [_, _, _, _, _, _, _, _, _, d6, d5, d4, d3, d2, d1, d0] = bitStringToInt $ d6 : d5 : d4 : d3 : d2 : d1 : d0 : []
  extractYear bitStr
    | length bitStr >= 16 || length bitStr < 16 = 0

  testBit :: Int -> Int -> Bool
  testBit testValue testBit
    | bitStrLength - 1 < testBit = False
    | otherwise = doTestBit $ intBitStr !! (abs $ -(bitStrLength - 1) + testBit) where
      bitStrLength = length intBitStr
      intBitStr = intToBitString testValue
      doTestBit '1' = True
      doTestBit '0' = False

  shiftRight :: [Char] -> [Char]
  shiftRight bitStr = (init $ '0' : bitStr)

  shiftLeft :: [Char] -> [Char]
  shiftLeft bitStr
    | length bitStr <= 8 = bitStr ++ ['0']
    | otherwise = (tail bitStr) ++ ['0']

  rotateR :: [Char] -> [Char]
  rotateR bitStr = (bitStr !! (length bitStr - 1)) : (take (length bitStr - 1) bitStr)

  rotateL :: [Char] -> [Char]
  rotateL (x:xs) = xs ++ [x]


  chrXOR :: Char -> Char -> Char
  chrXOR '0' '1' = '1'
  chrXOR '1' '0' = '1'
  chrXOR '0' '0' = '0'
  chrXOR '1' '1' = '0'

  chrAND :: Char -> Char -> Char
  chrAND '0' '0' = '0'
  chrAND '1' '0' = '0'
  chrAND '0' '1' = '0'
  chrAND '1' '1' = '1'
