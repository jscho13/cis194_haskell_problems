toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
  | x < 0 = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x
  | x < 0 = []
  | otherwise = [x `mod` 10] ++ toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x] 
doubleEveryOther (x:xs)
  | length xs `mod` 2 == 0 = [x] ++ doubleEveryOther xs
  | otherwise = [x*2] ++ doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + sumDigits xs
  | x >= 10 = sum (toDigits x) + sumDigits xs

calculateRemainder  :: Integer -> Integer
calculateRemainder x = x `mod` 10 

validate :: Integer -> Bool
validate x
  | calculateRemainder (sumDigits (doubleEveryOther (toDigits x))) == 0 = True
  | calculateRemainder (sumDigits (doubleEveryOther (toDigits x))) /= 0 = False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

