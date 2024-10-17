toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = x `mod` 10 : toDigits (x `div` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x
  | null x = []
  | even (length x) = head x * 2 : doubleEveryOther (tail x)
  | otherwise = head x : doubleEveryOther (tail x)

sumDigits :: [Integer] -> Integer
sumDigits x
  | null x = 0
  | otherwise = sum (toDigitsRev (head x)) + sumDigits (tail x)

validate :: Integer -> Bool
validate x
  | sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0 = True
  | otherwise = False

main = do
  print (validate 4012888888881881)
  print (validate 4012888888881882)
