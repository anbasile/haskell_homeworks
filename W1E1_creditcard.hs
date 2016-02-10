toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  |n <= 0 = []
  |mod n 10 == n = reverse[n]
  |otherwise = mod n 10: toDigitsRev (div n 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x:y*2:doubleEveryOther xs


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x)+sumDigits xs

validate :: Integer -> Bool
validate n
  |sumDigits (reverse (doubleEveryOther (toDigitsRev n))) `mod` 10 == 0 = True
  |sumDigits (reverse (doubleEveryOther (toDigitsRev n))) `mod` 10 /= 0 = False







