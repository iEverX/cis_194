toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10:toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns =
    let doit _ [] = []
        doit double (x:xs)
            | double = (2*x):doit False xs
            | otherwise = x:doit True xs
    in
        doit (length ns `mod` 2 == 0) ns

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigitsRev

validate :: Integer -> Bool
validate n = mod v 10 == 0 where
    v = sumDigits . doubleEveryOther . toDigits $ n



type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
    | n == 1 = [(a,b)]
    | otherwise = hanoi (n - 1) a c b ++ (a,b):hanoi (n-1) c b a
