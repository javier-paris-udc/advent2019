module Main where


import AoC                (applyInput, intP)
import Data.List          (group)
import Text.Parsec        (char)
import Text.Parsec.String (Parser)


intToDigits :: Int -> [Int]
intToDigits = reverse . numToDigitsRev
  where
    numToDigitsRev n
        | n == 0    = []
        | otherwise = n `rem` 10 : numToDigitsRev (n `div` 10)


monotonic :: [Int] -> Bool
monotonic = and . (zipWith (<=) <*> tail)


solveP2 :: (Int, Int) -> Int
solveP2 (st, end) = length $ filter isValidP2 [st .. end]
  where
    isValidP2 n = let digits = intToDigits n
                  in monotonic digits && any ((==2) . length)  (group digits)


solveP1 :: (Int, Int) -> Int
solveP1 (st, end) = length $ filter isValidP1 [st .. end]
  where
    isValidP1 x = let digits = intToDigits x
                  in monotonic digits && or  (zipWith (==) digits (tail digits))


numPairP :: Parser (Int, Int)
numPairP = do
    n1 <- intP
    _  <- char '-'
    n2 <- intP
    return (n1, n2)


main :: IO ()
main = applyInput numPairP solveP1 solveP2