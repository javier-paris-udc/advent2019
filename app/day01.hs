module Main where


import AoC (applyInput, intP)
import Text.Parsec.String (Parser)
import Text.Parsec (sepEndBy, newline)


fuel4Module :: Int -> Int
fuel4Module = subtract 2 . (`div` 3)


fuel :: Int -> Int
fuel = sum . takeWhile (> 0). drop 1 . iterate fuel4Module


solveP2 :: [Int] -> Int
solveP2 = sum . map fuel


solveP1 :: [Int] -> Int
solveP1 = sum . map fuel4Module


fuelP :: Parser [Int]
fuelP = intP `sepEndBy` newline


main :: IO ()
main = applyInput fuelP solveP1 solveP2