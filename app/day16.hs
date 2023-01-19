module Main where

import AoC                (applyInput)
import Data.List          (foldl')
import Text.Parsec        (digit, many1)
import Text.Parsec.String (Parser)


patternList :: Int -> [Int]
patternList n = tail $ cycle $ concatMap (replicate n) [0, 1, 0, -1]


fft :: [Int] -> [Int]
fft l =
    map ((`mod` 10).abs.sum.zipWith (*) l.patternList) [1 .. length l]


applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n == 0    = x
    | otherwise = applyN (n - 1) f (f x)


listToInt :: [Int] -> Int
listToInt = foldl' (\n i -> n * 10 + i) 0


solveP2 :: [Int] -> Int
solveP2 l = listToInt $ take 8 finalL
  where
    offset    = listToInt (take 7 l)
    extendedL = take (10_000 * length l) (cycle l)
    relevantL = drop offset extendedL
    finalL    = map (`mod` 10)  $ applyN 100 (scanr1 (+)) relevantL


solveP1 :: [Int] -> Int
solveP1 l =
    let finalNum = applyN 100 fft l
    in  listToInt $ take 8 finalNum


numbersP :: Parser [Int]
numbersP = many1 (read.(:[]) <$> digit)


main :: IO ()
main = applyInput numbersP solveP1 solveP2