module Main where


import AoC                 (applyInputSWith)
import Control.Applicative (liftA2)
import Data.Array          (Array, (!), bounds, listArray)
import Data.Function       (on)
import Data.List           (intercalate, minimumBy, singleton)
import Text.Parsec         (digit, many1)
import Text.Parsec.String  (Parser)


type Image = Array (Int, Int, Int) Int


count :: Int -> Image -> Int -> Int
count n img layer = length $ filter ((==n) . (img !)) coords
  where
    (_, sizex, sizey) = snd $ bounds img
    coords            = liftA2 (layer,,) [0 .. sizex] [0 .. sizey]


solveP2 :: Image -> String
solveP2 img =
    intercalate "\n" (map rowToStr [0 .. sizex])
  where
    (layers, sizex, sizey) = snd $ bounds img
    rowToStr i = map (getPixel i) [0 .. sizey]
    getPixel i j = foldr (\l rest -> case img ! (l, i, j) of
                                        0 -> ' '
                                        1 -> '#'
                                        _ -> rest)
                         undefined
                         [0 .. layers]


solveP1 :: Image -> Int
solveP1 img = count 1 img mostZerosLayer * count 2 img mostZerosLayer
  where
    (layers, _, _) = snd $ bounds img
    mostZerosLayer = minimumBy (compare `on` count 0 img) [0 .. layers]


imageP :: Parser Image
imageP = do
    nums <- map (read . singleton) <$> many1 digit
    let layers = length nums `div` (25 * 6)
    return $ listArray ((0, 0, 0), (layers - 1, 5, 24)) nums


main :: IO ()
main = applyInputSWith imageP () solveP1 solveP2 print putStrLn