module Main where


import AoC                 (applyInput)
import IntCode             (IntCode, intCodeP, runWithIO)
import Data.List           (unfoldr)
import Data.Maybe          (catMaybes)
import Data.Bifunctor      (first)
import Data.Function       ((&))


type Coord = (Int, Int)


isBeam :: IntCode -> Coord -> Bool
isBeam code (x, y) = (==1) $ head $ snd $ runWithIO [x, y] code


buildIntervals :: IntCode -> [(Int, Int)]
buildIntervals code =
    unfoldr slide (0, 0, 0)
  where
    slide (left, right, y) =
        let newLeft  = head $ dropWhile (not . isBeam code . (, y)) [left ..]
            newRight = [max right newLeft ..]
                     & dropWhile (isBeam code . (, y))
                     & head
                     & subtract 1
        in Just ((newLeft, newRight), (newLeft, newRight, y + 1))


solveP2 :: IntCode -> Int
solveP2 code = uncurry (+) $ first (*10000) (head squares)
  where
    intervals = buildIntervals code
    squares   = catMaybes $ zipWith3 check100Sq intervals (drop 99 intervals) [0..]
    check100Sq (_, right1) (left2, _) y
        | right1 - left2 >= 99 = Just (left2, y)
        | otherwise            = Nothing


solveP1 :: IntCode -> Int
solveP1 = sum . map intervalLen . take 50 . buildIntervals
  where
    intervalLen (st, end)
        | st > 49   = 0
        | otherwise = min 49 end - st + 1


main :: IO ()
main = applyInput intCodeP solveP1 solveP2