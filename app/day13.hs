module Main where


import           AoC                 (applyInput)
import           IntCode             (IntCode
                                     ,IntComputer
                                     ,continue
                                     ,emptyOutput
                                     ,getOutput
                                     ,halted
                                     ,intCodeP
                                     ,runWithIO
                                     ,runState)
import           Control.Arrow       ((>>>))
import           Data.List           (find, foldl', intercalate, unfoldr)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap, (!?))
import           Data.Maybe          (fromJust, fromMaybe)
import           Data.Function       ((&))
import           Data.Bifunctor      (first)


data Dir = SE | SW | NE | NW deriving (Show, Eq)
type Coord = (Int, Int)
type Board = HashMap Coord Int


ball, empty, paddle, wall :: Int
ball   = 4
paddle = 3
empty  = 0
wall   = 1


chunksOf :: Int -> [Int] -> [[Int]]
chunksOf n = unfoldr (\l -> if null l then Nothing else Just (splitAt n l))


sepCoords :: [a] -> ((a, a), a)
sepCoords [x, y, v] = ((x, y), v)
sepCoords _         = undefined


sepToList :: [Int] -> [(Coord, Int)]
sepToList = map sepCoords . chunksOf 3


codeToChar :: Int -> Char
codeToChar x = case x of
    0 -> ' '
    1 -> '#'
    2 -> '~'
    3 -> '='
    4 -> 'o'
    _ -> undefined


draw :: Board -> String
draw m = intercalate "\n" (map row [0 .. maxRow])
  where
    maxCol = maximum $ map fst $ Map.keys m
    maxRow = maximum $ map snd $ Map.keys m
    row i = map (\j -> codeToChar $ fromMaybe 0 (m !? (j, i))) [0 .. maxCol]


buildMap :: Board -> [Int] -> Board
buildMap board = sepToList
             >>> foldl' modifyMap board
  where
    modifyMap m (c,v)
        | v == 0    = Map.delete c m
        | otherwise = Map.insert c v m


estimateBall :: IntComputer -> Coord
estimateBall comp
    | snd current == 18 = current
    | otherwise =   comp
                  & emptyOutput
                  & continue [0]
                  & getOutput
                  & getPos ball
  where
    current = getPos ball (getOutput comp)


estimateMove :: Coord -> Coord -> Int
estimateMove (ballx, _) (paddlex, _) = signum (ballx - paddlex)


getPos :: Int -> [Int] -> Coord
getPos e = sepToList >>> find ((== e). snd) >>> fromMaybe ((0,0),0) >>> fst


play :: IntComputer -> IntComputer
play c0 =
    playWithBall map0 paddle0 c0
  where
    playWithBall m paddlePos comp
        | halted comp = comp
        | otherwise   =
            let nextBallPos = estimateBall comp
                nextMove    = estimateMove nextBallPos paddlePos
                newComp     = continue [nextMove] (emptyOutput comp)
                newMap      = buildMap m (getOutput newComp)
            in playWithBall newMap (first (+nextMove) paddlePos) newComp

    map0    = buildMap Map.empty (getOutput c0)
    paddle0 = getPos paddle (getOutput c0)


solveP2 :: IntCode -> Int
solveP2 code = play machine0
             & getOutput
             & sepToList
             & find ((== (-1, 0)) . fst)
             & fromJust
             & snd
  where
    machine0 = runState [] (Map.insert 0 2 code)


solveP1 :: IntCode -> Int
solveP1 = runWithIO []
      >>> snd
      >>> chunksOf 3
      >>> map sepCoords
      >>> Map.fromList
      >>> Map.filter (==2)
      >>> Map.size


main :: IO ()
main = applyInput intCodeP solveP1 solveP2