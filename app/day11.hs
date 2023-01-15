module Main where
import AoC (applyInputSWith)
import IntCode (intCodeP, IntCode, runState, IntComputer, continue, getOutput, halted, emptyOutput)
import Data.Bifunctor (first, second)
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Internal.Strict (HashMap, (!?))
import Data.Maybe (fromMaybe)
import Data.List (intercalate)


type Coord = (Int, Int)
data Dir   = N | E | S | W deriving (Show, Eq)


left :: Dir -> Dir
left d = case d of
    N -> W
    E -> N
    S -> E
    W -> S


right :: Dir -> Dir
right d = case d of
    N -> E
    E -> S
    S -> W
    W -> N


move :: Dir -> Coord -> Coord
move d = case d of
    N -> first  (subtract 1)
    S -> first  (+1)
    E -> second (+1)
    W -> second (subtract 1)


robot :: IntComputer -> Dir -> Coord -> HashMap Coord Int -> HashMap Coord Int
robot comp dir coord grid
    | halted comp = grid
    | otherwise   =
        let newComp        = continue [color] comp
            turnDir        = getOutput newComp !! 0
            col            = getOutput newComp !! 1
            newDir         = turn turnDir dir
        in robot (emptyOutput newComp)
                 newDir
                 (move newDir coord)
                 (Map.insert coord col grid)
  where
    color  = fromMaybe 0 (grid !? coord)
    turn turnDir
        | turnDir == 0 = left
        | otherwise    = right


solveP2 :: IntCode -> String
solveP2 code = intercalate "\n" (map row [minX .. maxX])
  where
    grid  = robot (runState [] code) N (0, 0) (Map.singleton (0, 0) 1)
    maxX  = maximum (map fst $ Map.keys grid)
    minX  = minimum (map fst $ Map.keys grid)
    maxY  = maximum (map snd $ Map.keys grid)
    minY  = minimum (map snd $ Map.keys grid)
    row i = map (\j -> maybe ' ' colToChar (grid !? (i, j))) [minY .. maxY]
    colToChar 0 = ' '
    colToChar _ = '#'


solveP1 :: IntCode -> Int
solveP1 code = Map.size $ robot (runState [] code) N (0, 0) Map.empty


main :: IO ()
main = applyInputSWith intCodeP () solveP1 solveP2 print putStrLn