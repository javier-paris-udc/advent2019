module Main where


import           AoC                 (applyInput)
import           IntCode             (IntCode
                                     ,IntComputer
                                     ,continue
                                     ,emptyOutput
                                     ,getOutput
                                     ,intCodeP
                                     ,runState)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap, (!?), (!))
import           Data.Bifunctor      (first, second)
import           Data.List           (find, foldl', intercalate)
import           Control.Monad       (mplus)
import           Data.Maybe          (fromJust)
import qualified Data.HashSet        as Set
import           Control.Arrow       ((>>>))


data Tile  = Empty | Wall | Oxygen deriving (Show, Eq)
type Coord = (Int, Int)
type Board = HashMap Coord Tile
data Dir   = N | S | E | W deriving (Show, Eq)
type Move  = Coord -> Coord


dirToInt :: Dir -> Int
dirToInt dir = case dir of
    N -> 1
    S -> 2
    W -> 3
    E -> 4


move :: Dir -> Move
move dir = case dir of
    N -> second (+1)
    S -> second (subtract 1)
    E -> first  (+1)
    W -> first  (subtract 1)


tileToChar :: Tile -> Char
tileToChar t = case t of
    Empty  -> '.'
    Wall   -> '#'
    Oxygen -> 'o'


draw :: Board -> String
draw b = intercalate "\n" (map row [minRow .. maxRow])
  where
    minRow = minimum (map snd (Map.keys b))
    maxRow = maximum (map snd (Map.keys b))
    minCol = minimum (map fst (Map.keys b))
    maxCol = maximum (map fst (Map.keys b))
    row y  = map (\x -> if (x,y) == (0,0) then 'x'
                        else maybe ' ' tileToChar (b !? (x, y)))
                [minCol .. maxCol]


buildMap :: Board -> Coord -> IntComputer -> Board
buildMap b tile comp =
    foldl' checkNewTile b [N, S, E, W]
  where
    checkNewTile board dir
        | Map.member nextTile board = board
        | otherwise =
            let newComp  = continue [dirToInt dir] comp
            in case head $ getOutput newComp of
                0 -> addTile Wall
                1 -> buildMap (addTile Empty)  nextTile (emptyOutput newComp)
                2 -> buildMap (addTile Oxygen) nextTile (emptyOutput newComp)
                _ -> undefined
      where
        nextTile  = move dir tile
        addTile t = Map.insert nextTile t board


deep :: Coord -> (a -> a -> a) -> (a -> a) -> a -> a -> Board -> a
deep start combine emptyF oxyV a0 board = explore start (Set.singleton (0,0))
  where
    explore tile visited =
        foldr (combine . moveTo) a0 [N, S, E, W]
      where
        moveTo dir
            | Set.member nextTile visited = a0
            | otherwise =
                case board ! nextTile of
                    Wall   -> a0
                    Empty  -> emptyF (explore nextTile addTile)
                    Oxygen -> oxyV
          where
            nextTile = move dir tile
            addTile  = Set.insert nextTile visited


solveP2 :: IntCode -> Int
solveP2 code =
    let board  = buildMap (Map.singleton (0, 0) Empty) (0, 0) $ runState [] code
        oxygen = fst $ fromJust $ find (\(_, v) -> v == Oxygen) $ Map.toList board
    in deep oxygen max (+1) 0 0 board


solveP1 :: IntCode -> Int
solveP1 = runState []
      >>> buildMap (Map.singleton (0, 0) Empty) (0, 0)
      >>> deep (0, 0) mplus (fmap (+1)) (Just 1) Nothing
      >>> fromJust


main :: IO ()
main = applyInput intCodeP solveP1 solveP2