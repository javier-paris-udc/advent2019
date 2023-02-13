module Main where


import           AoC                (applyInput)
import qualified Data.Array         as Array
import           Data.Array         (Array, (!), inRange, range)
import           Data.Bifunctor     (first, Bifunctor (second))
import           Data.Bits          (shiftL)
import           Data.Function      ((&))
import qualified Data.HashSet       as Set
import           Data.HashSet       (HashSet)
import           Text.Parsec        ((<|>), char, many1, newline, sepEndBy1)
import           Text.Parsec.String (Parser)


type Coord  = (Int, Int)
type Coord3 = (Int, Int, Int)


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x


up, down, left, right :: Coord -> Coord
up    = first (subtract 1)
down  = first (+1)
left  = second (subtract 1)
right = second (+1)


toInt :: Array Coord Bool -> Int
toInt = sum . map ((1 `shiftL`) . fst). filter snd . zip [0..] . Array.elems


up3 :: Coord3 -> [Coord3]
up3 (x, y, z)
    | y == 0           = [(x + 1, 1    , 2)]
    | y == 3 && z == 2 = [(x - 1, 4    , newz) | newz <- [0 .. 4]]
    | otherwise        = [(x    , y - 1, z)]


down3 :: Coord3 -> [Coord3]
down3 (x, y, z)
    | y == 4           = [(x + 1, 3    , 2)]
    | y == 1 && z == 2 = [(x - 1, 0    , newz) | newz <- [0 .. 4]]
    | otherwise        = [(x    , y + 1, z)]


left3 :: Coord3 -> [Coord3]
left3 (x, y, z)
    | z == 0           = [(x + 1, 2   , 1)]
    | z == 3 && y == 2 = [(x - 1, newy, 4) | newy <- [0 .. 4]]
    | otherwise        = [(x    , y   , z - 1)]


right3 :: Coord3 -> [Coord3]
right3 (x, y, z)
    | z == 4           = [(x + 1, 2   , 3)]
    | z == 1 && y == 2 = [(x - 1, newy, 0) | newy <- [0 .. 4]]
    | otherwise        = [(x    , y   , z + 1)]


infestLevels :: HashSet Coord3 -> Int -> Int
infestLevels bugSet minutes
    | minutes == 0 = Set.size bugSet
    | otherwise    = infestLevels (Set.fromList (filter bugRules coords)) (minutes - 1)
  where
    level = let levels = Set.map fst3 bugSet
            in max (maximum levels) (abs $ minimum levels)

    coords = [(x, y, z) | x <- [-level - 1 .. level + 1]
                        , y <- [0 .. 4]
                        , z <- [0 .. 4]
                        , y /= 2 || z /= 2]

    neighbours3 c = up3 c ++ down3 c ++ left3 c ++ right3 c

    bugRules c = (c `Set.member` bugSet && bugsAround == 1)
              || (not (c `Set.member` bugSet) && bugsAround `elem` [1, 2])
      where
        bugsAround = length $ filter (`Set.member` bugSet) (neighbours3 c)


infest :: HashSet Int -> Array Coord Bool -> Array Coord Bool
infest prev eris
    | toInt eris `Set.member` prev = eris
    | otherwise              = infest (Set.insert (toInt eris) prev) next
  where
    next         = Array.listArray ran (map update (range ran))
    ran          = ((0, 0), (4, 4))
    neighbours c = length $ filter (\pos -> inRange ran pos && eris ! pos)
                                   ([up, down, left, right] <*> [c])
    update c
        | eris ! c  = neighbours c == 1
        | otherwise = neighbours c `elem` [1,2]


solveP2 :: Array Coord Bool -> Int
solveP2 level0 = infestLevels bugSet 200
  where
    bugSet :: HashSet (Int, Int, Int)
    bugSet = Array.assocs level0
           & filter snd
           & map ((\(x, y) -> (0, x, y)) . fst)
           & Set.fromList


solveP1 :: Array Coord Bool -> Int
solveP1 = toInt . infest Set.empty


erisP :: Parser (Array Coord Bool)
erisP = do
    rs <- many1 (bugP <|> emptyP) `sepEndBy1` newline
    let rows = length rs
        cols = length $ head rs
    return (Array.listArray ((0, 0), (rows - 1, cols - 1)) (concat rs))
  where
    bugP = char '#' >> return True
    emptyP = char '.' >> return False


main :: IO ()
main = applyInput erisP solveP1 solveP2