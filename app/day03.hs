module Main where


import           AoC                 (applyInput, commaSepP, intP)
import           Control.Applicative (liftA2)
import           Control.Arrow       ((>>>))
import           Data.Bifunctor      (bimap, first, second)
import           Data.Function       ((&))
import           Data.List           (elemIndex)
import           Data.Maybe          (fromJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Text.Parsec         (char, choice, newline, sepBy1)
import           Text.Parsec.String  (Parser)


data Dir = L | R | U | D deriving (Show, Eq)
data Move = Move Dir Int deriving (Show, Eq)
type Coord = (Int, Int)


move :: Dir -> Coord -> Coord
move dir = case dir of
    L -> second (subtract 1)
    R -> second (+1)
    U -> first  (subtract 1)
    D -> first  (+1)


movesToList :: Coord -> [Move] -> [Coord]
movesToList c l = case l of
    []              -> []
    (Move dir n):ms ->
        let thisMove = take n $ drop 1 $ iterate (move dir) c
        in thisMove ++ movesToList (last thisMove) ms


wireToSet :: [Move] -> Set Coord
wireToSet = Set.fromList . movesToList (0, 0)


distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


solveP2 :: ([Move], [Move]) -> Int
solveP2 (w1, w2) = common
                 & Set.map (\c -> liftA2 (+) (elemIndex c coords1) (elemIndex c coords2))
                 & minimum
                 & fromJust
                 & (+ 2)
  where
    coords1 = movesToList (0,0) w1
    coords2 = movesToList (0,0) w2
    common  = Set.fromList coords1 `Set.intersection` Set.fromList coords2


solveP1 :: ([Move], [Move]) -> Int
solveP1 = bimap wireToSet wireToSet
      >>> uncurry Set.intersection
      >>> Set.map (distance (0,0))
      >>> minimum


wiresP :: Parser ([Move], [Move])
wiresP = do
    wire1 <- movesP
    _     <- newline
    wire2 <- movesP
    return (wire1, wire2)
  where
    movesP = moveP `sepBy1` commaSepP
    moveP  = choice [char 'R' >> Move R <$> intP
                    ,char 'L' >> Move L <$> intP
                    ,char 'U' >> Move U <$> intP
                    ,char 'D' >> Move D <$> intP]


main :: IO ()
main = applyInput wiresP solveP1 solveP2