module Main where


import           AoC                (applyInput)
import           Data.HashSet       (HashSet)
import           Text.Parsec.String (Parser)
import qualified Data.HashSet       as Set
import           Text.Parsec        ((<|>), char, many1, newline, sepEndBy1)
import           Data.Function      ((&), on)
import           Data.List          (foldl', sortBy, unfoldr)
import           Data.Biapplicative (biliftA2, first)
import           Data.Foldable      (maximumBy)
import           Data.Maybe         (mapMaybe)


type Coord = (Int, Int)


tailMaybe :: [a] -> Maybe [a]
tailMaybe []     = Nothing
tailMaybe (_:xs) = Just xs


findInLine :: Int -> Int -> Coord -> Coord -> HashSet Coord -> [Coord]
findInLine cols rows from to asts =
    filter (`Set.member` asts) allLine
  where
    (dx, dy)      = biliftA2 (-) (-) to from
    factor        = gcd dx dy
    delta         = (dx `div` factor,  dy `div` factor)
    allLine       = unfoldr (\c -> if inside c then Just (c, next c) else Nothing)
                            $ next from
    inside (x, y) = x >= 0 && y >= 0 && x < cols && y < rows
    next          = biliftA2 (+) (+) delta


linesFrom :: HashSet Coord -> Int -> Int -> Coord -> [[Coord]]
linesFrom asteroids cols rows coord = searchVisible $ Set.delete coord asteroids
  where
    searchVisible ast  =
        case Set.toList ast of
            []      -> []
            (c : _) -> let line         = findInLine cols rows coord c ast
                           remainingAst = foldl' (flip Set.delete) ast line
                       in line : searchVisible remainingAst


angle :: Coord -> Double
angle (c, r) = axisAngle (c, -r)
  where
    axisAngle (col, row)
        | col >= 0 && row >= 0 = atan $ dCol / dRow                   -- 1st quadrant
        | col >= 0             =     pi / 2 + axisAngle (-row,  col)  -- 2nd quadrand
        | row >= 0             = 3 * pi / 2 + axisAngle ( row, -col)  -- 4th quadrant
        | otherwise            =     pi     + axisAngle (-col, -row)  -- 3th quadrant
      where
        dCol = fromIntegral col
        dRow = fromIntegral row


unroll :: [[a]] -> [a]
unroll = concat . unfoldr (\l -> if null l
                                    then Nothing
                                    else Just (map head l, mapMaybe tailMaybe l))


solveP2 :: (HashSet Coord, Int, Int) -> Int
solveP2 (asteroids, cols, rows) = unroll sortedLines !! 199
                                & first (*100)
                                & uncurry (+)
  where
    (station, views) = asteroids
                     & Set.map ((,) <*> linesFrom asteroids cols rows)
                     & maximumBy (compare `on` (length . snd))
    sortedLines      = sortBy (compare `on` (angle . minus station . head)) views
    minus c1 c2      = biliftA2 (-) (-) c2 c1


solveP1 :: (HashSet Coord, Int, Int) -> Int
solveP1 (asteroids, cols, rows) =
    maximum $ Set.map (length . linesFrom asteroids cols rows) asteroids


asteroidP :: Parser (HashSet Coord, Int, Int)
asteroidP = do
    rows <- many1 (char '#' <|> char '.') `sepEndBy1` newline
    let nCols  = length (head rows)
        nRows  = length rows
        coords = rows
               & concat
               & zip [0..]
               & filter ((=='#') . snd)
               & map (\(n, _) -> (n `mod` nCols, n `div` nCols))
    return (Set.fromList coords, nCols, nRows)


main ::IO ()
main = applyInput asteroidP solveP1 solveP2