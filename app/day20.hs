{-# LANGUAGE DeriveGeneric #-}
module Main where


import           AoC                 (applyInput)
import           Text.Parsec.String  (Parser)
import           Data.Array          (Array, (!))
import qualified Data.Array          as Array
import           Text.Parsec         (char, choice, many1, newline, sepEndBy1, upper)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Bifunctor      (first, second)
import           Data.List           (groupBy, nub, sort, sortBy)
import           Data.Function       ((&), on)
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import           Data.Tuple          (swap)
import           Djistra             (djistra)
import           Data.Hashable       (Hashable)
import           GHC.Generics        (Generic)
import           Data.Maybe          (fromMaybe)


data Tile      = Wall | Empty | Letter Char | Open deriving (Show, Eq)
data PortalDir = In | Out deriving (Show, Eq, Ord, Generic)
type Coord     = (Int, Int)

instance Hashable PortalDir


rev :: PortalDir -> PortalDir
rev In  = Out
rev Out = In


up, down, left, right :: Coord -> Coord
up    = first (subtract 1)
down  = first (+1)
left  = second (subtract 1)
right = second (+1)


fromLetter :: Tile -> Char
fromLetter (Letter c) = c
fromLetter _          = undefined


isLetter :: Tile -> Bool
isLetter (Letter _) = True
isLetter _          = False


distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)


contiguous :: Coord -> Coord -> Bool
contiguous c1 c2 = distance c1 c2 < 2


neighbours :: Array Coord Tile -> Coord -> [Coord]
neighbours board coord = [up, down, left, right] <*> [coord]
                       & filter (\c -> Array.inRange (Array.bounds board) c
                                    && board ! c == Open)


buildPaths :: Array Coord Tile
           -> HashMap Coord (PortalDir, String)
           -> (Coord, (PortalDir, String))
           -> ((String, PortalDir), [(String, PortalDir, Int)])
buildPaths board portalMap (coord, (d, portal)) = ((portal, d), dfs (coord :| []) 0)
  where
    dfs (c:|cs) cost =
        case portalMap Map.!? c of
            Nothing -> expand c cs cost
            Just (dir, p)
                | portal /= p -> [(p, dir, cost + 1)]
                | otherwise   -> expand c cs cost
    expand c cs cost = concatMap (\new -> dfs (new:|c:cs) (cost + 1)) (nextCs c cs)
    nextCs c cs = filter (`notElem` cs) $ neighbours board c


buildGraph :: Array Coord Tile -> HashMap (String, PortalDir) [(String, PortalDir, Int)]
buildGraph board = Map.fromListWith (++) $ map (buildPaths board portalMap) portals
  where
    portalLetters = map (second fromLetter) $ filter (isLetter . snd) $ Array.assocs board
    horizontal    = sort portalLetters
    vertical      = sortBy (compare `on` (swap.fst)) portalLetters
    portals       = groupBy (contiguous `on` fst) horizontal
                 ++ groupBy (contiguous `on` fst) vertical
                  & filter ((==2).length)
                  & map joinLetters
                  & sort
                  & nub
    portalMap     = Map.fromList portals
    joinLetters [(c1, l1), (c2, l2)] = concatMap (neighbours board) [c1, c2]
                                        & head
                                        & (, (checkBorder c1 c2, [l1, l2]))
    joinLetters _                    = undefined
    checkBorder (x1, y1) (x2, y2)
        |  x1 == 0 || x2 == 0
        || y1 == 0 || y2 == 0
        || x1 == maxX || x2 == maxX
        || y1 == maxY || y2 == maxY = Out
        | otherwise                 = In
    (_, (maxX, maxY)) = Array.bounds board


solveP2 :: Array Coord Tile -> Maybe Int
solveP2 board = subtract 1 . snd <$> djistra ("AA", Out, 0) (== ("ZZ", Out, 0)) getNext
  where
    graph = buildGraph board
    getNext :: (String, PortalDir, Int) -> [((String, PortalDir, Int), Int)]
    getNext (portal, d, level)
        | level == 0 = graph Map.! (portal, d)
                     & filter (\(p, dir, _) -> dir == In || p == "ZZ")
                     & map (\(p, dir, cost) ->
                        if dir == In then ((p, rev dir, 1), cost)
                                     else ((p, dir, 0), cost))
        | otherwise  = graph Map.! (portal, d)
                     & filter (\(p, _, _)   -> p `notElem` ["AA", "ZZ"])
                     & map (\(p, dir, cost) ->
                        if dir == In then ((p, rev dir, level + 1), cost)
                                     else ((p, rev dir, level - 1), cost))


solveP1 :: Array Coord Tile -> Maybe Int
solveP1 board = subtract 1 . snd <$> djistra "AA" (== "ZZ") getNext
  where
    graph         = buildGraph board
    getNext c     = map (\(p, _, cost) -> (p, cost)) $ getNextList c
    getNextList c = (graph Map.! (c, Out)) ++ fromMaybe [] (graph Map.!? (c, In))


mazeP :: Parser (Array Coord Tile)
mazeP = do
    mapLines <- line `sepEndBy1` newline
    let rows   = length mapLines
        cols   = length (head mapLines)
    return $ Array.listArray ((0, 0), (rows - 1, cols - 1)) (concat mapLines)
  where
    line = many1 tileP
    tileP = choice [char ' ' >> return Empty
                   ,char '#' >> return Wall
                   ,char '.' >> return Open
                   ,Letter <$> upper
                   ]


main :: IO ()
main = applyInput mazeP solveP1 solveP2