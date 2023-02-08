module Main where


import           AoC                 (applyInput)
import           Data.Array          (Array, (!), (//), assocs, elems, listArray)
import           Data.Bifunctor      (first, second)
import           Data.Char           (isDigit, toLower)
import           Data.Function       ((&))
import qualified Data.HashSet        as Set
import           Data.HashSet        (HashSet)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap, (!?))
import           Data.Maybe          (fromJust, isNothing)
import           Data.List           (find)
import           Text.Parsec         ((<|>)
                                     ,char
                                     ,choice
                                     ,lower
                                     ,many1
                                     ,newline
                                     ,sepEndBy1
                                     ,upper)
import           Text.Parsec.String  (Parser)


type Coord = (Int, Int)
data Tile = Empty | Wall | Key Char | Door Char deriving (Show, Eq)


up, down, left, right :: Coord -> Coord
up    = first (subtract 1)
down  = first (+1)
left  = second (subtract 1)
right = second (+1)


isKey :: Tile -> Bool
isKey (Key _) = True
isKey _       = False


fromKey :: Tile -> Char
fromKey (Key k) = k
fromKey _       = undefined

findMin :: HashMap Char (HashMap Char (Int, [Char])) -> [Char] -> HashSet Char -> Int
findMin m startKeys allKeys =
    fromJust $ search [(stKeySet, 0, stKeySet)] Nothing Map.empty
  where
    stKeySet = Set.fromList startKeys

    search paths candidate visited = case paths of
        [] -> candidate
        (key, cost, keys):ps
            | keys == allKeys && isNothing candidate ->
                search ps (Just cost) visited
            | keys == allKeys ->
                search ps (min cost <$> candidate) visited
            | Map.member (key, keys) visited && visited Map.! (key, keys) <= cost ->
                search ps candidate visited
            | lowerCost candidate cost ->
                search ps candidate visited
            | otherwise ->
                search (expand key cost keys ++ ps)
                       candidate
                       (addVisited key keys cost visited)

    lowerCost candidate cost = maybe False (<= cost) candidate

    expand k cost keys =
        Set.toList k
      & concatMap (\key -> expandOne key (Set.delete key k) cost keys)

    expandOne k kSet cost keys =
        m Map.! k
      & Map.filter (all (`Set.member` keys) . snd)
      & Map.toList
      & map (\(key, (pCost, _)) -> (Set.insert key kSet
                                   ,pCost + cost
                                   ,Set.insert key keys))

    addVisited key keys = Map.insertWith min (key, keys)


floodPaths :: Array Coord Tile -> (Coord, Char) -> (Char, HashMap Char (Int, [Char]))
floodPaths board (coord, key) = (key, flood initPaths Map.empty (Map.singleton coord 0))
  where
    flood paths m costs =
        case paths of
            []     -> m
            ((p, reqs):ps) ->
                let lenp = length p - 1
                in case board ! head p of
                Empty
                    | isNothing (costs !? head p) || costs Map.! head p > length p - 1 ->
                        flood (((, reqs) <$> openPath p) ++ ps)
                              m
                              (Map.insert (head p) lenp costs)
                    | otherwise -> flood ps m costs
                Wall   -> flood ps m costs
                Key k
                    | isDigit k -> flood (((, reqs) <$> openPath p) ++ ps)
                                         m
                                         (Map.insert (head p) lenp costs)
                    | k /= key  -> flood ps (Map.insert k (lenp, reqs) m) costs
                    | otherwise -> flood ps m costs
                Door d
                    | isNothing (costs !? head p) || costs Map.! head p > length p - 1 ->
                        flood (((, d:reqs) <$> openPath p) ++ ps)
                              m
                              (Map.insert (head p) lenp costs)
                    | otherwise -> flood ps m costs

    openPath []     = undefined
    openPath (c:cs) = (:c:cs).($ c)  <$> [up, down, left, right]

    initPaths       = (, []) <$> openPath [coord]


buildMap :: Array Coord Tile -> HashMap Char (HashMap Char (Int, [Char]))
buildMap board = Map.fromList $ floodPaths board <$> nodes
  where
    nodes = second fromKey <$> filter (isKey . snd) (assocs board)


solveP2 :: Array Coord Tile -> Int
solveP2 board = findMin m "0123" allKeys
  where
    stCoord               = fst $ fromJust $ find ((==Key '0') . snd) $ assocs board
    fourBoard             = board // changeEntrance stCoord
    allKeys               = Set.fromList $ map fromKey $ filter isKey $ elems fourBoard
    m                     = buildMap fourBoard
    changeEntrance (x, y) =
        [((x - 1, y - 1), Key '0'), ((x - 1, y), Wall), ((x - 1, y + 1), Key '1')
        ,((x    , y - 1), Wall)   , ((x    , y), Wall), ((x    , y + 1), Wall)
        ,((x + 1, y - 1), Key '2'), ((x + 1, y), Wall), ((x + 1, y + 1), Key '3')]



solveP1 :: Array Coord Tile -> Int
solveP1 board = findMin m "0" (Set.fromList $ map fromKey $ filter isKey $ elems board)
  where
    m = buildMap board


mapP :: Parser (Array Coord Tile)
mapP = do
    rows <- many1 tileP `sepEndBy1` newline
    let nrows = length rows
        ncols = length (head rows)
    return $ listArray ((0, 0), (nrows - 1, ncols - 1)) (concat rows)
  where
    tileP = choice [char '#' >> return Wall
                   ,char '.' >> return Empty
                   ,Key  <$> (lower <|> (char '@' >> return '0'))
                   ,Door . toLower <$> upper
                   ]


main :: IO ()
main = applyInput mapP solveP1 solveP2