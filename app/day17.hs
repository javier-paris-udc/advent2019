module Main where


import           AoC                 (applyInput)
import           IntCode             (IntCode
                                     ,continue
                                     ,emptyOutput
                                     ,getOutput
                                     ,intCodeP
                                     ,runState
                                     ,runWithIO)
import qualified Data.Char           as Char
import           Data.HashSet        (HashSet)
import           Data.List           (findIndex
                                     ,findIndices
                                     ,inits
                                     ,isPrefixOf
                                     ,intercalate
                                     ,intersperse
                                     ,sortOn
                                     ,tails
                                     ,unfoldr)
import           Data.Function       ((&))
import qualified Data.HashSet        as Set
import           Data.Bifunctor      (first, second)
import           Data.Maybe          (catMaybes, fromJust, isJust)
import           Control.Arrow       ((>>>))
import           Control.Monad       (mplus)
import           Data.Ord            (Down(..))
import qualified Data.HashMap.Strict as Map


type Coord   = (Int, Int)
type Board   = HashSet Coord
data Dir     = N | S | E | W deriving (Show, Eq)
data Command = R | L | F Int deriving (Eq)


instance Show Command where
    show :: Command -> String
    show L     = "L"
    show R     = "R"
    show (F n) = show n


north, south, east, west :: Coord -> Coord
north = second (subtract 1)
south = second (+1)
east  = first  (+1)
west  = first  (subtract 1)


move :: Dir -> Coord -> Coord
move d = case d of
    N -> north
    S -> south
    E -> east
    W -> west


left :: Dir -> Dir
left d = case d of
    N -> W
    W -> S
    S -> E
    E -> N


right :: Dir -> Dir
right = left . left . left


charToMaybeDir :: Char -> Maybe Dir
charToMaybeDir c = case c of
    '^' -> Just N
    'v' -> Just S
    '<' -> Just W
    '>' -> Just E
    _   -> Nothing


nextTo :: Coord -> [Coord]
nextTo c = map ($ c) [north, south, east, west]


commLen :: [Command] -> Int
commLen cmds = length cmds - 1 + sum (map cmdLen cmds)
  where
    cmdLen cmd = case cmd of
        L   -> 1
        R   -> 1
        F n -> digits n
    digits n
        | n < 10    = 1
        | otherwise = 1 + digits (n `div` 10)


divideBy :: Eq a => [a] -> [a] -> [[a]]
divideBy a b
    | null b           = []
    | a `isPrefixOf` b = divideBy a (drop (length a) b)
    | otherwise        = let segment = nonPrefix a b
                         in segment : divideBy a (drop (length segment) b)
  where
    nonPrefix x y
        | null y           = []
        | x `isPrefixOf` y = []
        | otherwise        = head y : nonPrefix x (tail y)


findSequences :: Int -> [[Command]] -> Maybe [[Command]]
findSequences n commands
    | n == 0 && null commands = Just []
    | null commands           = Nothing
    | otherwise               = foldr (mplus . tryCandidate) Nothing candidates
  where
    candidates     = head commands
                   & inits
                   & takeWhile ((<=20) . commLen)
                   & sortOn (Down . points)
    points c       = commLen c * length (filter (isPrefixOf c) (tails (head commands)))
    tryCandidate c = (c :) <$> findSequences (n - 1) (concatMap (divideBy c) commands)


findPath :: Coord -> Dir -> Board -> [Command]
findPath pos dir board
    | leftCoord  `Set.member` board = L : findForward (left dir)
    | rightCoord `Set.member` board = R : findForward (right dir)
    | otherwise                     = []
  where
    leftCoord       = move (left  dir) pos
    rightCoord      = move (right dir) pos
    findForward d =
        let line = takeWhile (`Set.member` board) $ iterate (move d) pos
        in F (length line - 1) : findPath (last line) d board


buildBoard :: String -> Board
buildBoard = lines
         >>> map (findIndices (`elem` "#<>^v"))
         >>> zipWith (\n row -> map (,n) row) [0 ..]
         >>> concat
         >>> Set.fromList


findRobot :: String -> (Coord, Dir)
findRobot = lines
        >>> map (map charToMaybeDir)
        >>> map (\l -> ((,) <*> (fromJust . (l !!))) <$> findIndex isJust l)
        >>> zipWith (\i -> fmap (first (,i))) [0 ..]
        >>> catMaybes
        >>> head


pathToCmds :: [Command] -> [[Command]] -> String
pathToCmds path commands = unfoldr decomposePath path
  where
    decomposePath p
        | null p    = Nothing
        | otherwise = do
            i <- findIndex (`isPrefixOf` p) commands
            return (Char.chr (Char.ord 'A' + i), drop (length (commands !! i)) p)


solveP2 :: IntCode -> Int
solveP2 code = last $ getOutput $ continue (map Char.ord inputStr) (emptyOutput machine0)
  where
    machine0   = runState [] (Map.insert 0 2 code)
    asciiBoard = map Char.chr $ getOutput machine0
    board      = buildBoard asciiBoard
    (pos, dir) = findRobot asciiBoard
    path       = findPath pos dir board
    commands   = findSequences 3 [path]
    cmdSeq     = pathToCmds path (fromJust commands)
    inputStr   = intersperse ',' cmdSeq ++ "\n"
              ++ concatMap ((++"\n") . intercalate "," . map show) (fromJust commands)
              ++ "n\n"


solveP1 :: IntCode -> Int
solveP1 code = Set.filter (all (`Set.member` board) . nextTo) board
             & Set.toList
             & map (uncurry (*))
             & sum
  where
    board = buildBoard $ map Char.chr $ snd $ runWithIO [] code


main :: IO ()
main = applyInput intCodeP solveP1 solveP2