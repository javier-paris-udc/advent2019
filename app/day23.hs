module Main where


import           AoC                 (applyInput)
import           IntCode             (IntCode
                                     ,IntComputer
                                     ,continue
                                     ,emptyOutput
                                     ,getOutput
                                     ,intCodeP
                                     ,runState)
import           Data.Function       ((&))
import           Data.List           (singleton, unfoldr)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict ((!?))
import           Data.Maybe          (fromJust, fromMaybe)
import           Control.Applicative (liftA2)


groupsOf :: (Eq a, Num a) => Int -> [a] -> [[a]]
groupsOf n = unfoldr (\l -> if null l then Nothing else Just (splitAt n l))


network :: [[Int]] -> [IntComputer] -> (Maybe ([Int], [[Int]]), [IntComputer])
network input computers
    | Map.null packets && all (==[-1]) input = (Nothing, nextComputers)
    | otherwise =
        case packets !? 255 of
            Nothing -> network nextInput nextComputers
            Just p  -> (Just (drop (length p - 2) p, nextInput), nextComputers)
  where
    packets = map getOutput nextComputers
            & filter (not . null)
            & concatMap (groupsOf 3)
            & map (\l -> (head l, tail l))
            & Map.fromListWith (flip (++))

    nextComputers = zipWith (flip continue) (map emptyOutput computers) input

    nextInput = map (\i -> fromMaybe [-1] (packets !? i)) [0..49]


withNat :: Maybe [Int] -> Maybe Int -> [[Int]] -> [IntComputer] -> Int
withNat nat prevY input comps =
    case network input comps of
        (Just (newNat, newInput), newComps)      ->
                withNat (Just newNat) prevY newInput newComps
        (Nothing, newComps)
            | maybeEqual (fmap (!! 1) nat) prevY -> fromJust prevY
            | otherwise                          ->
                case nat of
                    Nothing -> error "no value in nat"
                    Just v  -> withNat nat (Just (v !! 1)) (v:replicate 49 [-1]) newComps
  where
    maybeEqual a b = fromMaybe False (liftA2 (==) a b)


solveP2 :: IntCode -> Int
solveP2 code = withNat Nothing
                       Nothing
                       (singleton <$> [0..49])
                       (replicate 50 (runState [] code))


solveP1 :: IntCode -> Int
solveP1 code = network (singleton <$> [0..49]) (replicate 50 (runState [] code))
             & fst
             & fromJust
             & fst
             & (!! 1)


main :: IO ()
main = applyInput intCodeP solveP1 solveP2