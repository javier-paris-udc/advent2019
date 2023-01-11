module Main where


import           AoC                 (applyInput)
import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as Map
import           IntCode             (IntCode, intCodeP, run)
import           Data.List           (find)
import           Control.Applicative (liftA2)


solveP2 :: IntCode -> Int
solveP2 i0 =
    case find ((== 19690720) . (! 0) . run) mems of
        Nothing -> undefined
        Just m  -> 100 * m ! 1 + m ! 2
  where
    i0Alarm noun verb = Map.insert 1 noun (Map.insert 2 verb i0)
    mems              = liftA2 i0Alarm [0 .. 99] [0 .. 99]


solveP1 :: IntCode -> Int
solveP1 i0 = (! 0) $ run mem0
  where
    mem0 = Map.insert 1 12 $ Map.insert 2 2 i0


main :: IO ()
main = applyInput intCodeP solveP1 solveP2