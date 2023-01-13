module Main where
import AoC (applyInput)
import IntCode (intCodeP, IntCode, runWithIO)


solveP2 :: IntCode -> [Int]
solveP2 = snd . runWithIO [2]


solveP1 :: IntCode -> [Int]
solveP1 = snd . runWithIO [1]


main :: IO ()
main = applyInput intCodeP solveP1 solveP2