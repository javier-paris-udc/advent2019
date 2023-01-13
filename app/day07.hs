module Main where


import AoC        (applyInput)
import Data.List  (foldl', mapAccumL, permutations, singleton)
import Data.Maybe (fromJust, isNothing)
import IntCode    (IntCode, continue, getOutput, halted, intCodeP, runState, runWithIO)


runFeedBack :: IntCode -> [Int] -> Int
runFeedBack code phases = repeatRounds machines 0
  where
    machines          = map ((`runState` code) . singleton) phases

    doRound           = mapAccumL applyMachine . Just

    applyMachine mv mach
        | halted mach  = (Nothing, mach)
        | isNothing mv = (Nothing, mach)
        | otherwise    =
            let v     = fromJust mv
                newSt = continue [v] mach
            in (Just $ head (getOutput newSt), newSt)

    repeatRounds ms v
        | halted (ms !! 4) = head $ getOutput (ms !! 4)
        | otherwise        =
            case doRound v ms of
                (Nothing, _)     -> error "Should be at end state"
                (Just nv, newMs) -> repeatRounds newMs nv


solveP2 :: IntCode -> Int
solveP2 code = maximum $ map (runFeedBack code) (permutations [5 .. 9])


solveP1 :: IntCode -> Int
solveP1 code = maximum $ map (foldl' runAmp 0) (permutations [0 .. 4])
  where
    runAmp amp phase = head $ snd $ runWithIO [phase, amp] code


main :: IO ()
main = applyInput intCodeP solveP1 solveP2