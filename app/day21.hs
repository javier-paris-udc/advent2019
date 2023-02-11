module Main where


import           AoC       (applyInputSWith)
import           IntCode   (IntCode, intCodeP, runWithIO)
import qualified Data.Char as Char


walkProg :: String
walkProg =
    unlines ["NOT A J"
            ,"NOT B T"
            ,"OR T J"
            ,"NOT C T"
            ,"OR T J"
            ,"AND D J"
            ,"WALK"
            ]


runProg :: String
runProg =
    unlines ["NOT A J"
            ,"NOT B T"
            ,"OR T J"
            ,"NOT C T"
            ,"OR T J"
            ,"AND D J"
            ,"NOT J T"
            ,"AND J T"
            ,"OR E T"
            ,"OR H T"
            ,"AND T J"
            ,"RUN"
            ]


solve :: String -> IntCode -> String
solve program = toString . snd . runWithIO (map Char.ord program)
  where
    toString l = let (msg, n) = span (<255) l
                 in map Char.chr msg ++ show n


main :: IO ()
main = applyInputSWith intCodeP () (solve walkProg) (solve runProg) putStrLn putStrLn