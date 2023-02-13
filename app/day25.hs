module Main where
import AoC (parseFromArg)
import IntCode (intCodeP, continue, runState, IntComputer, getOutput, emptyOutput, halted)
import Data.Char (ord, chr)


run :: IntComputer -> IO ()
run computer = do
    let prompt = map chr $ getOutput computer
    putStr prompt
    if halted computer then return ()
    else do
        input <- getLine
        run (continue (map ord (input++"\n")) (emptyOutput computer))


main :: IO ()
main = do
    parseRes <- parseFromArg intCodeP ()
    case parseRes of
        Left err ->
            putStrLn err
        Right program ->
            run (runState [] program)