module Main where

import Text.Parsec.String (Parser)
import Text.Parsec        ((<|>)
                          ,char
                          ,digit
                          ,many1
                          ,option
                          ,parse
                          ,sepEndBy
                          ,space
                          ,spaces
                          ,string
                          ,try)
import System.Environment (getArgs)
import Data.List          (foldl')


deckSizeP1 :: Integer
deckSizeP1 = 10007


deckSizeP2 :: Integer
deckSizeP2 = 119315717514047


timesP2 :: Integer
timesP2 = 101741582076661


data Shuffle =
    Cut Integer
   |Increment Integer
   |Stack
    deriving (Show, Eq)


data ShuffleR =
    Forward Integer Integer
   |Back Integer Integer
   deriving (Show, Eq)


stack :: Integer -> Integer -> Integer
stack deckSize n = deckSize - n - 1


cut :: Integer -> Integer -> Integer-> Integer
cut deckSize c n
    | n >= absc = n - absc
    | otherwise = deckSize - absc + n
  where
    absc = if c >= 0 then c else deckSize + c


increment :: Integer -> Integer -> Integer -> Integer
increment deckSize i n = (i * n) `rem` deckSize


applyShuffle :: Integer -> Integer -> Shuffle -> Integer
applyShuffle deckSize n (Cut c)       = cut deckSize c n
applyShuffle deckSize n (Increment i) = increment deckSize i n
applyShuffle deckSize n Stack         = stack deckSize n


rev :: Integer -> ShuffleR -> ShuffleR
rev deckSize (Forward pos inc) = Back    (deckSize - pos - 1) inc
rev deckSize (Back    pos inc) = Forward (deckSize - pos - 1) inc


movePos :: Integer -> ShuffleR -> Integer -> ShuffleR
movePos deckSize shr n =
    case shr of
        (Forward pos inc) -> Forward (cut deckSize n pos) inc
        (Back pos inc)    -> Back (cut deckSize n pos) inc


changeInc :: Integer -> ShuffleR -> Integer -> ShuffleR
changeInc deckSize (Forward pos inc) n =
    Forward ((pos * n) `mod` deckSize) ((inc * n) `mod` deckSize)
changeInc deckSize (Back pos inc) n =
    Back ((pos * n) `mod` deckSize) ((inc * n) `mod` deckSize)


applyShuffleR :: Integer -> ShuffleR -> Shuffle -> ShuffleR
applyShuffleR deckSize shr sh =
    case sh of
        Cut c -> movePos deckSize shr c
        Increment i -> changeInc deckSize shr i
        Stack -> rev deckSize shr


inverse :: Integer -> Integer -> Integer
inverse a m = powmod a (m-2)
  where
    powmod _ 0 = 1
    powmod x n = if even n then powmod ((x*x) `mod` m) (n `div` 2)
                 else (x*powmod x (n-1)) `mod` m


mirror :: Integer -> Shuffle -> Shuffle
mirror deckSize sh =
    case sh of
        Cut c -> Cut (-c)
        Increment i -> Increment (inverse i deckSize)
        Stack -> Stack


collapse :: Integer -> [Shuffle] -> ShuffleR
collapse deckSize =
    foldl' (applyShuffleR deckSize) (Forward 0 1)


mixShuffleR :: Integer -> ShuffleR -> ShuffleR -> ShuffleR
mixShuffleR deckSize (Forward pos1 inc1) (Forward pos2 inc2) =
        Forward ((pos2+pos1*inc2) `mod` deckSize) ((inc1*inc2) `mod` deckSize)
mixShuffleR _ _ _ = error "mixShuffleR"


powShuffleR :: Integer -> ShuffleR -> Integer -> ShuffleR
powShuffleR _ shr 1 = shr
powShuffleR deckSize shr times
    | even times = powShuffleR deckSize (mixShuffleR deckSize shr shr) (times `div` 2)
    | otherwise  = mixShuffleR deckSize shr (powShuffleR deckSize shr (times - 1))


repeatShuffle :: Integer -> Integer -> ShuffleR -> Integer -> Integer
repeatShuffle deckSize times shr n =
    case powShr of
         Forward pos inc -> (pos + (n * inc)) `mod` deckSize
         Back    pos inc -> (pos - (n * inc)) `mod` deckSize
  where
    powShr = powShuffleR deckSize shr times


solveP2 :: Integer -> Integer -> [Shuffle] -> Integer -> Integer
solveP2 deckSize times shuffles =
    repeatShuffle deckSize times fShuffle
  where
    fShuffle = collapse deckSize (reverse (mirror deckSize <$> shuffles))


solveP1 :: Integer -> [Shuffle] -> Integer -> Integer
solveP1 deckSize shuffles n = foldl' (applyShuffle deckSize) n shuffles


-- Parser

integerP :: Parser Integer
integerP =
    do
        sign <- option 1 (char '-' >> return (-1))
        n    <- many1 digit
        return (sign * read n)


cutP :: Parser Shuffle
cutP = do
    string "cut" >> spaces
    Cut <$> integerP


incrementP :: Parser Shuffle
incrementP = do
    string "deal with increment" >> spaces
    Increment <$> integerP


stackP :: Parser Shuffle
stackP = string "deal into new stack" >> return Stack


shuffleP :: Parser Shuffle
shuffleP = do
    try cutP <|> try incrementP <|> try stackP


shuffleListP :: Parser [Shuffle]
shuffleListP = shuffleP `sepEndBy` space


main :: IO ()
main =
    do
        args <- getArgs
        case args of
            [inputFile] ->
                do
                    input <- readFile inputFile
                    case parse shuffleListP  "" input of
                        Left err -> print err
                        Right shuffles ->
                            do
                                print (solveP1 deckSizeP1 shuffles 2019)
                                print (solveP2 deckSizeP2 timesP2 shuffles 2020)
            _ -> putStrLn "Use: day22 input"
