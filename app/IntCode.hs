{-# LANGUAGE RecordWildCards #-}
module IntCode (IntCode,
                intCodeP,
                run,
                runWithIO)
where


import           AoC                 (commaSepP, intP)
import           Control.Monad.State (State, execState, get, put)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import           Data.List           (unfoldr)
import           Text.Parsec         (sepBy1)
import           Text.Parsec.String  (Parser)



type IntCode = HashMap Int Int
type Mode    = State St Int
type Params  = [Mode]
data St = St {mem    :: IntCode
             ,ip     :: Int
             ,input  :: [Int]
             ,output :: [Int]}
          deriving (Show, Eq)


opCodes :: HashMap Int (Params -> State St ())
opCodes = Map.fromList [(1,  add)
                       ,(2,  prod)
                       ,(3,  doInput)
                       ,(4,  doOutput)
                       ,(5,  jmpTrue)
                       ,(6,  jmpFalse)
                       ,(7,  less)
                       ,(8,  equals)
                       ,(99, end)]


modeMap :: HashMap Int (Int -> Mode)
modeMap = Map.fromList [(0, positionMode)
                       ,(1, immediateMode)]


immediateMode :: Int -> State St Int
immediateMode pos = do
    St { .. } <- get
    return $ mem ! (ip + pos)


positionMode :: Int -> State St Int
positionMode pos = do
    St { .. } <- get
    return $ mem ! (mem ! (ip + pos))


end :: Params -> State St ()
end = const $ return ()


equals :: Params -> State St ()
equals params = do
    St { .. } <- get
    val1      <- params !! 0
    val2      <- params !! 1
    let dst = mem ! (ip + 3)
    if val1 == val2 then put St { mem = Map.insert dst 1 mem, ip = ip + 4 , .. }
                    else put St { mem = Map.insert dst 0 mem, ip = ip + 4 , .. }
    execIntCode


less :: Params -> State St ()
less params = do
    St { .. } <- get
    val1      <- params !! 0
    val2      <- params !! 1
    let dst = mem ! (ip + 3)
    if val1 < val2 then put St { mem = Map.insert dst 1 mem, ip = ip + 4 , .. }
                   else put St { mem = Map.insert dst 0 mem, ip = ip + 4 , .. }
    execIntCode



jmpFalse :: Params -> State St ()
jmpFalse params = do
    St { .. } <- get
    bool      <- params !! 0
    dstIp     <- params !! 1
    if bool == 0 then put St { ip = dstIp, .. }
                 else put St { ip = ip + 3, .. }
    execIntCode


jmpTrue :: Params -> State St ()
jmpTrue params = do
    St { .. } <- get
    bool      <- params !! 0
    dstIp     <- params !! 1
    if bool /= 0 then put St { ip = dstIp, .. }
                 else put St { ip = ip + 3, .. }
    execIntCode


biop :: (Int -> Int -> Int) -> Params -> State St ()
biop op params = do
    St { .. } <- get
    par1      <- params !! 0
    par2      <- params !! 1
    let dst = mem ! (ip + 3)
    put St { mem = Map.insert dst (par1 `op` par2) mem, ip = ip + 4, .. }
    execIntCode


prod :: Params -> State St ()
prod = biop (*)


add :: Params -> State St ()
add = biop (+)


doInput :: Params -> State St ()
doInput _ = do
    St { .. } <- get
    let dst = mem ! (ip + 1)
    case input of
        []        -> error "inifinite list is empty, go figure"
        (i0 : is) -> put St { mem = Map.insert dst i0 mem, input = is, ip = ip + 2, .. }
    execIntCode


doOutput :: Params -> State St ()
doOutput params = do
    St { .. } <- get
    val       <- params !! 0
    put St { output = val : output, ip = ip + 2, ..}
    execIntCode


parseModes :: Int -> Params
parseModes modeCodes =
    let funs = unfoldr (\n -> Just (modeMap ! (n `mod` 10), n `div` 10)) modeCodes
    in zipWith (\f param -> f param) funs [1 ..]


execIntCode :: State St ()
execIntCode = do
    St { .. } <- get
    let control = mem ! ip
        opCode  = control `mod` 100
        modes   = parseModes (control `div` 100)
    opCodes ! opCode $ modes


runWithIO :: [Int] -> IntCode -> (IntCode, [Int])
runWithIO inputL code = (mem finalState, reverse $ output finalState)
  where
    state0     = St {mem    = code
                    ,ip     = 0
                    ,input  = inputL
                    ,output = []}
    finalState = execState execIntCode state0


run :: IntCode -> IntCode
run code = mem $ execState execIntCode St { mem = code, ip = 0, input = [], output = [] }


intCodeP :: Parser (HashMap Int Int)
intCodeP = Map.fromList . zip [0..] <$> intP `sepBy1` commaSepP