{-# LANGUAGE RecordWildCards #-}
module IntCode (continue
               ,getOutput
               ,halted
               ,IntCode
               ,intCodeP
               ,IntComputer
               ,run
               ,runWithIO
               ,runState
               ,MachineSt (..))
where


import           AoC                 (commaSepP, intP)
import           Control.Monad.State (State, execState, get, modify, put)
import           Data.HashMap.Strict (HashMap, (!), (!?))
import qualified Data.HashMap.Strict as Map
import           Data.List           (unfoldr)
import           Data.Maybe          (fromMaybe)
import           Text.Parsec         (sepBy1)
import           Text.Parsec.String  (Parser)



type IntCode     = HashMap Int Int
type Mode        = State IntComputer Int
type Params      = [Mode]
data MachineSt   = Halt | InputWait | Running deriving (Show, Eq)
data IntComputer = St {mem     :: IntCode
                      ,ip      :: Int
                      ,input   :: [Int]
                      ,output  :: [Int]
                      ,state   :: MachineSt
                      ,relBase :: Int }
                   deriving (Show, Eq)


getOutput :: IntComputer -> [Int]
getOutput = output


halted :: IntComputer -> Bool
halted = (==Halt) . state


opCodes :: HashMap Int (Params -> State IntComputer ())
opCodes = Map.fromList [(1,  add)
                       ,(2,  prod)
                       ,(3,  doInput)
                       ,(4,  doOutput)
                       ,(5,  jmpTrue)
                       ,(6,  jmpFalse)
                       ,(7,  less)
                       ,(8,  equals)
                       ,(9,  relBaseOffset)
                       ,(99, end)]


modeMap :: HashMap Int (Int -> Mode)
modeMap = Map.fromList [(0, positionMode)
                       ,(1, immediateMode)
                       ,(2, relativeMode)]



-- Memory access

readMem :: Int -> State IntComputer Int
readMem pos = do
    St { .. } <- get
    return $ fromMaybe 0 (mem !? pos)


relativeMode :: Int -> State IntComputer Int
relativeMode pos = do
    St { .. } <- get
    (+relBase) <$> readMem (ip + pos)


immediateMode :: Int -> State IntComputer Int
immediateMode pos = do
    St { .. } <- get
    return $ ip + pos


positionMode :: Int -> State IntComputer Int
positionMode pos = do
    St { .. } <- get
    readMem (ip + pos)


readParam :: Params -> Int -> State IntComputer Int
readParam params n = do
    addr      <- params !! n
    readMem addr



-- Misc

end :: Params -> State IntComputer ()
end = const $ modify (\st -> st { state = Halt })


relBaseOffset :: Params -> State IntComputer ()
relBaseOffset params = do
    St { .. } <- get
    val       <- readParam params 0
    put $ St { relBase = relBase + val , ip = ip + 2, .. }
    execIntCode



-- Jumps

jmp :: (Int -> Bool) -> Params -> State IntComputer ()
jmp test params = do
    St { .. } <- get
    bool      <- readParam params 0
    dstIp     <- readParam params 1
    if test bool then put St { ip = dstIp, .. }
                 else put St { ip = ip + 3, .. }
    execIntCode


jmpFalse :: Params -> State IntComputer ()
jmpFalse = jmp (==0)


jmpTrue :: Params -> State IntComputer ()
jmpTrue = jmp (/=0)



-- Binary Operations

biop :: (Int -> Int -> Int) -> Params -> State IntComputer ()
biop op params = do
    St { .. } <- get
    par1      <- readParam params 0
    par2      <- readParam params 1
    dst       <- params !! 2
    put St { mem = Map.insert dst (par1 `op` par2) mem, ip = ip + 4, .. }
    execIntCode


prod :: Params -> State IntComputer ()
prod = biop (*)


add :: Params -> State IntComputer ()
add = biop (+)


equals :: Params -> State IntComputer ()
equals = biop (\x y -> if x == y then 1 else 0)


less :: Params -> State IntComputer ()
less = biop (\x y -> if x < y then 1 else 0)


-- I/O

doInput :: Params -> State IntComputer ()
doInput params = do
    St { .. } <- get
    dst       <- params !! 0
    case input of
        []        -> put St { state = InputWait, .. }
        (i0 : is) -> do
            put St { mem = Map.insert dst i0 mem, input = is, ip = ip + 2, .. }
            execIntCode


doOutput :: Params -> State IntComputer ()
doOutput params = do
    St { .. } <- get
    val       <- readParam params 0
    put St { output = val : output, ip = ip + 2, ..}
    execIntCode



-- Base Code

parseModes :: Int -> Params
parseModes modeCodes =
    let funs = unfoldr (\n -> Just (modeMap ! (n `mod` 10), n `div` 10)) modeCodes
    in zipWith (\f param -> f param) funs [1 ..]


execIntCode :: State IntComputer ()
execIntCode = do
    St { .. } <- get
    control   <- readMem ip
    let opCode  = control `mod` 100
        modes   = parseModes (control `div` 100)
    opCodes ! opCode $ modes


runState :: [Int] -> IntCode -> IntComputer
runState inputL code = execState execIntCode state0
  where
    state0 = St {mem     = code
                ,ip      = 0
                ,input   = inputL
                ,output  = []
                ,state   = Running
                ,relBase = 0}


continue :: [Int] -> IntComputer -> IntComputer
continue inp st = execState execIntCode st { input = inp }


runWithIO :: [Int] -> IntCode -> (IntCode, [Int])
runWithIO inputL code = (mem finalState, reverse $ output finalState)
  where
    state0     = St {mem     = code
                    ,ip      = 0
                    ,input   = inputL
                    ,output  = []
                    ,state   = Running
                    ,relBase = 0}
    finalState = execState execIntCode state0


run :: IntCode -> IntCode
run code = fst $ runWithIO [] code


intCodeP :: Parser (HashMap Int Int)
intCodeP = Map.fromList . zip [0..] <$> intP `sepBy1` commaSepP