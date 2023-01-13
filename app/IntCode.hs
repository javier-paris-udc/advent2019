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
import           Control.Monad.State (State, execState, get, put, modify)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import           Data.List           (unfoldr)
import           Text.Parsec         (sepBy1)
import           Text.Parsec.String  (Parser)



type IntCode     = HashMap Int Int
type Mode        = State IntComputer Int
type Params      = [Mode]
data MachineSt   = Halt | InputWait | Running deriving (Show, Eq)
data IntComputer = St {mem    :: IntCode
                      ,ip     :: Int
                      ,input  :: [Int]
                      ,output :: [Int]
                      ,state  :: MachineSt }
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
                       ,(99, end)]


modeMap :: HashMap Int (Int -> Mode)
modeMap = Map.fromList [(0, positionMode)
                       ,(1, immediateMode)]


immediateMode :: Int -> State IntComputer Int
immediateMode pos = do
    St { .. } <- get
    return $ mem ! (ip + pos)


positionMode :: Int -> State IntComputer Int
positionMode pos = do
    St { .. } <- get
    return $ mem ! (mem ! (ip + pos))


end :: Params -> State IntComputer ()
end = const $ modify (\st -> st { state = Halt })


equals :: Params -> State IntComputer ()
equals params = do
    St { .. } <- get
    val1      <- params !! 0
    val2      <- params !! 1
    let dst = mem ! (ip + 3)
    if val1 == val2 then put St { mem = Map.insert dst 1 mem, ip = ip + 4 , .. }
                    else put St { mem = Map.insert dst 0 mem, ip = ip + 4 , .. }
    execIntCode


less :: Params -> State IntComputer ()
less params = do
    St { .. } <- get
    val1      <- params !! 0
    val2      <- params !! 1
    let dst = mem ! (ip + 3)
    if val1 < val2 then put St { mem = Map.insert dst 1 mem, ip = ip + 4 , .. }
                   else put St { mem = Map.insert dst 0 mem, ip = ip + 4 , .. }
    execIntCode



jmpFalse :: Params -> State IntComputer ()
jmpFalse params = do
    St { .. } <- get
    bool      <- params !! 0
    dstIp     <- params !! 1
    if bool == 0 then put St { ip = dstIp, .. }
                 else put St { ip = ip + 3, .. }
    execIntCode


jmpTrue :: Params -> State IntComputer ()
jmpTrue params = do
    St { .. } <- get
    bool      <- params !! 0
    dstIp     <- params !! 1
    if bool /= 0 then put St { ip = dstIp, .. }
                 else put St { ip = ip + 3, .. }
    execIntCode


biop :: (Int -> Int -> Int) -> Params -> State IntComputer ()
biop op params = do
    St { .. } <- get
    par1      <- params !! 0
    par2      <- params !! 1
    let dst = mem ! (ip + 3)
    put St { mem = Map.insert dst (par1 `op` par2) mem, ip = ip + 4, .. }
    execIntCode


prod :: Params -> State IntComputer ()
prod = biop (*)


add :: Params -> State IntComputer ()
add = biop (+)


doInput :: Params -> State IntComputer ()
doInput _ = do
    St { .. } <- get
    let dst = mem ! (ip + 1)
    case input of
        []        -> put St { state = InputWait, .. }
        (i0 : is) -> do
            put St { mem = Map.insert dst i0 mem, input = is, ip = ip + 2, .. }
            execIntCode


doOutput :: Params -> State IntComputer ()
doOutput params = do
    St { .. } <- get
    val       <- params !! 0
    put St { output = val : output, ip = ip + 2, ..}
    execIntCode


parseModes :: Int -> Params
parseModes modeCodes =
    let funs = unfoldr (\n -> Just (modeMap ! (n `mod` 10), n `div` 10)) modeCodes
    in zipWith (\f param -> f param) funs [1 ..]


execIntCode :: State IntComputer ()
execIntCode = do
    St { .. } <- get
    let control = mem ! ip
        opCode  = control `mod` 100
        modes   = parseModes (control `div` 100)
    opCodes ! opCode $ modes


runState :: [Int] -> IntCode -> IntComputer
runState inputL code = execState execIntCode state0
  where
    state0 = St {mem    = code
                ,ip     = 0
                ,input  = inputL
                ,output = []
                ,state  = Running}


continue :: [Int] -> IntComputer -> IntComputer
continue inp st = execState execIntCode st { input = inp }


runWithIO :: [Int] -> IntCode -> (IntCode, [Int])
runWithIO inputL code = (mem finalState, reverse $ output finalState)
  where
    state0     = St {mem    = code
                    ,ip     = 0
                    ,input  = inputL
                    ,output = []
                    ,state  = Running}
    finalState = execState execIntCode state0


run :: IntCode -> IntCode
run code = mem $ execState execIntCode state0
  where
    state0 = St { mem = code, ip = 0, input = [], output = [], state = Running }


intCodeP :: Parser (HashMap Int Int)
intCodeP = Map.fromList . zip [0..] <$> intP `sepBy1` commaSepP