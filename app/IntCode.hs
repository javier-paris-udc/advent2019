{-# LANGUAGE RecordWildCards #-}
module IntCode (IntCode,
                intCodeP,
                run)
where


import           AoC                 (commaSepP, intP)
import           Control.Monad.State (State, execState, get, put)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as Map
import           Text.Parsec         (sepBy1)
import           Text.Parsec.String  (Parser)



type IntCode = HashMap Int Int
data St = St {mem :: IntCode,
              ip  :: Int}
          deriving (Show, Eq)


opCodes :: HashMap Int (State St ())
opCodes = Map.fromList [(1, add)
                       ,(2, prod)
                       ,(99, end)]


end :: State St ()
end = return ()


biop :: (Int -> Int -> Int) -> State St ()
biop op = do
    St { .. } <- get
    let par1 = mem ! (mem ! (ip + 1))
        par2 = mem ! (mem ! (ip + 2))
        dst  = mem ! (ip + 3)
    put St { mem = Map.insert dst (par1 `op` par2) mem, ip = ip + 4 }
    execIntCode


prod :: State St ()
prod = biop (*)


add :: State St ()
add = biop (+)


execIntCode :: State St ()
execIntCode = do
    St { .. } <- get
    let opCode = mem ! ip
    opCodes ! opCode


intCodeP :: Parser (HashMap Int Int)
intCodeP = Map.fromList . zip [0..] <$> intP `sepBy1` commaSepP


run :: IntCode -> IntCode
run code = mem $ execState execIntCode St { mem = code, ip = 0 }
