{-# LANGUAGE RecordWildCards #-}
module Main where


import           AoC                 (applyInput, commaSepP, intP)
import           Data.HashMap.Strict (HashMap, (!))
import           Text.Parsec.String  (Parser)
import           Text.Parsec         (sepBy1)
import qualified Data.HashMap.Strict as Map
import           Control.Monad.State (State, execState, get, put)
import           Data.List           (find)
import           Control.Applicative (liftA2)


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


solveP2 :: IntCode -> Int
solveP2 i0 =
    case find ((== 19690720) . (! 0) . mem . execState execIntCode) states of
        Nothing -> undefined
        Just s  -> 100 * mem s ! 1 + mem s ! 2
  where
    i0Alarm noun verb = Map.insert 1 noun (Map.insert 2 verb i0)
    state0 m = St { mem = m, ip = 0 }
    states   = map state0 (liftA2 i0Alarm [0 .. 99] [0 .. 99])


solveP1 :: IntCode -> Int
solveP1 i0 = (! 0) $ mem $ execState execIntCode state0
  where
    i0Alarm = Map.insert 1 12 (Map.insert 2 2 i0)
    state0 = St { mem = i0Alarm, ip = 0 }


intCodeP :: Parser (HashMap Int Int)
intCodeP = Map.fromList . zip [0..] <$> intP `sepBy1` commaSepP


main :: IO ()
main = applyInput intCodeP solveP1 solveP2