module Main where


import           Text.Parsec.String (Parser)
import           Text.Parsec        (newline, sepEndBy1, string)
import           AoC                (applyInput, intP)
import           Data.List          (foldl')
import           Data.Tuple.Extra   (fst3, snd3, thd3)
import qualified Data.HashSet       as Set


type Coord = (Int, Int, Int)
type Speed = (Int, Int, Int)
data Body  = Body { pos :: Coord, vel :: Speed } deriving (Show, Eq)


mkBody :: Coord -> Body
mkBody c = Body { pos = c, vel = (0, 0, 0) }


triliftA2 :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
triliftA2 f (x1, x2, x3) (y1, y2, y3) = (f x1 y1, f x2 y2, f x3 y3)


trimap :: (a -> b) -> (a, a, a) -> (b, b, b)
trimap f (x, y, z) = (f x, f y, f z)


move :: [Body] -> [Body]
move = map (\b -> b { pos = triliftA2 (+) (pos b) (vel b)})


changeVel :: [Body] -> [Body]
changeVel bodys = map changeBodyVel bodys
  where
    changeBodyVel b = foldl' (\b1 b2 -> b1 { vel = accel b1 (pos b2) }) b bodys
    accel b1 c      = triliftA2 (+) (vel b1) $ trimap signum $ triliftA2 (-) c (pos b1)


energy :: [Body] -> Int
energy = sum . map energyOne
  where
    energyOne b    = sum3 (pos b) * sum3 (vel b)
    sum3 (x, y, z) = abs x + abs y + abs z


applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n == 0    = x
    | otherwise = applyN (n - 1) f (f x)


findCycle :: [Int] -> Int
findCycle = repeatTillCycle 0 Set.empty . map (, 0)
  where
    repeatTillCycle n visited bodys
        | bodys `Set.member` visited = n
        | otherwise =
            let newBodys = map (moveOne . accelOne bodys) bodys
            in repeatTillCycle (n + 1) (Set.insert bodys visited) newBodys
    accelOne bodys (p, v) = let gt = length $ filter (< p) (map fst bodys)
                                lt = length $ filter (> p) (map fst bodys)
                            in (p, v + lt - gt)
    moveOne (p, v) = (p + v, v)


solveP2 :: [Coord] -> Int
solveP2 positions = foldl1 lcm [xCycle, yCycle, zCycle]
  where
    xCycle = findCycle $ map fst3 positions
    yCycle = findCycle $ map snd3 positions
    zCycle = findCycle $ map thd3 positions


solveP1 :: [Coord] -> Int
solveP1 = energy . applyN 1000 (move . changeVel) . map mkBody


bodysP :: Parser [Coord]
bodysP = bodyP `sepEndBy1` newline
  where
    bodyP = do
        _ <- string "<x="
        x <- intP
        _ <- string ", y="
        y <- intP
        _ <- string ", z="
        z <- intP
        _ <- string ">"
        return (x, y, z)


main :: IO ()
main = applyInput bodysP solveP1 solveP2