module Main where


import AoC                (applyInput)
import Text.Parsec        (alphaNum, char, many1, sepEndBy1, spaces)
import Text.Parsec.String (Parser)
import Data.Maybe         (fromJust)
import Control.Monad      (mplus)


data Tree a = Node a [Tree a] deriving (Show, Eq)


buildTree :: [(String, String)] -> Tree String
buildTree orbitList = buildFrom "COM"
  where
    buildFrom body =
        let orbitingBody = filter ((==body) . fst) orbitList
        in Node body (map (buildFrom . snd) orbitingBody)


orbits :: Eq a => Tree a -> Int
orbits = orbitsFrom 0
  where
    orbitsFrom n tree =
        case tree of
            Node _ []       -> n
            Node _ children -> n + sum (map (orbitsFrom (n + 1)) children)


path :: Eq a => a -> Tree a -> [a]
path dst = fromJust . findPath
  where
    findPath (Node r children)
        | r == dst  = Just [r]
        | otherwise = (r:) <$> foldr (mplus . findPath) Nothing children


solveP2 :: [(String, String)] -> Int
solveP2 orbitList = length pathYou + length pathSan - (2 * common) - 4
  where
    tree    = buildTree orbitList
    pathYou = path "YOU" tree
    pathSan = path "SAN" tree
    common  = length (takeWhile id $ zipWith (==) pathYou pathSan) - 1


solveP1 :: [(String, String)] -> Int
solveP1 = orbits . buildTree


orbitsP :: Parser [(String, String)]
orbitsP = orbitP `sepEndBy1` spaces
  where
    orbitP = do
        center  <- many1 alphaNum
        _       <- char ')'
        orbiter <- many1 alphaNum
        return (center, orbiter)


main :: IO ()
main = applyInput orbitsP solveP1 solveP2