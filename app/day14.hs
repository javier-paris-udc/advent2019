module Main where


import           AoC                 (applyInput, blanksP, commaSepP, intP)
import           Data.Bifunctor      (second)
import           Data.Function       ((&))
import           Data.HashMap.Strict (HashMap, (!), (!?))
import qualified Data.HashMap.Strict as Map
import           Data.List           (foldl')
import           Data.Maybe          (fromMaybe)
import           Text.Parsec         (letter, many1, newline, sepBy1, sepEndBy1, string)
import           Text.Parsec.String  (Parser)


type Resource    = String
type ReactionMap = HashMap Resource (Int, [(Resource, Int)])

useAvail :: HashMap Resource Int -> Resource -> Int -> (Int, HashMap Resource Int)
useAvail avail res amount =
    case avail !? res of
        Nothing -> (amount, avail)
        Just av
            | av > amount  -> (0, Map.insert res (av - amount) avail)
            | av == amount -> (0, Map.delete res avail)
            | otherwise    -> (amount - av, Map.delete res avail)


findNeeds :: HashMap Resource Int
          -> HashMap Resource Int
          -> ReactionMap
          -> (Int, HashMap Resource Int)
findNeeds avail needs resMap =
    case Map.toList $ Map.delete "ORE" needs of
        []              -> (needs ! "ORE", avail)
        (res, amount):_ ->
            let (remAmount, remAvail) = useAvail avail res amount
                (newAvail, newNeeds)  = fabricate res remAmount remAvail
            in findNeeds newAvail newNeeds resMap
  where
    fabricate res remAmount remAvail
        | remAmount == 0 = (remAvail, Map.delete res needs)
        | otherwise      =
            let (produced, using) = resMap ! res
                need              = remAmount `div` produced
                                  + signum (remAmount `mod` produced)
                leftOver          = (need * produced) - remAmount
                newNeeds          = map (second (* need)) using
            in if leftOver > 0 then
                (Map.insert res leftOver remAvail, Map.delete res $ insertAll newNeeds)
               else
                (remAvail, Map.delete res $ insertAll newNeeds)

    insertAll = foldl' (\n (r, amount) -> Map.insertWith (+) r amount n) needs


findOver :: HashMap Resource Int -> ReactionMap -> (Int, HashMap Resource Int)
findOver resources resMap =
    case reclaimable of
        []              -> (fromMaybe 0 (resources !? "ORE"), Map.delete "ORE" resources)
        (res, amount):_ -> let (prod, needs)    = resMap ! res
                               reclaimedN       = amount `div` prod
                               leftOver         = amount `mod` prod
                               reclaimedRes     = map (second (*reclaimedN)) needs
                               updatedResources = Map.insert res leftOver resources
                           in findOver (addAll reclaimedRes updatedResources) resMap

  where
    reclaimable = resources
                & Map.delete "ORE"
                & Map.toList
                & filter (\(res, amount) -> fst (resMap ! res) <= amount)


addAll :: [(Resource, Int)] -> HashMap Resource Int -> HashMap Resource Int
addAll reclaimed res = foldl' (\m (k, v) -> Map.insertWith (+) k v m) res reclaimed


solveP2 :: ReactionMap -> Int
solveP2 resMap = iterateFuel 1_000_000_000_000 Map.empty
  where
    iterateFuel remOre resources =
        let fuel         = remOre `div` oreOne
            fuelLast     = (fst (findOver (Map.unionWith (+) resources remOne) resMap)
                           + remOre) `div` oreOne
            newRemOre    = remOre `mod` oreOne
            extraRes     = Map.map (* fuel) remOne
            (reclaimedOre, remRes) = findOver (Map.unionWith (+) extraRes resources)
                                              resMap
        in if fuel == 0 then fuelLast
           else fuel + iterateFuel (newRemOre + reclaimedOre) remRes

    (oreOne, remOne) = findNeeds Map.empty (Map.singleton "FUEL" 1) resMap



solveP1 :: ReactionMap -> Int
solveP1 = fst . findNeeds Map.empty (Map.singleton "FUEL" 1)


reactionsP :: Parser ReactionMap
reactionsP = do
    Map.fromList <$> reactionP `sepEndBy1` newline
  where
    reactionP = do
        reqs               <- resourceP `sepBy1` commaSepP
        _                  <- string " => "
        (resource, amount) <- resourceP
        return (resource, (amount, reqs))
    resourceP = do
        amount <- intP
        _      <- blanksP
        name   <- many1 letter
        return (name, amount)



main :: IO ()
main = applyInput reactionsP solveP1 solveP2