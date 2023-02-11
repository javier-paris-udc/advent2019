module Djistra (djistra) where

import           Data.Bifunctor (bimap)
import           Data.List      (foldl')
import           Data.Sequence  (Seq, ViewL ((:<)))
import qualified Data.Sequence  as Seq
import qualified Data.Set       as Set


binInsert :: (Ord b) => Seq ([a], b) -> ([a], b) -> Seq ([a], b)
binInsert prioList x@(_, cost) =
    Seq.insertAt pos x prioList
  where
    pos = findPos 0 (Seq.length prioList)
    findPos left right
        | left == right                        = left
        | snd (Seq.index prioList mid) == cost = mid
        | snd (Seq.index prioList mid) <  cost = findPos (mid + 1) right
        | otherwise                            = findPos left mid
      where
        mid = (left + right) `div` 2


djistra :: (Eq a, Ord a, Ord b, Num b)
        => a
        -> (a -> Bool)
        -> (a -> [(a, b)])
        -> Maybe ([a], b)
djistra start isEnd adj =
    runDjistra (Seq.singleton ([start], 0)) (Set.singleton start)
  where
    runDjistra next visited = case Seq.viewl next of
        (path@(pos : _), cost) :< _
            | isEnd pos -> Just (reverse path, cost)
            | otherwise -> let newVertex  = expandPath visited pos
                               newPaths   = bimap (:path) (+cost) <$> newVertex
                               newVisited = Set.insert pos visited
                               nextNoPos  = Seq.filter ((/= pos) . head . fst) next
                               newNext    = foldl' binInsert nextNoPos newPaths
                           in runDjistra newNext newVisited
        Seq.EmptyL      -> Nothing
        _               -> error "empty path"
    expandPath visited pos = filter ((`Set.notMember` visited) . fst) $ adj pos
