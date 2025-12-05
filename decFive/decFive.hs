import Data.List (foldl', group, nub, partition, sort, sortBy)
import Data.Set (fromList)
import Data.Text (pack, split, unpack)

main :: IO ()
main = do
  --   content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"
  let (idRanges, ids) = partition (elem '-') . filter (not . null) . lines $ content
  let (ranges, freshRanges) = unzip . parseRanges $ idRanges
  let idsToCheck = map read ids
  let freshCount = length . filter (\id -> any (\predicate -> predicate id) freshRanges) $ idsToCheck
  print freshCount

  let mergedRanges = merge . sortRanges $ ranges
  let freshIdCount = sum . map (\(min, max) -> max - min + 1) $ mergedRanges
  print freshIdCount

sortRanges :: [(Int, Int)] -> [(Int, Int)]
sortRanges = sortBy (\a b -> let minComp = compare (fst a) (fst b) in if minComp == EQ then compare (snd a) (snd b) else minComp)

merge :: [(Int, Int)] -> [(Int, Int)]
merge = sortRanges . doMerge
  where
    doMerge :: [(Int, Int)] -> [(Int, Int)]
    doMerge = foldl' (\acc r -> if null acc then [r] else mergeAcc acc r) []

    mergeAcc :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    mergeAcc acc r =
      let (rMin, rMax) = r
          (lastMin, lastMax) = head acc
          intersecting = lastMin <= rMin && rMin <= lastMax && rMin <= lastMax && lastMax <= rMax
          contained = lastMin <= rMin && rMin <= lastMax && lastMin <= rMax && rMax <= lastMax
       in if contained
            then acc
            else
              if intersecting
                then (min lastMin rMin, max lastMax rMax) : tail acc
                else r : acc

parseRanges :: [String] -> [((Int, Int), Int -> Bool)]
parseRanges [] = []
parseRanges (x : xs) =
  let [min, max] = map (read . unpack) . split (== '-') . pack $ x
   in ((min, max), \x -> min <= x && x <= max) : parseRanges xs