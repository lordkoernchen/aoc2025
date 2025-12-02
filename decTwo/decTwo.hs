{-# LANGUAGE BangPatterns #-}
import Data.Text (split, pack, unpack)
import qualified Data.Text as Data
main :: IO ()
main = do
--   content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"
  let !ranges = parseRanges $ pack content
  let sumInvalidIds = sum . filter invalidId' . concatMap createAllIdsInRange $ ranges
  print sumInvalidIds

parseRanges :: Data.Text -> [(Int, Int)]
parseRanges ranges = map doRead $ split (== ',') ranges
  where doRead !interval = let parts = map unpack $ split (== '-') interval
                          in (read $ head parts, read $ last parts)

createAllIdsInRange :: (Int, Int) -> [Int]
createAllIdsInRange (lower, upper) = [lower..upper]

invalidId :: Int -> Bool
invalidId !val   | totalLength == 0 = False
                | even . length $ stringified = let firstHalf = take (totalLength `div` 2) stringified
                                                    replicated = firstHalf ++ firstHalf
                                                in stringified == replicated
                | otherwise = False
    where
      stringified = show val
      totalLength = length stringified

invalidId' :: Int -> Bool
invalidId' !val  | totalLength == 0 = False
                | otherwise = doDetailedCheck val

    where
      !stringified = show val
      !totalLength = length stringified

      !potentialLengths = [1..totalLength]

      replicator l = let part = take l stringified
                     in filter (\s -> length s == totalLength) [concat (replicate x part) | x <- [2..totalLength]]
      doDetailedCheck x = elem stringified $ concatMap replicator potentialLengths