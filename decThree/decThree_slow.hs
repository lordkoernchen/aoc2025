{-# LANGUAGE BangPatterns #-}
import Data.List (tails, subsequences, foldl')

main :: IO ()
main = do
  -- content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"
  let banks = lines content
  let bankParts = map tails banks
  let simple =  [ foldl' max 0 [ let first = head vals in foldl' max 0 [ read [first, second] | second <- tail vals] | vals <- single, length vals >= 2]| single <- bankParts]
  print (sum simple)
  print (sum $ subsequences12 banks)

subsequences12 :: (Ord b, Num b, Read b) => [[Char]] -> [b]
subsequences12 = map subsequence12

subsequence12 :: (Ord c, Num c, Read c) => [Char] -> c
subsequence12 bank = foldl' max 0 . map read . filter (\xs -> length xs == 12) $ subs
  where
    subs = subsequences bank
