import Data.List (tails, subsequences)

main :: IO ()
main = do
  -- content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"
  let banks = lines content
  let bankParts = map tails banks
  let simple =  [ foldl max 0 [ let first = head vals in foldl max 0 [ read [first, second] | second <- tail vals] | vals <- single, length vals >= 2]| single <- bankParts]
  print (sum simple)
  let subsequences12 = map (foldl max 0 . map read . filter (\xs -> length xs == 12) . subsequences ) banks
  print (sum subsequences12)