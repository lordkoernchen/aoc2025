import Data.List (tails, subsequences)

main :: IO ()
main = do
  content <- readFile "testInput.txt"
  -- content <- readFile "realInput.txt"
  let banks = lines content
  let bankParts = map tails banks
  let simple =  [ foldl max 0 [ let first = head vals in foldl max 0 [ read [first, second] | second <- tail vals] | vals <- single, length vals >= 2]| single <- bankParts]
  print (sum simple)
  let subsequences12 = map (foldl max 0 . map read . filter (\xs -> length xs == 12) . subsequences ) banks
  print subsequences12
  print (sum subsequences12)
  let scanned = map scanBatteries banks
  print scanned
  print (sum scanned)


scanBatteries :: String -> Int
scanBatteries bank = read $ foldl' findHighest initial rest 
  where
    splitted = splitAt 12 bank
    initial = fst splitted
    rest = snd splitted

    findHighest :: String -> Char -> String
    findHighest acc new = foldl' (max . read) acc [ remove acc i ++ [new] | i <- [0..11] ]
    
    remove :: String -> Int -> String
    remove acc i = take i acc ++ drop (i + 1) acc

