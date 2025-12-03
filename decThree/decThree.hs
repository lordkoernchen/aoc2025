import Data.List (tails, foldl')

main :: IO ()
main = do
  -- content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"
  let banks = lines content
  let bankParts = map tails banks
  let simple =  [ foldl max 0 [ let first = head vals in foldl max 0 [ read [first, second] | second <- tail vals] | vals <- single, length vals >= 2]| single <- bankParts]
  print (sum simple)
  let scanned = map scanBatteries banks
  print scanned
  print (sum . map read $ scanned)


scanBatteries :: [Char] -> String
scanBatteries bank = foldl' findHighest initial rest 
  where
    splitted = splitAt 12 bank
    initial = fst splitted
    rest = snd splitted

findHighest :: String -> Char -> String
findHighest acc new = foldl' maxBy acc [ remove acc i ++ [new] | i <- [0..11] ]

options :: String -> Char -> [String]
options acc new = [ remove acc i ++ [new] | i <- [0..11] ]

maxBy :: String -> String -> String
maxBy a b = let parsedA = toInt a
                parsedB = toInt b
            in if parsedA >= parsedB then a else b

toInt :: String -> Int
toInt = read

remove :: String -> Int -> String
remove acc i = take i acc ++ drop (i + 1) acc

