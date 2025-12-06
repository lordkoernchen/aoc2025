import Data.Char (isNumber, isSpace)
import Data.List (foldl', groupBy, transpose, uncons)

main = do
  content <- readFile "realInput.txt"
  --   content <- readFile "testInput.txt"
  let contentLines = lines content
  -- simple solution (part 1)
  let tasks = transpose . map words $ contentLines
  print . sum . map (solve . uncons . reverse) $ tasks
  -- complex solution (part 2)
  let numOfNumberLines = length . filter (any isNumber) $ contentLines
  let (nums, ops) = splitAt numOfNumberLines contentLines
  let numsPerTask = removeBlanks . groupNumsInTask . transpose $ nums
  print . sum . zipWith (curry (solve . return)) (words . head $ ops) $ numsPerTask

groupNumsInTask :: [String] -> [[String]]
groupNumsInTask = groupBy (\a b -> (notNull . words $ a) && (notNull . words $ b))

removeBlanks :: [[String]] -> [[String]]
removeBlanks = filter notNull . map (filter notNull . map (filter (not . isSpace)))

notNull :: [a] -> Bool
notNull = not . null

solve :: Maybe (String, [String]) -> Int
solve Nothing = 0
solve (Just (op, nums)) = foldl' (f op) (neutral op) $ map read nums
  where
    neutral ('+' : _) = 0
    neutral ('*' : _) = 1
    neutral op = error ("unknown op" ++ show op)

    f ('+' : _) = (+)
    f ('*' : _) = (*)
    f op = error ("unknown op" ++ show op)