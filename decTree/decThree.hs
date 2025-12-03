import Data.List (tails)

main :: IO ()
main = do
  let content = lines $ readFile "testInput.txt"
  let bla::[[String]] = map tails unsorted
  let numbers::[Int] =  [ foldl max 0 [ let first = head vals in foldl max 0 [ read [first, second] | second <- tail vals] | vals <- single, length vals >= 2]| single <- bla]
  putStrLn $ show $ sum numbers
