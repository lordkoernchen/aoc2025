main :: IO ()
main = do
  content <- readFile "realInput.txt"
  -- content <- readFile "testInput.txt"
  let instructionList = convertToInstructions $ lines content
  let initial = 50
  let decoded = decode initial instructionList
  let simplePassword = countElem 0 decoded
  print ("simplePassword: " ++ show simplePassword)
  let complexDecoded = complexDecode'' initial instructionList
  let complexPassword = countElem 0 complexDecoded
  print ("complexPassword: " ++ show complexPassword)

convertToInstructions :: [String] -> [Int]
convertToInstructions [] = []
convertToInstructions (x : xs) = convert x : convertToInstructions xs
  where
    convert :: [Char] -> Int
    convert ('R' : is) = read is
    convert ('L' : is) = -1 * read is
    convert anything = error ("cannot parse " ++ anything)

turnWheel :: Int -> Int -> Int
turnWheel acc x = (acc + x) `mod` 100

decode :: Int -> [Int] -> [Int]
decode = scanl turnWheel

countElem :: (Eq a) => a -> [a] -> Int
countElem elem = foldl (\acc x -> if elem == x then acc + 1 else acc) 0

countElem' :: (Eq a) => a -> [a] -> Int
countElem' elem xs = doCountElem 0 elem xs
  where
    doCountElem :: (Eq a) => Int -> a -> [a] -> Int
    doCountElem count elem [] = count
    doCountElem count elem (x : xs) = if elem == x then doCountElem (count + 1) elem xs else doCountElem count elem xs

complexDecode'' :: Int -> [Int] -> [Int]
complexDecode'' a [] = [a]
complexDecode'' a xs = complexDecode' a xs

-- not the most elegant solution but it works ;)
complexDecode' :: Int -> [Int] -> [Int]
complexDecode' initial [] = []
complexDecode' initial (x : xs) =
  let next = turnWheel initial x
      turned = simulateTurning initial x []
   in turned ++ complexDecode' (last turned) xs
  where
    simulateTurning :: Int -> Int -> [Int] -> [Int]
    simulateTurning current 0 visited = visited
    simulateTurning current x visited
      | x < 0 = simulateTurning ((current - 1) `mod` 100) (x + 1) (visited ++ [current])
      | x > 0 = simulateTurning ((current + 1) `mod` 100) (x - 1) (visited ++ [current])
    simulateTurning _ _ _ = error "unexhaustive case in simulateTurning"