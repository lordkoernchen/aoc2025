{-# LANGUAGE BangPatterns #-}
type Area = [[Char]]

main :: IO ()
main = do
--   content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"
--   print content
  let matrix = lines content
  let marked = markMovable 4 matrix
--   print . unlines $ marked
--   writeFile "markedOutput.txt" (unlines marked)
  let movableCount = countMovable marked
  print movableCount
  print (removeAllPossible matrix) 

-- returns the total number of removed scrolls after several iterations
removeAllPossible :: Area -> Int
removeAllPossible = countMovable . doMarkAndRemove
    where 
        doMarkAndRemove :: Area -> Area
        doMarkAndRemove !area = let nextMarked = markMovable 4 area
                                    markedCount = countMovable area
                                    nextMarkedCount = countMovable nextMarked
                                in if markedCount == nextMarkedCount then nextMarked else doMarkAndRemove nextMarked


markMovable :: Int -> Area -> Area
markMovable neighborLimit area = [[mark posX posY val | (posX, val) <- indexed row] | (posY, row) <- indexed area]
  where
    maxX = (length . head $ area) -1
    maxY = length area - 1

    mark posX posY val
      | val == empty = val
      | val == scroll =
        let numberOfNeighborScrolls = length . filter (\(x, y) -> ((area !! y) !! x) == scroll) $ neighborsIndices maxX maxY posX posY
         in if numberOfNeighborScrolls < neighborLimit then movableScroll else scroll
      | otherwise = val

neighborsIndices :: Int -> Int -> Int -> Int -> [(Int, Int)]
neighborsIndices maxX maxY posX posY = [(posX + x, posY + y) 
                                       | x <- indexModifiers
                                       , y <- indexModifiers
                                       , not (x == 0 && y == 0)
                                       , inLimits maxX (posX + x)
                                       , inLimits maxY (posY + y)
                                       ]

inLimits :: (Ord a, Num a) => a -> a -> Bool
inLimits max val = 0 <= val && val <= max

indexModifiers :: [Int]
indexModifiers = [-1, 0, 1]

indexed :: [b] -> [(Int, b)]
indexed = zip [0 ..]

countMovable :: Area -> Int
countMovable = length . filter (== movableScroll) . concat

movableScroll :: Char
movableScroll = 'x'

scroll :: Char
scroll = '@'

empty :: Char
empty = '.'
