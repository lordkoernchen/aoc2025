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
markMovable neighborLimit area = marked
  where
    maxX = (length . head $ area) -1
    maxY = length area - 1
    positionsX = [0 .. maxX]
    positionsY = [0 .. maxY]

    allPositions = [(x, y) | x <- positionsX, y <- positionsY]

    indices = [-1, 0, 1]
    neighborsIndices posX posY = [(posX + x, posY + y) | x <- indices, y <- indices, not (x == 0 && y == 0), (posX + x, posY + y) `elem` allPositions]

    addIndex = zip [0 ..]
    areaWithIndices = addIndex . map addIndex $ area

    marked = [[mark posX posY val | (posX, val) <- row] | (posY, row) <- areaWithIndices]

    mark posX posY val
      | val == empty = val
      | val == scroll =
        let numberOfNeighborScrolls = length . filter (\(x, y) -> ((area !! y) !! x) == scroll) $ neighborsIndices posX posY
         in if numberOfNeighborScrolls < neighborLimit then movableScroll else scroll
      | otherwise = val

countMovable :: Area -> Int
countMovable = length . filter (== movableScroll) . concat

movableScroll :: Char
movableScroll = 'x'

scroll :: Char
scroll = '@'

empty :: Char
empty = '.'
