type Point = (Int, Char)

type Row = [Point]

type Grid = [Row]

main :: IO ()
main = do
--   content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"

  -- these: total number of splits is count of all splitters minus the numbers of splitters that aren't hit by any beam
  let traced = traceBeam $ lines content
  let totalSplits = countActiveSplitters traced
  print totalSplits

  -- determine total number of timelines (paths that are created)
  let allTimelineEnds = determineAllTimelines $ lines content
  print $ "total timelines: " ++ (show . length $ allTimelineEnds)

determineAllTimelines :: [String] -> [Point]
determineAllTimelines area = traceTimelines [initialBeamPosition] otherRows
  where
    -- add coordinates
    areaWithIndices :: Grid
    areaWithIndices = zip [0 ..] <$> area

    firstRow :: Row
    firstRow = head areaWithIndices

    initialBeamPosition = head . filter (\(_, x) -> x == start) $ firstRow

    otherRows :: Grid
    otherRows = tail areaWithIndices

    traceTimelines :: [Point] -> [Row] -> [Point]
    traceTimelines [] _ = []
    traceTimelines currentTimelines [] = currentTimelines
    traceTimelines currentTimelines (currentRow : rows) =
      let nextTimelines = concatMap (`determineNextTimelines` currentRow) currentTimelines
       in traceTimelines nextTimelines rows

    determineNextTimelines :: Point -> Row -> [Point]
    determineNextTimelines beamPos@(idx, val) row
      | snd (row !! idx) == splitter = [(idx - 1, val), (idx + 1, val)]
      | otherwise = [beamPos]

traceBeam :: [String] -> [String]
traceBeam area = removeCoords traced
  where
    -- add coordinates
    areaWithIndices :: Grid
    areaWithIndices = zip [0 ..] <$> area

    firstRow :: Row
    firstRow = head areaWithIndices

    otherRows :: Grid
    otherRows = tail areaWithIndices

    traced :: Grid
    traced = doTrace firstRow otherRows [firstRow]

    doTrace :: Row -> Grid -> Grid -> Grid
    doTrace _ [] traced = traced
    doTrace prev (current : xs) traced = doTrace transformedX xs (traced ++ [transformedX])
      where
        aligned :: [(Point, Point)]
        aligned = zip prev current
        transformedX :: Row
        transformedX = transform prev current <$> aligned

transform :: Row -> Row -> (Point, Point) -> Point
transform prev current ((prevId, prevVal), cur@(curId, curVal))
  | prevVal == start = (curId, beam) -- start trace
  | prevVal == beam && curVal == empty = (curId, beam) -- passthrough
  | curId - 1 >= 0
      && snd (current !! (curId - 1)) == splitter
      && snd (prev !! (curId - 1)) == beam =
    (curId, beam) -- splitter to the left hit by beam
  | curId + 1 < length current
      && snd (current !! (curId + 1)) == splitter
      && snd (prev !! (curId + 1)) == beam =
    (curId, beam) -- splitter to the right hit by beam
  | otherwise = cur

removeCoords :: Grid -> [String]
removeCoords = map (snd <$>)

start :: Char
start = 'S'

beam :: Char
beam = '|'

splitter :: Char
splitter = '^'

empty :: Char
empty = '.'

countActiveSplitters :: [String] -> Int
countActiveSplitters [] = 0
countActiveSplitters [x1, x2] = countHits x1 x2
countActiveSplitters (x1 : x2 : xs) = countHits x1 x2 + countActiveSplitters (x2 : xs)
countActiveSplitters any = error $ "unexpected row constellation" ++ show any

countHits :: String -> String -> Int
countHits above current = length . filter (\(a, c) -> a == beam && c == splitter) $ zip above current