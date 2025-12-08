{-# LANGUAGE BangPatterns #-}

import Data.Char (isNumber)
import Data.List (foldl', minimumBy, nub, nubBy, partition, sortBy, words)
import Data.Set (Set, fromList, insert, toList, unions)
import Data.Text (pack, replace, unpack)
import GHC.Float (int2Float)

type Point = (Int, Int, Int)

main :: IO ()
main = do
  -- content <- readFile "testInput.txt"
  -- let numberOfConnectionsToMake = 10
  content <- readFile "realInput.txt"
  let numberOfConnectionsToMake = 1000

  let !points = parse <$> lines content

  let !allDistances = concatMap (\(p, dists) -> [(p, d, dist) | (d, dist) <- dists]) $ findAllDistances points
  let !pointsWithShortestDistances = sortBy smallestDistance' allDistances
  let !uniquePointsWithShortestDistances = removeDuplicates pointsWithShortestDistances

  let !initialCircuits = (: []) <$> points
  let !circuitsByLength = sortBy (\a b -> length b `compare` length a) $ determineCircuitsAfterNSteps numberOfConnectionsToMake uniquePointsWithShortestDistances initialCircuits

  let !largestCircuits = take 3 circuitsByLength

  print "three largest circuits length multiplied"
  print $ foldl' (\acc g -> acc * length g) 1 largestCircuits

  -- second part: distance to wall (duplicates irrelevant)
  let ((x1, _, _), (x2, _, _)) = determineLastAddedConnection pointsWithShortestDistances (fromList <$> initialCircuits)
  print "distanceToWall"
  print (x1 * x2)

removeDuplicates :: [(Point, Point, Float)] -> [(Point, Point, Float)]
removeDuplicates = nubBy (\(a1, a2, _) (b1, b2, _) -> (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1))

determineLastAddedConnection :: [(Point, Point, Float)] -> [Set Point] -> (Point, Point)
determineLastAddedConnection ds cs = doDetermineLastAddedConnection ds cs ((0, 0, 0), (0, 0, 0))
  where
    doDetermineLastAddedConnection :: [(Point, Point, Float)] -> [Set Point] -> (Point, Point) -> (Point, Point)
    doDetermineLastAddedConnection [] [singleCircuit] !lastAdded = lastAdded
    doDetermineLastAddedConnection (dist : distances) !circuits !lastAdded = doDetermineLastAddedConnection distances newCircuits newLastAdded
      where
        !newLastAdded =
          if length circuits /= length newCircuits
            then (\(a, b, _) -> (a, b)) dist
            else lastAdded

        !newCircuits = collapseCircuits dist circuits

    doDetermineLastAddedConnection [] _ lastAdded = lastAdded

collapseCircuits :: (Point, Point, Float) -> [Set Point] -> [Set Point]
collapseCircuits (p1, p2, _) !circuits = newCircuit : otherCircuits
  where
    !(matchingCircuits, otherCircuits) = partition (\g -> p1 `elem` g || p2 `elem` g) circuits
    !newCircuit = insert p2 $ insert p1 $ unions matchingCircuits

determineCircuitsAfterNSteps :: Int -> [(Point, Point, Float)] -> [[Point]] -> [[Point]]
determineCircuitsAfterNSteps 0 _ circuits = circuits
determineCircuitsAfterNSteps _ [] circuits = circuits
determineCircuitsAfterNSteps c (dist : distances) circuits = determineCircuitsAfterNSteps (c -1) distances (toList <$>collapseCircuits dist (fromList <$> circuits))

findAllDistances :: [Point] -> [(Point, [(Point, Float)])]
findAllDistances ps = [(p, sortBy (\a b -> snd a `compare` snd b) $ (\p' -> (p', distance p p')) <$> [p' | p' <- ps, p /= p']) | p <- ps]

findShortestDistances :: [Point] -> [(Point, Point, Float)]
findShortestDistances ps = (\(p1, (p2, d)) -> (p1, p2, d)) <$> sortBy (\(_, (_, d1)) (_, (_, d2)) -> d1 `compare` d2) allDistances
  where
    allDistances = [(p, minimumBy smallestDistance ((\p' -> (p', distance p p')) <$> [p' | p' <- ps, p /= p'])) | p <- ps]

smallestDistance :: (Point, Float) -> (Point, Float) -> Ordering
smallestDistance (_, d1) (_, d2) = d1 `compare` d2

smallestDistance' :: (Point, Point, Float) -> (Point, Point, Float) -> Ordering
smallestDistance' (_, _, d1) (_, _, d2) = d1 `compare` d2

distance :: Point -> Point -> Float
distance (x1, y1, z1) (x2, y2, z2) = sqrt . int2Float $ sqX + sqY + sqZ
  where
    sqX = (x1 - x2) * (x1 - x2)
    sqY = (y1 - y2) * (y1 - y2)
    sqZ = (z1 - z2) * (z1 - z2)

parse :: String -> Point
parse str = (read x, read y, read z)
  where
    [x, y, z] = Data.List.words $ unpack $ replace (pack ",") (pack " ") (pack str)