{-# LANGUAGE BangPatterns #-}

import qualified Data.Bifunctor
import Data.List (maximumBy, partition, sortBy)
import Data.Text (pack, split, unpack)
import Text.XHtml (border)
import GHC.Float

type Point = (Int, Int)

type RectAngle = (Point, Point)

main :: IO ()
main = do
  -- content <- readFile "testInput.txt"
  content <- readFile "realInput.txt"

  let points = parse <$> lines content
  let rectAngles = [ alignVector (p1, p2) | p1 <- points, p2 <- points, p1 /= p2, fst p1 /= fst p2 || snd p1 /= snd p2]

  print "largest rectAngle"
  let !rectAnglesBySize = sortBy (\r1 r2 -> area r2 `compare` area r1) rectAngles
  print . (\r -> (r, area r)) $ head rectAnglesBySize


  -- the rest doesn't produces the correct result. (too small). Larger valid rectangles get somehow filtered
  let !polygonBorders = sortBy (\v1 v2 -> lenVec v2 `compare` lenVec v1) $ alignVector <$> borderVectors points

  let !(horizontalBorders, verticalBorders) = partition isHorizontal polygonBorders

  let minPoint = (0, 0)
  let maxPoint = (100000, 100000)

  let rectAnglesWithoutInnerPoints = filter (not . containsOtherPoints points) rectAnglesBySize

  let rectWithValidOuterPoints = filter (allPointsInside polygonBorders) rectAnglesWithoutInnerPoints

  let validRectAngles = filter (not . intersectedBy polygonBorders) rectWithValidOuterPoints

  print "largest fully contained rectAngle"
  print . (\r -> (r, area r)) $ maximumBy (\r1 r2 -> area r1 `compare` area r2) validRectAngles

containsOtherPoints :: [Point] -> RectAngle -> Bool
containsOtherPoints ps (p1@(x1,y1), p2@(x2,y2)) = any (\p@(x,y) -> (p /= p1 && p /= p2) && x1 < x && x < x2 && y1 < y && y < y2) ps

intersectedBy :: [(Point, Point)] -> RectAngle -> Bool
intersectedBy bs r = let rectBorders = alignVector <$> borderVectors (corners r)
                      in any (\(_, hits) -> (not . null) hits) $ untouchingIntersectionsPerRectBorder rectBorders bs

untouchingIntersectionsPerRectBorder :: [(Point, Point)] -> [(Point, Point)] -> [((Point, Point), [(Point, Point)])]
untouchingIntersectionsPerRectBorder !bs !rs = [(r, [b | b <- bs, not (touching b r), existsCommonPoint b r]) | r <- rs]

allPointsInside :: [(Point, Point)] -> RectAngle -> Bool
allPointsInside bs r = all validCorner allCorners
  where
    !allCorners = corners r
    validCorner !c = onBorder bs c || insideBorder c

    minPoint = (0, 0)
    maxPoint = (100000, 100000)

    insideBorder :: Point -> Bool
    insideBorder p =
      let rs = rays p minPoint maxPoint
          intersects = (\(_, is) -> length is) <$> intersections bs rs
       in all (\l -> l > 0 && odd l) intersects

onBorder :: [(Point, Point)] -> Point -> Bool
onBorder bs c = any (contains c) bs

contains :: Point -> (Point, Point) -> Bool
contains (x, y) ((x1, y1), (x2, y2)) = x1 <= x && x <= x2 && y1 <= y && y <= y2

touching :: (Point, Point) -> (Point, Point) -> Bool
touching (p1, p2) (p3, p4) = p1 == p3 || p1 == p4 || p2 == p3 || p2 == p4

isHorizontal :: (Point, Point) -> Bool
isHorizontal ((x1, y1), (x2, y2)) = x1 == x2

rays :: Point -> Point -> Point -> [(Point, Point)]
rays inQuestion@(x, y) (minX, minY) (maxX, maxY) = [alignVector (inQuestion, (x', y')) | x' <- [minX, x, maxX], y' <- [minY, y, maxY], x' == x || y' == y, (x', y') /= inQuestion]

intersections :: [(Point, Point)] -> [(Point, Point)] -> [((Point, Point), [(Point, Point)])]
intersections !bs !rs = [(r, [b | b <- bs, existsCommonPoint b r]) | r <- rs]

existsCommonPoint :: (Point, Point) -> (Point, Point) -> Bool
existsCommonPoint ((x1, y1), (x1', y1')) ((x2, y2), (x2', y2')) = coordsInIntervals
  where
    !isHorizontal_1 = x1 == x1'
    !isHorizontal_2 = x2 == x2'

    !coordsInIntervals =
      (not isHorizontal_1 && not isHorizontal_2 && y1 == y2 && (inInterval x1 x2 x1' || inInterval x1 x2' x1' || inInterval x2 x1 x2' || inInterval x2 x1' x2'))
        || (isHorizontal_1 && isHorizontal_2 && x1 == x2 && (inInterval y1 y2 y1' || inInterval y1 y2' y1' || inInterval y2 y1 y2' || inInterval y2 y1' y2'))
        || (isHorizontal_1 && (inInterval x2 x1 x2' || inInterval x2' x1 x2) && (inInterval y1 y2 y1' || inInterval y1' y2 y1))
        || (isHorizontal_2 && (inInterval x1 x2 x1' || inInterval x1' x2 x1) && (inInterval y2 y1 y2' || inInterval y2' y1 y2))

inInterval :: Int -> Int -> Int -> Bool
inInterval !i1 !a !i2 = i1 <= a && a <= i2

corners :: RectAngle -> [Point]
corners ((x1, y1), (x2, y2)) = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]

area :: RectAngle -> Int
area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

parse :: String -> Point
parse !str = (x, y)
  where
    [x, y] = map (read . unpack) . split (== ',') . pack $ str

borderVectors :: [Point] -> [(Point, Point)]
borderVectors [] = []
borderVectors !points = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) $ (last points, head points) : internalBorderVectors points
  where
    internalBorderVectors [] = []
    internalBorderVectors [_] = error "insufficient point length"
    internalBorderVectors [p1, p2] = [(p1, p2)]
    internalBorderVectors (p1 : p2 : ps) = (p1, p2) : internalBorderVectors (p2 : ps)

alignVector :: (Point, Point) -> (Point, Point)
alignVector v@(p1@(x1, y1), p2@(x2, y2))
  | x1 == x2 && y1 <= y2 = v
  | x1 == x2 && y1 > y2 = (p2, p1)
  | y1 == y2 && x1 <= x2 = v
  | y1 == y2 && x1 > x2 = (p2, p1)
  | otherwise = v

lenVec :: ((Int, Int), (Int, Int)) -> Float
lenVec ((x1,y1), (x2,y2)) = sqrt (int2Float a + int2Float b)
  where
    a = (x1 - x2)*(x1 - x2)
    b = (y1 - y2)*(y1 - y2)