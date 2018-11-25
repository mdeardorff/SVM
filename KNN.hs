module KNN where

import Clustering
import Data.List(sortBy, group, groupBy)

main = knn 10 4.0

knn k r = do
  d <- readData
  testD <- readTest
  let ranges = calcRanges (map fst d)
      testRanges = calcRanges (map fst testD)
      normalizeD = map (\(x,i) -> (normalize ranges x, i)) d
      normalizeTest = map (\(x,i) -> (normalize testRanges x, i)) testD
      neighbors = map (\x -> ((getKNeighbors r k x normalizeD), snd x)) normalizeTest --map (\(x,i) -> (getKNeighbors r k x normalizeD,i)) (normalizeTest)
      assignments = map (\x -> (vote (fst x), snd x)) neighbors
  return assignments --grade assignments

grade :: [(Int,Int)] -> IO ()
grade list = do
  let correct = length $ filter ((==) True) (map (\x -> (fst x == snd x)) list)
      possible = length list
      percentage = fromIntegral correct / fromIntegral possible
  putStrLn ((show correct) ++ " out of " ++ (show possible))
  putStrLn ((show percentage) ++ "% accuracy")
  --HMMMMMMMMMMMMMMM


--makeAssignments ::

getKNeighbors :: Double -> Int -> (Features, Int) -> [(Features,Int)] -> [(Features,Int)]
getKNeighbors r k f d = helper [] r k f d (map (calcDistance r (fst f)) (map fst d)) --take k (sortBy (distanceFrom f r) d)-- helper [] r k f d
  where
    helper acc _ 0 _ _ mDistance = acc
    helper acc r k f d mDistance = do
      let mindex = (minIndex (mDistance))
      helper ((d `at` mindex):acc) r (k-1) f (deleteN mindex d) (deleteN mindex mDistance)
--

--helper :: [] -> Double -> Int -> (Features, Int) -> [(Features, Int)] -> [(Features,Int)]
helper acc _ 0 _ _ _ = acc
helper acc r k f d mDistance = undefined --do
  --let mindex = (minIndex (map (calcDistance r (fst f)) (map fst d)))
  --helper ((d `at` mindex):acc) r (k-1) f (deleteN mindex d)

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

distanceFrom :: (Features, Int) -> Double -> (Features, Int) -> (Features, Int) -> Ordering --sort the distance of two feature vectors from a base vector and category weight
distanceFrom f r v1 v2  = compare (calcDistance r (fst f) (fst v1)) (calcDistance r (fst f) (fst v2))

--vote :: [(Features, Int)] -> Int
vote f = maxIndex $ map length (groupBy (\a b -> snd a == snd b) (sortBy (second) f))

readData = do
  d <- readFile "adult.data"
  let l = lines d
      w = map (splitBy ',') l
      feat = map (parseLineFull) w
  return feat

readTest = do
  d <- readFile "validation.test"
  let l = lines d
      w = map (splitBy ',') l
      feat = map (parseLineFull) w
  return feat
