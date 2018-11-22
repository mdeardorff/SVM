module Clustering where
--import Data.KMeans
import Data.Ord
import Data.Function
import Data.List (group, sort, nub, groupBy,sortBy)
import System.Random

data Vals = Numeric Double | Category Int deriving (Eq, Ord)
type Features = [Vals]

instance Show Vals where
  show (Numeric x) = "N: " ++ show x
  show (Category x) = "C: " ++ show x

mode :: (Ord a) => [a] -> [a]
mode xs = map fst $ filter ((==best).snd) counts
    where counts = map (\l -> (head l, length l)) . group . sort $ xs
          best = maximum (map snd counts)

mean :: Features -> Vals
mean vs = Numeric ((add 0 vs) / fromIntegral (length vs))
  where
    add acc [] = acc
    add acc ((Numeric x):xs) = add (acc + x) xs

kmeans k r = do
  d <- parseData
  g <- newStdGen
  let ranges = calcRanges d
      normalizeD = map (normalize ranges) d
      rs = take k . nub $ (randomRs (1,length d) g :: [Int]) --random indices for initial centroids
      cs = getCentroids rs normalizeD --initialize centroids
  results <- loop cs normalizeD r --begin iterating loop
  return (results)
  where
    loop cs d r = do
      let assignments = groupBy ((==) `on` snd) (sortBy (second) (map (assign cs r) d))
          newCentroids = map (calcPrototype . (map fst)) assignments
      if(newCentroids == cs)
        then do
          results <- evaluate newCentroids r
          performance results
        else do
          putStrLn "Calculating..."
          loop newCentroids d r

evaluate centroids r = do
  d <- parseDataFull
  let ranges = calcRanges (map fst d)
      normalizeD = map (\(x,i) -> (normalize ranges x, i)) (d)
      evals = map (eval centroids r) (normalizeD)
  return evals

performance results = do
  let correct = length $ filter ((==) True) results
      possible = length results
      percentage = fromIntegral correct / fromIntegral possible
  putStrLn ((show correct) ++ " out of " ++ (show possible))
  putStrLn ((show percentage) ++ "% accuracy")

eval :: [(Features)] -> Double -> (Features, Int) -> Bool
eval cents r (f,c) = if ((snd $ assign cents r f) == c)
  then True
  else False


  --assignments = groupBy ((==) `on` snd) (sortBy (second) (map (assign cs) d))
  --newCentroids = map (calcPrototype . (map fst)) assignments
  --newAssignments =  groupBy ((==) `on` snd) (sortBy (second) (map (assign newCentroids) (concat $ map (map (fst)) assignments)))

second a b = compare (snd a) (snd b)


--printLength :: [[(Features, Int)]]
printLength (a:as) = do
  (putStrLn (show (length a)))
  printLength as

--assign a feature vector based off of centroids
assign :: [Features] -> Double -> Features -> (Features,Int)
assign cs r v = helper [] v cs r
  where
    helper acc v [] r = (v,minIndex acc)
    helper acc v (c:cs) r = helper (acc ++ [(calcDistance r v c)]) v cs r

minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

--given the entire initial dataset, find min and max values for numeric features
calcRanges :: [Features] -> [(Double,Double)]
calcRanges x = helper [] (map (filter isNumeric) x)
  where
    helper acc [] = error $ "huh?"
    helper acc xs = case (head xs) of
      [] -> acc
      _  -> helper (acc ++ [strip (minimum (map head xs),maximum (map head xs))]) (map tail xs)
    strip (Numeric x, Numeric y) = (x,y)
    isNumeric (Numeric x) = True
    isNumeric _           = False

-- normalize a feature vector based on pre-calculated ranges
normalize :: [(Double,Double)] -> Features -> Features
normalize ranges x = helper [] x ranges
  where
    helper acc [] _ = acc
    helper acc ((Category x):xs) r = helper (acc ++ [Category x]) xs r
    helper acc ((Numeric x):xs) ((mi,ma):rs) = helper (acc ++ [Numeric ((x - mi) / (ma - mi))]) xs rs

--initial random selection for centroids.
getCentroids :: [Int] -> [Features] -> [Features]
getCentroids rs d = helper [] rs d
  where
    helper acc [] _ = acc
    helper acc (r:rs) d = helper (acc ++ [(d `at` r)]) rs d

--Indexing for nested lists
at :: [a] -> Int -> a
at v i = helper 0 v i
  where
    helper c (v:vs) i
      | c == i = v
      | c < i = helper (c+1) vs i
      | otherwise = error "Index too large"

--calculate distance between feature vectors given a constant for categorical significance
calcDistance :: Double -> Features -> Features -> Double
calcDistance r v1 v2 = helper 0.0 v1 v2 r
  where
    helper dist [] [] _ = dist
    helper dist ((Category v):vs) ((Category w):ws) r = helper (((diff v w) * r) + dist) vs ws r
    helper dist ((Numeric v):vs) ((Numeric w):ws) r = helper ((abs(v - w) ** 2) + dist) vs ws r


calcPrototype :: [Features] -> Features
calcPrototype vs = index [] vs
  where
    index acc [] = acc
    index acc vs = case (head vs) of
      [] -> acc
      ((Category x):xs)  -> index (acc ++ [(head $ mode $ map (head) vs)]) (map (tail) vs)--index (acc:(mode $ map (head) (vs))) (map (tail) vs)
      ((Numeric x):xs)   -> index (acc ++ [(mean $ map (head) vs)]) (map (tail) vs)
{-
  where
    index' acc [] = acc
    index' acc vs = undefined--index' (acc:(mode $ map (head) (vs))) (map (tail) vs)
-}
parseLine :: [String] -> Features
parseLine (age:wclass:fnlwgt:ed:ednum:mstat:occ:rel:race:sex:cgain:closs:hpw:nc:_) = [Numeric (read age), Category (catVal empK wclass), Numeric ((read fnlwgt)), Category (catVal eduK ed), Numeric ((read ednum)), Category (catVal mstatusK mstat), Category (catVal occK occ), Category (catVal raceK race), Category (catVal genderK sex), Numeric ((read cgain)), Numeric ((read closs)), Numeric ((read hpw)), Category (catVal countryK nc)]
parseLine _ = []

parseLineFull :: [String] -> (Features, Int)
parseLineFull (age:wclass:fnlwgt:ed:ednum:mstat:occ:rel:race:sex:cgain:closs:hpw:nc:label:_) = ([Numeric (read age), Category (catVal empK wclass), Numeric ((read fnlwgt)), Category (catVal eduK ed), Numeric ((read ednum)), Category (catVal mstatusK mstat), Category (catVal occK occ), Category (catVal raceK race), Category (catVal genderK sex), Numeric ((read cgain)), Numeric ((read closs)), Numeric ((read hpw)), Category (catVal countryK nc)], catVal labelK label)

parseData = do
  d <- readFile "adult.data"
  let l = lines d
      w = map (splitBy ',') l
      feat = map (parseLine) w
  return feat

parseDataFull = do
  d <- readFile "validation.test"
  let l = lines d
      w = map (splitBy ',') l
      feat = map (parseLineFull) w
  return feat

splitBy delimiter = foldr f [[]]
        where f c l@(x:xs) | c == delimiter = []:l
                         | otherwise = (c:x):xs

diff x y
 | x == y = 0.0
 | otherwise = 1.0

--get value for given category and key
catVal :: [String] -> String -> Int
catVal ind key = calc ind key 0
 where
    calc (x:xs) key count
     | x == key = count
     | otherwise = calc xs key (count+1)
    calc [] _ _ = -1

empK = [" Private", " Self-emp-not-inc", " Self-emp-inc", " Federal-gov", " Local-gov", " State-gov", " Without-pay", " Never-worked", " ?"]
eduK = [" Bachelors", " Some-college", " 11th", " HS-grad", " Prof-school", " Assoc-acdm", " Assoc-voc", " 9th", " 7th-8th", " 12th",  " Masters", " 1st-4th", " 10th", " Doctorate", " 5th-6th", " Preschool", " ?"]
mstatusK = [" Married-civ-spouse", " Divorced", " Never-married", " Separated", " Widowed", " Married-spouse-absent", " Married-AF-spouse", " ?"]
occK = [" Tech-support", " Craft-repair", " Other-service", " Sales", " Exec-managerial", " Prof-specialty", " Handlers-cleaners", " Machine-op-inspct", " Adm-clerical", " Farming-fishing", " Transport-moving", " Priv-house-serv", " Protective-serv", " Armed-Forces", " ?"]
relK = [" Wife", " Own-child", " Husband", " Not-in-family", " Other-relative", " Unmarried", " ?"]
raceK = [" White", " Asian-Pac-Islander", " Amer-Indian-Eskimo", " Other", " Black", " ?"]
genderK = [" Female", " Male", " ?"]
countryK = [" United-States", " Cambodia", " England", " Puerto-Rico", " Canada", " Germany", " Outlying-US(Guam-USVI-etc)", " India", " Japan", " Greece", " South", " China", " Cuba", " Iran", " Honduras", " Philippines", " Italy", " Poland", " Jamaica", " Vietnam", " Mexico", " Portugal", " Ireland", " France", " Dominican-Republic", " Laos", " Ecuador", " Taiwan", " Haiti", " Columbia", " Hungary", " Guatemala", " Nicaragua", " Scotland", " Thailand", " Yugoslavia", " El-Salvador", " Trinadad&Tobago", " Peru", " Hong", " Holand-Netherlands", " ?"]
labelK = [" <=50K", " >50K"]
