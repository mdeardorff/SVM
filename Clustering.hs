module Clustering where
--import Data.KMeans
import Data.Ord
import Data.Function
import Data.List (group, sort, nub, groupBy,sortBy)
import System.Random

dataSet :: [[Double]]
dataSet = [[2,2],[2,1],[0,0],[1,0],[0,1],[1,2]]


--run = kmeans 2 dataSet

data Vals = Numeric Int | Category Int deriving (Eq, Ord)
type Features = [Vals]

instance Show Vals where
  show (Numeric x) = "N: " ++ show x
  show (Category x) = "C: " ++ show x

mode :: (Ord a) => [a] -> [a]
mode xs = map fst $ filter ((==best).snd) counts
    where counts = map (\l -> (head l, length l)) . group . sort $ xs
          best = maximum (map snd counts)

mean :: Features -> Vals
mean vs = Numeric ((add 0 vs) `div` length vs)
  where
    add acc [] = acc
    add acc ((Numeric x):xs) = add (acc + x) xs

diff x y
  | x == y = 0.0
  | otherwise = 1.0

kmeans k = do
  d <- parseData
  g <- newStdGen
  let rs = take k . nub $ (randomRs (1,length d) g :: [Int]) --random indices for initial centroids
      cs = getCentroids rs d
      assignments = groupBy ((==) `on` snd) (sortBy (second) (map (assign cs) d))
      newCentroids = map (calcMode . (map fst)) assignments
      newAssignments =  groupBy ((==) `on` snd) (sortBy (second) (map (assign newCentroids) (concat $ map (map (fst)) assignments)))
  return (newAssignments)

second a b = compare (snd a) (snd b)

loop cs d = do
  let assignments = groupBy ((==) `on` snd) (sortBy (second) (map (assign cs) d))
      newCentroids = map (calcMode . (map fst)) assignments
  if(newCentroids == cs)
    then return newCentroids
    else loop newCentroids d

--printLength :: [[(Features, Int)]]
printLength (a:as) = do
  (putStrLn (show (length a)))
  printLength as

--assign a feature vector based off of centroids
assign :: [Features] -> Features -> (Features,Int)
assign cs v = helper [] v cs
  where
    helper acc v [] = (v,minIndex acc)
    helper acc v (c:cs) = helper (acc ++ [(calcDistance v c 2.0)]) v cs

minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]

getCentroids :: [Int] -> [Features] -> [Features]
getCentroids rs d = helper [] rs d
  where
    helper acc [] _ = acc
    helper acc (r:rs) d = helper (acc ++ [(d `at` r)]) rs d

--Indexing for nested lists
at :: [Features] -> Int -> Features
at v i = helper 0 v i
  where
    helper c (v:vs) i
      | c == i = v
      | c < i = helper (c+1) vs i
      | otherwise = error "Index too large"


calcDistance :: Features -> Features -> Double -> Double
calcDistance v1 v2 r = helper 0.0 v1 v2 r
  where
    helper dist [] [] _ = dist
    helper dist ((Category v):vs) ((Category w):ws) r = helper (((diff v w) * r) + dist) vs ws r
    helper dist ((Numeric v):vs) ((Numeric w):ws) r = helper ((abs(fromIntegral v - fromIntegral w) ** 2) + dist) vs ws r

calcMode :: [Features] -> Features
calcMode vs = index [] vs
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

parseData = do
  d <- readFile "adult.data"
  let l = lines d
      w = map (splitBy ',') l
      feat = map (parseLine) w
  return feat

splitBy delimiter = foldr f [[]]
        where f c l@(x:xs) | c == delimiter = []:l
                         | otherwise = (c:x):xs

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
