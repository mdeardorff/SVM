module Clustering where
import Data.KMeans
import Data.Ord
import Data.List (group, sort)

dataSet :: [[Double]]
dataSet = [[2,2],[2,1],[0,0],[1,0],[0,1],[1,2]]

empK = [" Private", " Self-emp-not-inc", " Self-emp-inc", " Federal-gov", " Local-gov", " State-gov", " Without-pay", " Never-worked", " ?"]
eduK = [" Bachelors", " Some-college", " 11th", " HS-grad", " Prof-school", " Assoc-acdm", " Assoc-voc", " 9th", " 7th-8th", " 12th",  " Masters", " 1st-4th", " 10th", " Doctorate", " 5th-6th", " Preschool", " ?"]
mstatusK = [" Married-civ-spouse", " Divorced", " Never-married", " Separated", " Widowed", " Married-spouse-absent", " Married-AF-spouse", " ?"]
occK = [" Tech-support", " Craft-repair", " Other-service", " Sales", " Exec-managerial", " Prof-specialty", " Handlers-cleaners", " Machine-op-inspct", " Adm-clerical", " Farming-fishing", " Transport-moving", " Priv-house-serv", " Protective-serv", " Armed-Forces", " ?"]
relK = [" Wife", " Own-child", " Husband", " Not-in-family", " Other-relative", " Unmarried", " ?"]
raceK = [" White", " Asian-Pac-Islander", " Amer-Indian-Eskimo", " Other", " Black", " ?"]
genderK = [" Female", " Male", " ?"]
countryK = [" United-States", " Cambodia", " England", " Puerto-Rico", " Canada", " Germany", " Outlying-US(Guam-USVI-etc)", " India", " Japan", " Greece", " South", " China", " Cuba", " Iran", " Honduras", " Philippines", " Italy", " Poland", " Jamaica", " Vietnam", " Mexico", " Portugal", " Ireland", " France", " Dominican-Republic", " Laos", " Ecuador", " Taiwan", " Haiti", " Columbia", " Hungary", " Guatemala", " Nicaragua", " Scotland", " Thailand", " Yugoslavia", " El-Salvador", " Trinadad&Tobago", " Peru", " Hong", " Holand-Netherlands", " ?"]

run = kmeans 2 dataSet

data Vals = Numeric Int | Category Int deriving (Eq, Ord)

instance Show Vals where
  show (Numeric x) = "N: " ++ show x
  show (Category x) = "C: " ++ show x

mode :: (Ord a) => [a] -> [a]
mode xs = map fst $ filter ((==best).snd) counts
    where counts = map (\l -> (head l, length l)) . group . sort $ xs
          best = maximum (map snd counts)
mean :: [Vals] -> Vals
mean vs = Numeric ((add 0 vs) `div` length vs)
  where
    add acc [] = acc
    add acc ((Numeric x):xs) = add (acc + x) xs

--calculate prototype of list of vectors
--index :: [[Int]] -> [Int]
printLength = do
  d <- parseData
  let l = map (\x -> (show $ length x)) d
  return l

calcPrototype = do
  d <- parseData
  let p = calcMode d
  return p

calcMode :: [[Vals]] -> [Vals]
calcMode vs = index [] vs
  where
    index :: [Vals] -> [[Vals]] -> [Vals]
    index acc [] = acc
    index acc vs = case (head vs) of
      [] -> acc -- Category (head(mode $ map (head) vs)):acc
      ((Category x):xs)  -> index (acc ++ [(head $ mode $ map (head) vs)]) (map (tail) vs)--index (acc:(mode $ map (head) (vs))) (map (tail) vs)
      ((Numeric x):xs)   -> index (acc ++ [(mean $ map (head) vs)]) (map (tail) vs)
{-
  where
    index' acc [] = acc
    index' acc vs = undefined--index' (acc:(mode $ map (head) (vs))) (map (tail) vs)
-}
parseLine :: [String] -> [Vals]
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
