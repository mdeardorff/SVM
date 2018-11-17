
module SVM where
  import Parsing

  parseData ('5':'0':'5':_) = [[1]]

  prepData = do
    adultData <- readFile "adult.data"
    let l = lines adultData
        s = map (splitBy ',') l
        d = map (parseLine) s
        preparedData = map catMaybes d

    return preparedData

  splitBy delimiter = foldr f [[]]
          where f c l@(x:xs) | c == delimiter = []:l
                           | otherwise = (c:x):xs


  parseLine :: [String] -> [Maybe Double]
  parseLine (age:wclass:fnlwgt:ed:ednum:mstatus:occ:rel:race:sex:cgain:closs:hpw:nc) = [Just (read age), lookup wclass wclassKey, Just (read fnlwgt), lookup ed edKey, Just (read ednum),
   lookup mstatus mstatusKey]

  catMaybes :: [Maybe a] -> [a]
  catMaybes ls = [x | Just x <- ls]

  enum :: String -> [(String,Float)] -> Float
  enum s e = case lookup s e of
    Just i -> i
    _      -> 0


  wclassKey = [(" Private", 1), (" Self-emp-not-inc",2),(" Self-emp-inc",3),(" Federal-gov",4), (" Local-gov", 5), (" State-gov", 6), (" Without-pay", 7), (" Never-worked", 8)]
  edKey = [(" Bachelors", 1), (" Some-college", 2), (" 11th", 3), (" HS-grad", 4), (" Prof-school", 5),  (" Assoc-acdm", 6), (" Assoc-voc", 7), (" 9th",8), (" 7th-8th", 9),(" 12th", 10), (" Masters", 11),
            (" 1st-4th", 12), (" 10th", 13), (" Doctorate", 14), (" 5th-6th", 15), (" Preschool", 16)]

  cga :: [[Float]] -> [Float] -> [Float] -> Int
  cga = undefined
