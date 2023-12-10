import Data.List

{- PROBLEM 1
It is often needed to convert a number written in arabic symbols,
to a string of its textual representation, i.e. for financial documents.
Write a function intToWords that transcribes an integer into its 
textual representation in format "digit-digit-digit-...". 
-}

intToWords :: (Num a, Show a) => a -> String
intToWords x = lst (show x)

lst [x] = convert x
lst x = convert(head x) ++ "-" ++ lst (tail x)

convert x
    | x == '-' = "minus"
    | otherwise = strNum (read [x])

strNum x = listOfNum !! x

listOfNum = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

{- PROBLEM 2
Write a function findMaxFrequency that for a given homogenous list of type a
returns a pair (a, Int) of the most frequent element (any, if there are more than one) and its frequecy. For an empty list throw an error. 
-}

findMaxFrequency :: (Ord a) => [a] -> (Int, a)
findMaxFrequency []  = error "Empty list"
findMaxFrequency lst = maximum (freq lst)

freq x = map(sorted)(list x)

sorted x = (length(head(list x)), head(head(list x)))

list x = group(sort x)

problem1 = do
    print (intToWords  150)  -- "one-five-zero"
    print (intToWords    0)  -- "zero"
    print (intToWords (-10)) -- "minus-one-zero"

problem2 = do
  print "Problem 2"
  print $ findMaxFrequency [1,2,1,3,1,4]   -- (1, 3)
  print $ findMaxFrequency [1,1,2,2]       -- (1, 2) or (2, 2)
  print $ findMaxFrequency "some sentence" -- ('e', 4)
  print $ findMaxFrequency ([] :: String)  -- error

main = do
  problem1
  problem2