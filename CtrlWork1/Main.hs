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

problem1 = do
    print (intToWords  150)  -- "one-five-zero"
    print (intToWords    0)  -- "zero"
    print (intToWords (-10)) -- "minus-one-zero"

main = do
  problem1