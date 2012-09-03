import System.IO
import Test.QuickCheck  

--csvToSpaceSeparated :: String -> String

--csvToSpaceSeparated inpStr = lines inpStr 

toFile :: IO()
toFile =  do
            inpStr <- readFile "inp.csv"
            writeFile "out.txt" (unlines (commaToSpaceTheLines (lines inpStr)))


-- parse input lines and give adjusted lines
commaToSpaceTheLines :: [String] -> [String]
commaToSpaceTheLines = addSpaceAndCombine . modifyWidth . splitAtComma

-- combine back the columns
addSpaceAndCombine :: [[String]] -> [String]
addSpaceAndCombine (x:xs) = concat (addSpace x) : addSpaceAndCombine xs
addSpaceAndCombine [] = []


addSpace :: [ String ] -> [ String ]
addSpace (x:xs) = x : separator : addSpace xs
    where separator = "     " :: String
addSpace [] = []

-- Parse input lines and split at commas
splitAtComma :: [String]  -> [[String]]

splitAtComma (x:xs) = (splitARowAtComma x [] ) : splitAtComma xs
splitAtComma [] = []

splitARowAtComma :: String -> String -> [String]
splitARowAtComma restLine@(x:xs) val  
    | x == ','  = val : splitARowAtComma xs ""
    | otherwise = splitARowAtComma xs (val ++ [x])
splitARowAtComma [] val = [val]

-- get Max Width Code

getMaxInts :: [Int] -> [Int] -> [Int]
getMaxInts (x:xs) (y:ys) 
    | x < y     = y : getMaxInts xs ys
    | otherwise = x : getMaxInts xs ys
getMaxInts x [] = x
getMaxInts [] y = y

getMaxWidthForRows :: [[String]] -> [Int] -> [Int]
getMaxWidthForRows (x:xs) inMax = getMaxWidthForRows xs newMax
    where   newMax = getMaxInts inMax xMax
            xMax = map length x

getMaxWidthForRows [] inMax = inMax

-- Modify width Code
modifyWidth :: [[String]] -> [[String]]
modifyWidth inpData@(x:xs) = newData
    where   max    = getMaxWidthForRows inpData []
            newData     = map (modifyWidthForARow max) inpData

modifyWidth [] = []

modifyWidthForARow :: [Int] -> [String] -> [String]
modifyWidthForARow (n:ns) (x:xs)
    | (length x) < n  = modifiedStr : modifyWidthForARow ns xs
    | otherwise     = x : modifyWidthForARow ns xs
    where   modifiedStr = spaces ++ x
            spaces = getSpaces (n - length x)

modifyWidthForARow _ [] = []

getSpaces :: Int -> String
getSpaces x 
    | x > 0     = " " ++ getSpaces (x-1)
    | otherwise = []



-- Testing
-- Using QuickCheck
prop_idempotent xs = commaToSpaceTheLines (commaToSpaceTheLines xs) == commaToSpaceTheLines xs
