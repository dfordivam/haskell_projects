import System.IO

--csvToSpaceSeparated :: String -> String

--csvToSpaceSeparated inpStr = lines inpStr 

toFile :: IO()
toFile =  do
            inpStr <- readFile "inp.csv"
            writeFile "out.txt" (unlines (fun8 (lines inpStr)))


-- parse input lines and give adjusted lines
fun8 :: [String] -> [String]
fun8 = fun9.fun6.fun1

-- combine back the columns
fun9 :: [[String]] -> [String]
fun9 (x:xs) = concat (fun10 x) : fun9 xs
fun9 [] = []


fun10 :: [ String ] -> [ String ]
fun10 (x:xs) = x : separator : fun10 xs
    where separator = "     " :: String
fun10 [] = []

-- Parse input lines and split at commas
fun1 :: [String]  -> [[String]]

fun1 (x:xs) = (fun2 x [] ) : fun1 xs
fun1 [] = []

fun2 :: String -> String -> [String]
fun2 restLine@(x:xs) val  
    | x == ','  = val : fun2 xs ""
    | otherwise = fun2 xs (val ++ [x])
fun2 [] val = [val]

fun4 :: [Int] -> [Int] -> [Int]
fun4 (x:xs) (y:ys) 
    | x < y     = y : fun4 xs ys
    | otherwise = x : fun4 xs ys
fun4 x [] = x
fun4 [] y = y

-- get Max
fun5 :: [[String]] -> [Int] -> [Int]
fun5 (x:xs) inMax = fun5 xs newMax
    where   newMax = fun4 inMax xMax
            xMax = map length x

fun5 [] inMax = inMax

-- Modify width
fun6 :: [[String]] -> [[String]]
fun6 inpData@(x:xs) = newData
    where   max    = fun5 inpData []
            newData     = map (fun7 max) inpData

fun6 [] = []

-- modify width of a row
fun7 :: [Int] -> [String] -> [String]
fun7 (n:ns) (x:xs)
    | (length x) < n  = modifiedStr : fun7 ns xs
    | otherwise     = x : fun7 ns xs
    where   modifiedStr = spaces ++ x
            spaces = getSpaces (n - length x)

fun7 _ [] = []

getSpaces :: Int -> String
getSpaces x 
    | x > 0     = " " ++ getSpaces (x-1)
    | otherwise = []

