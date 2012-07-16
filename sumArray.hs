--main = putStrLn "Hello"

--main =  do putStrLn "starting"
--           x <- readLn
--            putStr "input " 
sumArray :: Num a => [a] -> a 
sumArray (n:ns) =  n + sumArray ns
sumArray [] = 0 

isPresentIn val (x:xs) =    if val == x
                            then True
                            else isPresentIn val xs
isPresentIn _ [] = False

main = 
                if 2 `isPresentIn` val
                then putStrLn "1"
                else putStrLn "0"
            where val = [0, 1, 2 ] ::[Integer]
