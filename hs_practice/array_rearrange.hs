getNewIndex :: Int -> Int -> Maybe Int

getNewIndex     n i 
            | i < 1 || i > 3*n || n < 2 = Nothing
            | i < (n + 1)   = Just (3*(i-1) + 1) -- a
            | i < (2*n + 1) = Just (3*( (i - n ) -1) + 2) -- b
            | otherwise     = Just (3*( (i - 2*n ) -1) + 3) -- c

getNewIndex1 :: Int -> Int -> Int

getNewIndex1     n i 
            | i < 1 || i > 3*n || n < 2 = 0
            | i < (n + 1)   =  (3*(i-1) + 1) -- a
            | i < (2*n + 1) =  (3*( (i - n ) -1) + 2) -- b
            | otherwise     =  (3*( (i - 2*n ) -1) + 3) -- c

getPath :: [Int] -> Int -> [Int]

getPath list i 
            | i == length list  = []
            | i == 0            = getPath list 2
            | otherwise         = val : getPath list val
        where val = list !! (i - 1 )
