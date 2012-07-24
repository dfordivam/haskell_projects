getNewIndex :: Int -> Int -> Maybe Int

getNewIndex     n i 
            | i < 1 || i > 3n = Nothing
            | i < (n + 1)   = 3(i-1) + 1 -- a
            | i < (2n + 1)  = 3(i-1) + 2 -- b
            | otherwise     = 3(i-1) + 3 -- c



