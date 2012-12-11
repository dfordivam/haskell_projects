-- Input x:xs 
-- output length / Int

monadic_length :: [a] -> Int

monadic_length (x:xs) = 1 + monadic_length xs

monadic_length [] = 0
