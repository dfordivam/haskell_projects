maxVal (n:ns) = if ( n > ns_max )
                then n
                else ns_max
            where ns_max = maxVal ns
--maxVal [n] = n

maxVal [] = 0

main = 
                if maxVal val == 4
                then putStrLn "1"
                else putStrLn "0"
            where val = [0, 1, 4, 6, 5, 2, 3 ] ::[Integer]
