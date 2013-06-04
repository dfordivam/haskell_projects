import System.IO

--myfun  = []
myfun :: String -> String
myfun name = tail name

main =  do 
        output1
        name <- getLine
        outFile <- openFile "io_simple_txt" AppendMode
        let output2 = myfun name
        hPutStrLn outFile output2
        hClose outFile
    where output1 = putStrLn "Hello World!"
