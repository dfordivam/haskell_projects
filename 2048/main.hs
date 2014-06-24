import NumMatrix
import Data.Maybe
import System.Random

main :: IO ()
main = do
    let nm = getEmptyMatrix 4
    s <- initMatrix nm
    loop s

loop nm = do 
            print nm
            g <- getStdGen    
            let r1 =  head (randomRs (1::Int, 16) g)
            c <- getChar
            d <- getChar
            case c of 'q' -> exitGracefully
                      'w' -> checkInput nm UpD r1
                      'a' -> checkInput nm LeftD r1
                      's' -> checkInput nm DownD r1
                      'd' -> checkInput nm RightD r1
                      (_)  -> do {print "?" ; loop nm}

exitGracefully = print "Bye!"

checkInput :: NumMatrix -> Direction -> Int -> IO ()
checkInput nm dir r1 = if v == Nothing 
                       then 
                            if vall == [] then print "Game Over!"
                            else do { print "Try Again"; loop nm}
                       else loop newV
            where v = processInput nm dir
                  newV = addNum (fromJust v) r1 2
                  vall = catMaybes (map (processInput nm) dirs)
                  dirs = [LeftD, UpD, RightD, DownD]

