import System.IO

-- Two types of bricks
data BrickLength = Two | Three
data BrickType = Horizontal | Vertical
data Brick = Brick {
            brickType :: BrickType
            , brickLength :: BrickLength
            , position :: (Int, Int)}
            deriving (show)


main =  do 
        inh <- openFile "brick_data" ReadMode
        let puzzle = processFile inh []
        print puzzle
        hClose inh


processFile inh puzzle = do 
                            ineof <- hIsEOF inh
                            if ineof
                            then return puzzle
                            else do inpStr <- hGetLine inh
                                    let newBrick = processLine inpStr
                                    processFile inh (newBrick:puzzle)

processLine inpStr = Brick Horizontal Two (0,1)
                    
