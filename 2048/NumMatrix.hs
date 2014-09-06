module NumMatrix
    (
        NumMatrix(..),
        Direction(..),
        getEmptyMatrix,
        initMatrix,
        processInput,
        addNum
    ) where

import Data.Array.IArray
import System.Random

data Direction = UpD | DownD | LeftD | RightD deriving (Show, Eq)

data NumMatrix = NumMatrix {
    nmSize :: Int,
    nmArray :: Array Int Int
    } deriving (Eq)

instance Show NumMatrix where
    show = showNumMatrix

showNumMatrix :: NumMatrix -> String
showNumMatrix (NumMatrix n arr) = if null (indices arr) then "" 
                                            else showARow row ++ "\n" ++ showNumMatrix rest
        where e = splitAt n (elems arr)
              row = fst e
              rest = NumMatrix n restArr
              restArr = listArray (1, length restList) restList
              restList = snd e
              showARow [] = ""
              showARow (x:xs) = "\t" ++ show x ++ "  " ++ showARow xs

getEmptyMatrix :: Int -> NumMatrix
getEmptyMatrix size = NumMatrix size ar
  where a = replicate (size*size) 0 ::[Int]
        t = (1::Int, size*size::Int)
        ar = listArray t a :: Array Int Int

initMatrix :: NumMatrix -> IO NumMatrix
initMatrix nm@(NumMatrix size _) = do
    g <- getStdGen    
    let r1 =  head (randomRs (1::Int, size*size::Int) g)
    let nm1 = addNum nm r1 2
    let r2 =  head (randomRs (1::Int, size*size::Int) g)
    let nm2 = addNum nm1 r2 2
    return nm2

addNum :: NumMatrix -> Int -> Int -> NumMatrix
addNum (NumMatrix n arr) i v = NumMatrix n retArr
   where retArr = arr // [(k, v)]
         k :: Int
         k = head (snd (splitAt i (cycle emptyIndices)))
         emptyIndices :: [Int]
         emptyIndices = [ fst j | j <- assocs arr, snd j == 0]


processInput :: NumMatrix -> Direction -> Maybe NumMatrix
processInput (NumMatrix n arr) dir = 
            if modifiedData then Just retData else Nothing
    where retData = NumMatrix n a
            where a = if dirH
                      then listArray (1,n*n) out1
                      else array (1,n*n) out2

          out1 = concatMap fst mid
          modifiedData = any snd mid
          --modifiedData = retData == nm
          
          dirH = (dir == LeftD) || (dir == RightD)

          mid = map (shiftData n dir) (if dirH then rows else colmns)

          shiftData :: Int -> Direction -> [Int] -> ([Int], Bool)
          shiftData n0 dir0 arr0
                    | n0 < 2 = (arr1, False)
                    | dirL = (o : fst restOut , modified)
                    |otherwise = (reverse (fst reversed), snd reversed)

            where o | h == 0 = v 
                    | h == 2*v = h
                    | otherwise = h
                  
                  p = if (h == 0) || (h == v) then 0 else v
                  v = head (tail arr1)
                  h = head arr1
                  rest = p : tail (tail arr1)
                  restOut = shiftData (n-1) dir (shiftEmptySpace rest 0)
                  
                  -- Is either modified in this iteration or some inner iteration
                  modified = modifiedHere || snd restOut
                  modifiedHere = (h /= o) || (arr1 /= arr0)

                  arr1 = shiftEmptySpace arr0 0

                  -- Returns a list with non-zero elements shifted and 0s appended at the end
                  shiftEmptySpace :: [Int] -> Int -> [Int]
                  shiftEmptySpace [] zeros = replicate zeros 0  -- Append 0s
                  shiftEmptySpace (x:xs) zeros = if x == 0  -- Skip non-zero elements
                                            then shiftEmptySpace xs (zeros + 1)
                                            else x : shiftEmptySpace xs zeros

                  dirL = (dir0 == LeftD) ||  (dir0 == UpD)
                  reversed = shiftData n LeftD (reverse arr0)

          --rows = map (map ((!) arr)) (indices getrowIndices)
          --colmns = map (map ((!) arr)) (indices getcolIndices)
          --indices f = (map (Data.Traversable.sequence (map f [1..n]) ) [1..n])
          --getrowIndices = (\x -> (\y -> (x + (y-1)*n)))
          --getcolIndices = (\x -> (\y -> ((x-1)*n + y)))
          colIndices = [[ i+(j-1)*n | j<- [1..n]] | i<- [1..n]]
          rowIndices = [[ j+(i-1)*n | j<- [1..n]] | i<- [1..n]]
          rows = map (map (arr !)) rowIndices
          colmns = map (map ( arr !)) colIndices


          out2 = getRows out1 (concat colIndices)
            where getRows _ [] = []
                  getRows (x:xs) (i:is) = (i,x): getRows xs is
              

