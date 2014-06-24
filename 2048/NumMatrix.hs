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
import Data.Traversable
import System.Random

data Direction = UpD | DownD | LeftD | RightD deriving (Show, Eq)

data NumMatrix = NumMatrix {
    n :: Int,
    arr :: Array Int Int
    } deriving (Eq)

instance Show NumMatrix where
    show = showNumMatrix

showNumMatrix :: NumMatrix -> String
showNumMatrix (NumMatrix n arr) = if (null (indices arr)) then "" 
                                            else showARow (row) ++ "\n" ++ showNumMatrix rest
        where e = splitAt n (elems arr)
              row = fst e
              rest = NumMatrix n restArr
              restArr = listArray (1, length restList) restList
              restList = snd e
              showARow [] = ""
              showARow (x:xs) = "\t" ++ show x ++ "  " ++ (showARow xs)

getEmptyMatrix size = NumMatrix size ar
  where a = (replicate (size*size) 0)::[Int]
        t = (1::Int, size*size::Int)
        ar = (listArray t a):: Array Int Int

initMatrix :: NumMatrix -> IO NumMatrix
initMatrix nm@(NumMatrix size arr) = do
    g <- getStdGen    
    let r1 =  head (randomRs (1::Int, size*size::Int) g)
    let nm1 = addNum nm r1 2
    let r2 =  head (randomRs (1::Int, size*size::Int) g)
    let nm2 = addNum nm1 r2 2
    return nm2

addNum :: NumMatrix -> Int -> Int -> NumMatrix
addNum nm@(NumMatrix n arr) i v = NumMatrix n retArr
   where retArr = arr // [(index, v)]
         index :: Int
         index = head (snd (splitAt i (cycle emptyIndices)))
         emptyIndices :: [Int]
         emptyIndices = [(fst i) | i <- (assocs arr), snd i == 0]


processInput :: NumMatrix -> Direction -> Maybe (NumMatrix)
processInput nm@(NumMatrix n arr) dir = 
            if modifiedData then (Just retData) else Nothing
    where retData = if dirH
                       then NumMatrix n (listArray (1,n*n) out1)
                       else NumMatrix n (array (1,n*n) out2)

          out1 = concat (map fst mid)
          modifiedData = or (map snd mid)
          --modifiedData = retData == nm
          
          dirH = (dir == LeftD) || (dir == RightD)

          mid = if dirH
                   then map (shiftData n dir) rows
                   else map (shiftData n dir) colmns
          
          shiftData :: Int -> Direction -> [Int] -> ([Int], Bool)
          shiftData n dir arr0 = if (n < 2) then (arr, False) else
                                    if dirL
                                    then (o:(fst restOut) , modified)
                                    else (reverse (fst reversed),snd reversed)
            where o = if (h == 0) then v 
                                  else (if h == v then 2*v else h)
                  p = if (h == 0) then 0
                                  else (if h == v then 0 else v)
                  v = head (tail arr)
                  h = head arr
                  rest = p : (tail (tail arr))
                  restOut = (shiftData (n-1) dir (shiftEmptySpace rest 0))
                  modified = modifiedHere || (snd restOut)
                  modifiedHere = not(h == o) || not (arr == arr0)
                  arr = shiftEmptySpace arr0 0
                  shiftEmptySpace [] n = if n == 0 then [] else replicate n 0
                  shiftEmptySpace (x:xs) n = if x == 0  
                                            then shiftEmptySpace xs (n+1)
                                            else x:(shiftEmptySpace xs n)
                  dirL = (dir == LeftD) ||  (dir == UpD)
                  reversed = shiftData n LeftD (reverse arr0)

          --rows = map (map ((!) arr)) (indices getrowIndices)
          --colmns = map (map ((!) arr)) (indices getcolIndices)
          --indices f = (map (Data.Traversable.sequence (map f [1..n]) ) [1..n])
          --getrowIndices = (\x -> (\y -> (x + (y-1)*n)))
          --getcolIndices = (\x -> (\y -> ((x-1)*n + y)))
          colIndices = [[(i+(j-1)*n)| j<- [1..n]] | i<- [1..n]]
          rowIndices = [[(j+(i-1)*n)| j<- [1..n]] | i<- [1..n]]
          rows = map (map ((!) arr)) rowIndices
          colmns = map (map ((!) arr)) colIndices

          out2 = getRows out1 (concat colIndices)
            where getRows _ [] = []
                  getRows (x:xs) (i:is) = (i,x): (getRows xs is)
              

