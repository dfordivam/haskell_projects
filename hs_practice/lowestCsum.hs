import Data.Array

lowestCsum :: [Int] -> (Int , (Int, Int) )
lowestCsum = undefined

-- reduce the array to alternate + -
data SumNode = SumNode {
    sum     ::  Int,
    start   ::  Int,
    end     ::  Int
} deriving (Show)

get_final_nodes :: [Int] -> [SumNode]
get_final_nodes = reduce_sumNodes.reverse.reduce_list

reduce_sumNodes :: [SumNode] -> [SumNode]
reduce_sumNodes nodeList
                | length nodeList < 3   = nodeList
                | doMergingOfNodes      = reduce_sumNodes (newNode : rest)
                | otherwise             = first : (second :  reduce_sumNodes (third : rest) )

           where    nodeSum = Main.sum first + Main.sum second + Main.sum third
                    newNode = SumNode nodeSum (start first) (end third)
                    first = nodeList !! 0
                    second = nodeList !! 1
                    third = nodeList !! 2
                    rest = drop 3 nodeList
                    doMergingOfNodes = Main.sum first >= Main.sum second && Main.sum third >= Main.sum second




reduce_list :: [Int] -> [SumNode]

reduce_list lst@(xs) = reduce_array arr
    where   arr = listArray (1, l) xs
            l   = length xs

reduce_array :: Array Int Int -> [SumNode]
reduce_array arr = reduce_array_mid arr []

-- reduce_array _ = []
reduce_array_mid :: Array Int Int -> [SumNode] -> [SumNode]

reduce_array_mid arr [] = reduce_array_mid arr [newNode]
            where newNode = get_sum_node (1::Int) arr

reduce_array_mid arr sumNodeList 
                | start == snd (bounds arr) = sumNodeList
                | otherwise                 = reduce_array_mid arr (newNode : sumNodeList)

            where   start = end (head sumNodeList)
                    newNode = get_sum_node (start + 1) arr


get_sum_node :: Int -> Array Int Int -> SumNode
get_sum_node start arr = SumNode nodeSum nodeStart nodeEnd
        where   (nodeSum, nodeEnd) =    if  isPos
                                        then get_sum_of_array (first, start + 1) arr
                                        else get_nsum_of_array (first, start + 1) arr 
                first   = arr ! start
                isPos   = first >= 0
                nodeStart = start



get_sum_of_array :: (Int, Int) -> Array Int Int -> (Int, Int)
get_sum_of_array (arrSum, end) arr 
                | end > snd (bounds arr)            = (arrSum, end - 1)
                | arr ! end < 0                     = (arrSum, end - 1 )
                | otherwise                         = get_sum_of_array (arrSumNew, end + 1 ) arr
                                    where arrSumNew = arrSum + arr ! end

get_nsum_of_array :: (Int, Int) -> Array Int Int -> (Int, Int)
get_nsum_of_array (arrSum, end) arr 
                | end > snd (bounds arr)           = (arrSum, end - 1)
                | arr ! end > 0                     = (arrSum, end - 1)
                | otherwise                         = get_nsum_of_array (arrSumNew, end + 1 ) arr
                                    where arrSumNew = arrSum + arr ! end


