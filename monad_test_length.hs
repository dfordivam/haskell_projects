(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v 

llength :: String -> Maybe Int

llength str = case (length str) of
                 0 -> Nothing
                 x -> Just (x)

mllength :: String -> Maybe Int
mllength str = llength str >>? (llength.show)



