xLookup _ [] = Nothing
xLookup key ((k, v):xs)
 | key == k   = Just v
 | otherwise  = xLookup key xs