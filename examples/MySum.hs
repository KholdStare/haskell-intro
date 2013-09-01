module MySum
    ( mySum
    )
where

mySum xs = mySum' 0   xs
     where mySum' acc []     = acc
           mySum' acc (x:xs) = mySum' (acc+x) xs
