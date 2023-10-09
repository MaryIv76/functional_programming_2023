import Data.Function (fix)

-- xGcd
xGcdHelper f a b
 | a == b     = a
 | a > b      = f (a - b) b
 | otherwise  = f a (b - a)

xGcd a b = fix xGcdHelper a b


-- infiniteFib
infFibHelper f a b = a : f b (a + b)

infiniteFib = fix infFibHelper 0 1


-- fib
fibHelper f a _ 0 = a
fibHelper f a b n = f b (a + b) (pred n)

fib n = fix fibHelper 0 1 (pred n)


-- listSum
listSumHelper _ [] res = res
listSumHelper f (x:xs) res = f xs (res + x)

listSum xs = fix listSumHelper xs 0


-- listMin
listMinHelper _ [] min = min
listMinHelper f (x:xs) min = let newMin = if (x < min) then x else min in 
  f xs newMin

listMin (x:xs) = fix listMinHelper xs x


-- xReverse
xReverseHelper _ res [] = res
xReverseHelper f res (x:xs) = f (x:res) xs

xReverse xs = fix xReverseHelper [] xs
