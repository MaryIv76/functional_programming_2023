-- xGcd
xGcd a b
 | a == b     = a
 | a > b      = xGcd (a - b) b
 | otherwise  = xGcd a (b - a)


-- xPower, xPower2
xPower _ 0 = 1
xPower x n
 | (n `mod` 2) == 0  =  xPower (x * x) (n `div` 2)  
 | otherwise         =  x * xPower x (n-1)  


xPowerHelper _ 0 res = res
xPowerHelper x n res
 | (n `mod` 2) == 0  =  xPowerHelper (x * x) (n `div` 2) res 
 | otherwise         =  xPowerHelper x (n - 1) (res * x) 

xPower2 x n = xPowerHelper x n 1


-- xFibMatr
xFibMatrHelper a _ c _ 1 a' b' _ _ = a'*a + b'*c
xFibMatrHelper a b c d n a' b' c' d'
 | (n `mod` 2) == 0  = xFibMatrHelper (a*a + b*c) (a*b + b*d) (c*a + d*c) (c*b + d*d) (n `div` 2) a' b' c' d'
 | otherwise         = xFibMatrHelper a b c d (pred n) (a'*a + b'*c) (a'*b + b'*d) (c'*a + d'*c) (c'*b + d'*d)

xFibMatr n = xFibMatrHelper 0 1 1 1 n 1 0 1 0


-- allDivisors, isPerfectNum
allDivisorsHelper _ 0 l = l
allDivisorsHelper x i l = 
   let l'
        | (i * i) == x      = (i : l)
        | (x `mod` i) == 0  = (x `div` i) : (i : l)
        | otherwise         = l in
   allDivisorsHelper x (pred i) l'

allDivisors x = allDivisorsHelper x (floor $ sqrt (fromIntegral x)) []

isPerfectNum x = x == sum (allDivisors x) - x


-- syracuseLen
syracuseLenHelper x len
 | x == 1          = len + 1
 | x `mod` 2 == 0  = syracuseLenHelper (x `div` 2) (len + 1)
 | otherwise       = syracuseLenHelper (3 * x + 1) (len + 1)

syracuseLen x = syracuseLenHelper x 0


-- delannoy, delannoy2
delannoy 0 _ = 1
delannoy _ 0 = 1
delannoy m n = delannoy (m - 1) n + delannoy m (n - 1) + delannoy (m - 1) (n - 1)


delannoyHelper a b m n prevCol curCol prev 
 | m == a && n == b  = prev + prevCol!!(m) + prevCol!!(m - 1)
 | m == 0 && n == 0  = delannoyHelper a b (m + 1) n (replicate (a + 1) 0) [1] 1
 | m == 0            = delannoyHelper a b (m + 1) n curCol [1] 1
 | n == 0 && m == a  = delannoyHelper a b 0 (n + 1) prevCol (curCol ++ [prev]) prev
 | n == 0            = delannoyHelper a b (m + 1) n prevCol (curCol ++ [prev]) prev
 | m == a            = let cur = (prev + (prevCol !! m) + prevCol!!(m - 1)) in delannoyHelper a b 0 (n + 1) prevCol (curCol ++ [cur]) cur 
 | otherwise         = let cur = (prev + (prevCol !! m) + prevCol!!(m - 1)) in delannoyHelper a b (m + 1) n prevCol (curCol ++ [cur]) cur

delannoy2 _ 0 = 1
delannoy2 0 _ = 1
delannoy2 a b = delannoyHelper a b 0 0 [] [] 0


-- evalPolynomial
evalPolynomialHelper [] x res = res
evalPolynomialHelper (h:t) x res = evalPolynomialHelper t x (h + x * res)

evalPolynomial [] _ = 0
evalPolynomial (h:t) x = evalPolynomialHelper t x h


-- clone, clone2, clone3
clone _ [] = []
clone n (x:xs) = replicate n x ++ clone n xs


replicateHelper 0 _ res = res
replicateHelper n x res = replicateHelper (n - 1) x (x:res)

replicate' n x
 | n <= 0     = []
 | otherwise  = replicateHelper n x []

clone2 _ [] = []
clone2 n (x:xs) = replicate' n x ++ clone n xs


cloneHelper _ [] res = res
cloneHelper n (x:xs) res = cloneHelper n xs (res ++ replicate' n x)

clone3 n xs = cloneHelper n xs []


-- xZipWith, xZipWith2
xZipWith _ [] _ = []
xZipWith _ _ [] = []
xZipWith f (x:xs) (y:ys) = f x y : xZipWith f xs ys


xZipWithHelper _ [] _ res = res
xZipWithHelper _ _ [] res = res
xZipWithHelper f (x:xs) (y:ys) res = xZipWithHelper f xs ys (res ++ [f x y])

xZipWith2 f xs ys = xZipWithHelper f xs ys []


-- fib, infiniteFib, generalizedInfFib
fibHelper _ _ 0 res = res
fibHelper a b n res = fibHelper b (a + b) (pred n) (res ++ [a])

fib n = fibHelper 0 1 n []


infFibHelper a b = a : infFibHelper b (a + b) 
infiniteFib = infFibHelper 0 1 


generalizedInfFibHelper xs = sum xs : generalizedInfFibHelper (tail xs ++ [sum xs])
generalizedInfFib xs = xs ++ generalizedInfFibHelper xs 


-- fromDigits, toDigits, toDigits2, addDigitWise
fromDigitsHelper _ [] res = res
fromDigitsHelper x (d:ds) res = fromDigitsHelper x ds (x * res + d)

fromDigits x ds = fromDigitsHelper x ds 0


toDigits base x 
 | x < base   = [x]
 | otherwise  = (toDigits base (x `div` base)) ++ [x `mod` base]


toDigitsHelper base x res
 | x < base   = x : res
 | otherwise  = toDigitsHelper base (x `div` base) ((x `mod` base) : res)

toDigits2 base x = toDigitsHelper base x []


addDigitWise base xs ys = toDigits2 base (fromDigits base xs + fromDigits base ys)


-- delannoyPaths
delannoyPaths 0 0 = [[]]
delannoyPaths 0 n = [replicate' n 0]
delannoyPaths m 0 = [replicate' m 2]
delannoyPaths m n = map (++ [2]) (delannoyPaths (m - 1) n) ++ map (++ [0]) (delannoyPaths m (n - 1)) ++ map (++ [1]) (delannoyPaths (m - 1) (n - 1))


-- delannoyPaths2 a b = delPathsHelper a b 0 0 [[[]]] [[[]]] [[]]

-- delPathsHelper a b m n prevCol curCol prev
--  | m == a && n == b  = map (++ [2]) prev ++ map (++ [0]) prevCol!!(m) ++ map (++ [1]) prevCol!!(m - 1)
--  | m == 0 && n == 0  = delannoyHelper a b (m + 1) n [[[]]] [[[]]] [[]]
--  | m == 0            = delannoyHelper a b (m + 1) n curCol [map (++ [0]) curCol!!m] (map (++ [0]) curCol!!m)
--  | n == 0 && m == a  = delannoyHelper a b 0 (n + 1) prevCol (curCol ++ [map (++ [2]) prev]) (map (++ [2]) prev)
--  | n == 0            = delannoyHelper a b (m + 1) n prevCol (curCol ++ [map (++ [2]) prev]) (map (++ [2]) prev)
--  | m == a            = let cur = (map (++ [2]) prev ++ map (++ [0]) prevCol!!(m) ++ map (++ [1]) prevCol!!(m - 1)) in delannoyHelper a b 0 (n + 1) prevCol (curCol ++ [cur]) cur 
--  | otherwise         = let cur = (map (++ [2]) prev ++ map (++ [0]) prevCol!!(m) ++ map (++ [1]) prevCol!!(m - 1)) in delannoyHelper a b (m + 1) n prevCol (curCol ++ [cur]) cur
