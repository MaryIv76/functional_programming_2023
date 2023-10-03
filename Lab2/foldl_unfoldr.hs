import Data.List (foldl, unfoldr, nub)

-- Развернуть натуральное число n в список всех чисел, меньших его
-- lessX, natural_lessX
lessX x = unfoldr (\x -> Just(x, x - 1)) (pred x)


naturalLessXCore x
 | x == 0     = Nothing
 | otherwise  = Just(x, x - 1)

naturalLessX x = unfoldr naturalLessXCore (pred x)


-- Развернуть число в список разрядов его двоичного представления
-- toBinary
toBinaryCore 0 = Nothing
toBinaryCore x = Just (x `mod` 2, x `div` 2)

toBinary x = reverse $ unfoldr toBinaryCore x


-- Список разрядов преобразовать свёрткой в значение числа
-- fromBinary
fromBinaryCore res d = 2 * res + d

fromBinary ds = foldl fromBinaryCore 0 ds


-- Развернуть число в список его простых делителей
-- toPrimeDivisors, toUniquePrimeDivisors
toPrimeDivisorsCore (x, d, limit)
 | d > limit && x /= 1          = Just(x, (x `div` x, d, limit))
 | d > limit                    = Nothing
 | x `mod` d == 0               = Just(d, (x `div` d, d, limit))
 | otherwise                    = toPrimeDivisorsCore (x, succ d, limit)

toPrimeDivisors x = unfoldr toPrimeDivisorsCore (x, 2, floor $ sqrt (fromIntegral x))

toUniquePrimeDivisors = nub . toPrimeDivisors


-- Выразить список первых n чисел Фибоначчи через развёртку
-- Модификация: бесконечный список
-- fib, infFib
fibCore (a, b, n)
 | n == 0     = Nothing
 | otherwise  = Just(a, (b, a + b, pred n))

fib n = unfoldr fibCore (0, 1, n)


infFibCore (a, b) = Just(a, (b, a + b))

infFib = unfoldr infFibCore (0, 1)


-- Развернуть число в сиракузскую последовательность
-- syracuse
syracuseCore 1 = Nothing
syracuseCore x 
 | x `mod` 2 == 0  = Just(x, x `div` 2)
 | otherwise       = Just(x, 3 * x + 1)

syracuse x = (unfoldr syracuseCore x) ++ [1]


-- Выразить список простых чисел, не превышающих n, через развёртку с помощью решета Эратосфена
-- Модификация: бесконечный список всех простых чисел
-- primes, infPrimes
primesCore [] = Nothing
primesCore (x:xs) = Just (x, filter (\t -> t `mod` x /= 0) xs)

primes n = unfoldr primesCore (2: [3, 5 .. n])


infPrimes = unfoldr primesCore (2: [3, 5 ..])

