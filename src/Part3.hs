module Part3 where

import Data.Char
import Data.String

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 1 = False
prob18 x = and [x `mod` y /= 0 | y <- [2 .. (x-1)]]

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 n = 2 * n == sum (prob21 n)

-- получение списка делителей числа(исключая само число)
divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1 .. (n - 1)], rem n x == 0]

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = [x | x <- [1 .. n], rem n x == 0]

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 "" = 0
prob22 text = product (map (max 1 . count 'i') (words text))

count :: Eq a => a -> [a] -> Integer
count item = fromIntegral . length . filter (== item)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 num = findNum (sqrt (1 + 8 * fromInteger num)) == 0
  where
    findNum x = x - fromIntegral (floor x)

-- генерация треугольного числа
triangular :: Integer -> Integer
triangular x = x * (x + 1) `div` 2

-- генерация списка треугольного числа
triSeries :: [Integer]
triSeries = map triangular [1..]

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 = isPalindrome . toDigits

isPalindrome :: Eq a => [a] -> Bool
isPalindrome word = word == reverse word

toDigits :: Integer -> [Int]
toDigits = iter []
  where
    iter acc 0 = acc
    iter acc n = iter (fromIntegral (n `mod` 10) : acc) (n `div` 10)

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 x y = sum (divisors x) == y && sum (divisors y) == x

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 x [] = Nothing
prob27 sum (x:xs) = case findComplement sum x xs of
    Nothing -> prob27 sum xs
    (Just compl) -> Just (x, compl)
  where
    findComplement _ _ [] = Nothing
    findComplement sum item (x:xs)
      | item + x == sum = Just x
      | otherwise = findComplement sum item xs

-- todo doesn't work
prob222 :: Int -> [Int] -> Maybe (Int, Int)
prob222 n arr = case arr of
  [] -> Nothing
  [a] -> Nothing
  a ->  checkFirst (head a) n a
          where
            checkFirst :: Int -> Int -> [Int] -> Maybe (Int, Int)
            checkFirst curr n a = case a of
              [] -> Nothing
              [o] -> if curr + o == n
                     then Just (curr, o)
                     else checkSecond (head (tail a)) n (tail a)
                       where
                         checkSecond :: Int -> Int -> [Int] -> Maybe (Int, Int)
                         checkSecond curr2 n a =
                            if (curr2 + head (tail a)) == n
                            then Just (curr2, head (tail a))
                            else Nothing
              o -> if curr + head (tail o) == n
                   then Just (curr, head (tail o))
                   else Nothing


------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 1 = 9
prob29 2 = 9009
prob29 3 = 906609
prob29 k = fromInteger (maximum (filter prob25 ([x * y | x <- range, y <- range])))
 where
    range = [10^k - 1, 10^k - 2..10^(k-1)]

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 k = head (filter (\t -> length (divisors2 t) >= k) triSeries)

-- получение списка делителей числа(исключая само число)
divisors2 :: Integral a => a -> [a]
divisors2 n = [x | x <- [1 .. n], rem n x == 0]

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 n = sum (map sumCouple (friends n))

-- получение пар дружественных чисел
friends :: Int -> [(Int, Int)]
friends x = [(m, n) | m <- [1..x], n <- [1..(m - 1)],
            sum (divisors m) == n,
            sum (divisors n) == m]

-- получение пар дружественных чисел
friends2 :: Integer -> [(Integer, Integer)]
friends2 x = [(m, n) | m <- [1..x], n <- [1..(x - 1)],
            sum (divisors m) == n,
            sum (divisors n) == m]

sumCouple :: (Int, Int) -> Int
sumCouple (x, y) = x + y

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"


coins ::(Ord a, Num a) => [a]
coins = [5,9,13]


change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change amount = [ c:cs |c<-coins, amount>=c, cs<-change (amount - c) ]

