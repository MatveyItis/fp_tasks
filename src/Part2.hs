module Part2 where

import Part2.Types

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 c = case c of
  Red x -> x >= 0 && x <= 255
  Green x -> x >= 0 && x <= 255
  Blue x -> x >= 0 && x <= 255

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 c p = case p of
  Red x -> c {red = red c + x}
  Green x -> c {green = green c + x}
  Blue x -> c {blue = blue c + x}

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 c = case c of
  Red x -> x
  Green x -> x
  Blue x -> x

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 c = do
  let r = red c
  let g = green c
  let b = blue c
  if r > g && r > b
    then Just (Red r)
    else
      if g > b && g > r
        then Just (Green g)
        else
          if b > g && b > r
            then Just (Blue b)
            else Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 t = sum (convertToList t)

convertToList :: Tree a -> [a]
convertToList tree = [root tree] ++ maybeConvert (left tree) ++ maybeConvert (right tree)
  where
    maybeConvert (Just x) = convertToList x
    maybeConvert Nothing = []

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 = checkTree

checkTree :: Ord a => Tree a -> Bool
checkTree tree = checkLeft (left tree) (root tree) && checkRight (right tree) (root tree)

checkRight :: Ord a => Maybe (Tree a) -> a -> Bool
checkRight Nothing x = True
checkRight (Just tree) parent = root tree >= parent && checkTree tree

checkLeft :: Ord a => Maybe (Tree a) -> a -> Bool
checkLeft Nothing x = True
checkLeft (Just tree) parent = root tree < parent && checkTree tree


------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 a tree = findTree a (Just tree)

-- метод для поиска поддерева
findTree :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
findTree a Nothing = Nothing
findTree a (Just tree)
  | a > root tree = findTree a (right tree)
  | a < root tree = findTree a (left tree)
  | otherwise = Just tree

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
