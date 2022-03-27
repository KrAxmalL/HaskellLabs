{-# OPTIONS_GHC -Wall #-}
module Lab01 where

-- Function 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n | n <= 1 = 1
            | otherwise = n * factorial(n - 1)

-- Function 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum [] [] = []
listSum xs [] = xs
listSum [] ys = ys
listSum (x:xs) (y:ys) = (x + y) : listSum xs ys

-- Function 3 ----------------------------------------- 
oddEven :: [Int] -> [Int]
oddEven [] = []
oddEven [x] = [x]
oddEven (x:xs) = head xs : x : oddEven (tail xs)

-- Function 4 -----------------------------------------
position    ::  Int -> [Int] -> Int
position n xs = positionCounter n xs 0
          where positionCounter :: Int -> [Int] -> Int -> Int
                positionCounter _ [] _ = -1
                positionCounter num (y:ys) counter | num == y = counter
                                                   | otherwise = positionCounter num ys (counter + 1)

-- Function 5 -----------------------------------------
set :: [Int] -> [Int]
set xs = createSet xs []
   where createSet :: [Int] -> [Int] -> [Int]
         createSet [] zs = zs
         createSet (y:ys) zs | position y zs == -1 = createSet ys (insert y zs)
                             | otherwise = createSet ys zs

-- Function inserts element in the end of the given list -------
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x xs = head xs : insert x (tail xs)

-- Function 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union [] [] = []
union [] bs = set bs
union as [] = set as
union as (b:bs) | position b as == -1 = union (insert b as) bs
                | otherwise = union as bs

-- Function 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = findIntersection xs ys []
        where findIntersection :: [Int] -> [Int] -> [Int] -> [Int]
              findIntersection [] [] cs = cs
              findIntersection _ [] cs = cs
              findIntersection [] _ cs = cs
              findIntersection (a:as) bs cs | position a cs == -1 && position a bs >= 0 = findIntersection as bs (insert a cs)
                                            | otherwise = findIntersection as bs cs

-- Function 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial y | y <- [1, 2 ..]]