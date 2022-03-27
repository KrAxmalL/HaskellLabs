{-# OPTIONS_GHC -Wall #-}
module Lab02 where

-- Function 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr = foldr (+) 0 

-- Function 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1 .. n]

-- Function 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr [] [] = []
concatFr [] ys = ys
concatFr xs [] = xs
concatFr xs ys = foldr (:) ys xs

-- Function 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert [] = []
sortInsert [x] = [x]
sortInsert (x:xs) = insert (sortInsert xs) x
          where insert :: [Integer] -> Integer -> [Integer]
                insert [] n = [n]
                insert ys n | n > head ys = head ys : insert (tail ys) n
                            | otherwise = n : ys

-- Function 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] [] = []
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

-- Function 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = calculateSum m n 0
                     where calculateSum :: Integer -> Integer -> Double -> Double
                           calculateSum a b accum | b < 1 = accum
                                                  | otherwise = calculateSum a (b - 1) (elemOfSeries a b + accum) 
                           elemOfSeries :: Integer -> Integer -> Double 
                           elemOfSeries a b = fromIntegral (a ^ b) / fromIntegral (factorial b) 

-- Function 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Function 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (\prev curr -> (curr * curr) + prev) [1..]

-- Function 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes [] [] = []
indexes [] ys = [0 .. length ys]
indexes _ [] = []
indexes xs ys = positionOfList xs (splitOnParts ys (length xs))

-- Function splits a list on the lists of the given length with step 1 ----------
splitOnParts :: [Int] -> Int -> [[Int]]
splitOnParts xs n | n > length xs = []
                  | otherwise = take n xs : splitOnParts (tail xs) n

-- Function checks if lists are equal ----------
compareLists :: [Int] -> [Int] -> Bool
compareLists [] [] = True 
compareLists _ [] = False 
compareLists [] _ = False 
compareLists xs ys = (head xs == head ys) && compareLists (tail xs) (tail ys)

-- Function calculates the position of list in the list of lists ----------
positionOfList :: [Int] -> [[Int]] -> [Int]
positionOfList [] _ = []
positionOfList xs ys = positionCounter xs ys 0
                 where positionCounter :: [Int] -> [[Int]] -> Int -> [Int]
                       positionCounter _ [] _ = []
                       positionCounter [] bs counter = [counter .. length bs + counter]
                       positionCounter as (b:bs) counter | compareLists as b = counter : positionCounter as bs (counter + 1)
                                                         | otherwise = positionCounter as bs (counter + 1)
                                

