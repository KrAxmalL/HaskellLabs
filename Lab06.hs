{-# OPTIONS_GHC -Wall #-}
module Lab06 where
import GHC.Base (String)

newtype Poly a = P [a]

-- Function 1 -----------------------------------------
x :: Num a => Poly a
x = P [0, 1]

-- Function 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    (==) left right = case (left, right) of
        (P (l : ls), P (r : rs)) -> (r == l) && (P ls == P rs)
        (P ls, P []) -> null (filter (/= 0) ls)
        (P [], P rs) -> null (filter (/= 0) rs)

-- Function 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show poly = case poly of
        P [] -> "0"
        P [p] -> show p
        P idents -> if null (filter (/= 0) idents)
                    then "0"
                    else doShow (rmFstZr (reverse idents))

rmFstZr :: (Num a, Eq a, Show a) => [a] -> [a]
rmFstZr [] = []
rmFstZr xs | head xs == 0 = rmFstZr (tail xs)
           | otherwise = xs

doShow :: (Num a, Eq a, Show a) => [a] -> String
doShow idents = case idents of
    [] -> "0"
    [0] -> "0"
    [p] -> show p
    lst -> helpShow (lst, length lst - 1, "")

helpShow :: (Num a, Eq a, Show a) => ([a], Int, String) -> String
helpShow val = case val of
    ([], _, accum) -> accum
    ([0], _, accum) -> accum
    ([p], _, accum) -> accum ++ " + " ++ show p
    (p : ps, count, accum) -> helpShow(ps, count - 1, fillStr (p, count, head ps, accum))

fillStr :: (Num a, Eq a, Show a) => (a, Int, a, String) -> String
fillStr (c, e, nxt, accum) = accum ++ case e of
                                  0 ->  case c of
                                        0 -> ""
                                        nnc -> show nnc
                                  1 ->  case c of
                                        0 -> ""
                                        1 -> "x"
                                        nnc -> show nnc ++ "x"
                                  nne -> case c of
                                          0 -> ""
                                          1 -> "x^" ++ show nne
                                          nnc -> show nnc ++ "x^" ++ show nne
                                   ++ case nxt of
                                       0 -> ""
                                       _ -> " + "

-- Function 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus left right = case (left, right) of
    (P [], P []) -> P []
    (P ls, P []) -> P ls
    (P [], P rs) -> P rs
    (P ls, P rs) -> P (addLists ls rs)

addLists :: Num a => [a] -> [a] -> [a]
addLists left right = case (left, right) of
    ([], []) -> []
    (ls, []) -> ls
    ([], rs) -> rs
    (l:ls, r:rs) -> (l + r) : addLists ls rs

-- Function 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times left right = case (left, right) of
    (P ls, P rs) -> P (multLst ls rs)

multLst :: Num a => [a] -> [a] -> [a]
multLst l r = multLstH l r [] 0
        where multLstH :: Num a => [a] -> [a] -> [a] -> Int -> [a]
              multLstH ls rs res cnt | cnt == length ls = res
                                     | otherwise = multLstH ls rs (addLists res (multWithMove cnt (ls !! cnt) rs)) (cnt + 1)

moveList :: Num a => Int -> [a] -> [a]
moveList 0 ls = ls
moveList pos ls = 0 : moveList (pos - 1) ls

multWithMove :: Num a => Int -> a -> [a] -> [a]
multWithMove pos val ls = map (* val) (moveList pos ls)

-- Function 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate poly     = case poly of
        P [] -> P []
        P ps -> P (map negate ps)
    fromInteger num = P [fromInteger num]
    -- Розумних означень не існує
    abs    = undefined
    signum = undefined

-- Function 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP poly val = case poly of
    P [] -> 0
    P [p] -> p
    P ps -> applyPH ps val 0
      where applyPH :: Num a => [a] -> a -> Integer -> a
            applyPH [] _ _ = 0
            applyPH ls vl cnt = head ls * (vl ^ cnt) + applyPH (tail ls) vl (cnt + 1)

-- Function 8 -----------------------------------------
class Num a => Differentiable a where
    derive  :: a -> a
    nderive :: Int -> a -> a
    nderive 0 val = val
    nderive cnt val = nderive (cnt- 1) (derive val)

-- Function 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    derive poly = case poly of
        P [] -> P []
        P lst -> P (calcDer lst)
             where calcDer :: Num a => [a] -> [a]
                   calcDer [] = []
                   calcDer ls = tail (calcDerH ls 0)
                                where calcDerH :: Num a => [a] -> a -> [a]
                                      calcDerH [] _ = []
                                      calcDerH ps cnt = (head ps * cnt) : calcDerH (tail ps) (cnt + 1)