{-# OPTIONS_GHC -Wall #-}
module Lab07 where
import Data.Ratio
import Data.List (sort)

type PolinomOne = [(Int,Rational)]
type Linear   = [Row]
type Row      = [Rational]
data Solution = Empty | One Row  | Many [PolinomOne]
                 deriving (Show, Eq)

-- Function 1.a -----------------------------------------
coef :: Rational -> PolinomOne -> PolinomOne
coef c p0 = filter (\x -> snd x /= 0) (map (\x -> (fst x, snd x * c)) p0)

-- Function 1.b -----------------------------------------
add :: PolinomOne -> PolinomOne -> PolinomOne
add p1 p2 = filter (\x -> snd x /= 0) (doAdd p1 p2 [])
       where doAdd :: PolinomOne -> PolinomOne -> PolinomOne -> PolinomOne
             doAdd lp [] ac = ac ++ lp
             doAdd [] rp ac = ac ++ rp
             doAdd (l:lp) (r:rp) ac | fst l < fst r =  doAdd lp (r:rp) (ac ++ [l])
                                    | fst l == fst r = doAdd lp rp (ac ++ [(fst l, snd l + snd r)])
                                    | otherwise = doAdd (l:lp) rp (ac ++ [r])

-- Function 1.c -----------------------------------------
unify :: PolinomOne -> PolinomOne
unify [] = []
unify p = filter (\x -> snd x /= 0) (let sr = sort p in reverse (sumSame sr []))
      where sumSame :: PolinomOne -> PolinomOne -> PolinomOne
            sumSame [] ac = ac
            sumSame (nx:pol) [] = sumSame pol [nx]
            sumSame (nx:pol) (pr:ac) | fst nx == fst pr = sumSame pol ((fst nx, snd nx + snd pr) : ac)
                                     | otherwise = sumSame pol (nx : (pr : ac))

-- Function 2.a -----------------------------------------
findFree :: [PolinomOne] -> [Int]
findFree res = findFreeH res 1 []
      where findFreeH :: [PolinomOne] -> Int -> [Int] -> [Int]
            findFreeH [] _ ac = ac
            findFreeH (pl:rs) cn ac | isFree pl = findFreeH rs (cn + 1) (ac ++ [cn])
                                    | otherwise = findFreeH rs (cn + 1) ac

            isFree :: PolinomOne -> Bool
            isFree poly = case poly of
                  [x] -> fst x /= 0 && snd x == 1
                  _ -> False

-- Function 2.b -----------------------------------------
iswfCommon ::  [PolinomOne]  -> Bool
iswfCommon res = iswfCommonH res (findFree res)
           where iswfCommonH :: [PolinomOne] -> [Int] -> Bool
                 iswfCommonH [] [] = True
                 iswfCommonH _ [] = False
                 iswfCommonH [] _ = False
                 iswfCommonH rs fr = null (filter (\poly -> not (isCorrectPoly poly fr)) rs)

                 isCorrectPoly :: PolinomOne -> [Int] -> Bool
                 isCorrectPoly poly fr = null (filter (\x -> (fst x /= 0) && notElem (fst x) fr) poly)

-- Function 3.a -----------------------------------------
isSimple :: Linear -> Bool
isSimple lin = null (filter (\row -> length row /= 1) lin)

-- Function 3.b -----------------------------------------
solveSimple :: Linear -> Maybe [PolinomOne]
solveSimple lin | not (isSimple lin) = Nothing
                | null (filter (\x -> head x /= 0) lin) = Just []
                | otherwise = Nothing

-- Function 4.a -----------------------------------------
findRow :: Linear -> Maybe Int
findRow lin = findRowH lin 1
        where findRowH :: Linear -> Int -> Maybe Int
              findRowH ln cn | null ln = Nothing
                             | null (head ln) = Nothing
                             | head (head ln) /= 0 = Just cn
                             | otherwise = findRowH (tail ln) (cn + 1)

-- Function 4.b -----------------------------------------
exchangeRow :: [a] -> Int -> [a]
exchangeRow [] _ = error "Empty list, nothing to exchange"
exchangeRow le ind = if ind > length le
                     then error "Wrong index!"
                     else exchRowH (tail le) ind 2 (head le) [le !! (ind - 1)]
                    where exchRowH :: [a] -> Int -> Int -> a -> [a] -> [a]
                          exchRowH [] _ _ _ res = res
                          exchRowH (l:ls) pos cn toIns res | cn == pos = res ++ (toIns : ls)
                                                           | otherwise = exchRowH ls pos (cn+1) toIns (res ++ [l])

-- Function 5.a -----------------------------------------
forwardStep :: Row -> Linear -> Linear
forwardStep row = map (reduceWithRow row)
      where reduceWithRow :: Row -> Row -> Row
            reduceWithRow _ [] = []
            reduceWithRow [] _ = []
            reduceWithRow ls rs = reduceWithRowH (tail ls) (tail rs) (head rs / head ls) []
            reduceWithRowH :: Row -> Row -> Rational -> Row -> Row
            reduceWithRowH [] _ _ ac = ac
            reduceWithRowH _ [] _ ac = ac
            reduceWithRowH (l:ls) (r:rs) cf ac = reduceWithRowH ls rs cf (ac ++ [r - (l * cf)])

-- Function 5.b -----------------------------------------
reverseStep :: Row -> [PolinomOne] -> [PolinomOne]
reverseStep fs vs = findPoly (tail (map (/ negate(head fs)) fs)) vs : vs
              where findPoly :: Row -> [PolinomOne] -> PolinomOne
                    findPoly [] [] = []
                    findPoly rw pl = unify (findPolyH rw pl [])
                    findPolyH :: Row -> [PolinomOne] -> PolinomOne -> PolinomOne
                    findPolyH [] _ ac = ac
                    findPolyH curRw [] ac = ac ++ [(0, negate (head curRw))]
                    findPolyH (num:curRw) (pol:curPol) ac = findPolyH curRw curPol (ac ++ coef num pol)

-- Function 6 -----------------------------------------
gauss :: Int -> Linear -> Maybe [PolinomOne]
gauss i le = case goDown [] le i of
      (Nothing, _, _) -> Nothing
      (Just polys, linAc, cn) -> Just (goUp linAc polys cn)

goDown :: [Linear] -> Linear -> Int -> (Maybe [PolinomOne], [Linear], Int)
goDown linAc sys cn | isSimple sys = case solveSimple sys of
                                      Nothing -> (Nothing, linAc, cn - 1)
                                      Just poly -> (Just poly, linAc, cn - 1)
                    | otherwise = goDown (sys : linAc) (exFrwStep sys) (cn + 1)

goUp :: [Linear] -> [PolinomOne] -> Int -> [PolinomOne]
goUp linAc polys cn | null linAc = polys
                    | otherwise = goUp (tail linAc) (exRevStep (head linAc) polys cn) (cn - 1)

exFrwStep :: Linear -> Linear
exFrwStep sys = case findRow sys of
       Nothing -> map tail sys --variant 2
       Just x -> let ready = exchangeRow sys x in forwardStep (head ready) (tail ready) --variant 1

exRevStep :: Linear -> [PolinomOne] -> Int -> [PolinomOne]
exRevStep sys polys cn = case findRow sys of
        Nothing -> [(cn, 1)] : polys --variant 2
        Just x -> let ready = exchangeRow sys x in reverseStep (head ready) polys --variant 1

-- Function 7.a -----------------------------------------
testEquation :: [PolinomOne] -> Row -> Bool
testEquation pos rw = testH pos rw [] (last rw)
                where testH :: [PolinomOne] -> Row -> PolinomOne -> Rational -> Bool
                      testH [] [] res finB = (length res == 1) && (fst (head res) == 0) && (snd (head res) == finB)
                      testH poly [_] res finB = testH poly [] (unify res) finB
                      testH (pl:poly) (cf:row) res finB = testH poly row (res ++ map (\el -> (fst el, snd el * cf)) pl) finB
                      testH [] _ _ _ = False -- error - not the same amount of polynoms and variables
                      testH _ [] _ _ = False -- error - not the same amount of polynoms and variables

-- Function 7.b -----------------------------------------
testLinear :: [PolinomOne] -> Linear -> Bool
testLinear pos le = null (filter (== False) (map (testEquation pos) le))

-- Function 8 -----------------------------------------
solving :: Linear -> Solution
solving le =  case gauss 1 (dummy le : le) of
      Nothing -> Empty
      Just polLs -> if null (filter (not . null . filter (\pr -> fst pr /= 0)) polLs)
                    then One (map (snd . head) polLs)
                    else Many polLs

dummy :: Linear -> Row
dummy sys = [0 % 1 | _ <- head sys]

-------------------------------------------------------
pol0, pol1, pol2, pol3, pol4 :: PolinomOne
pol0 = [(0,3/5), (3,1), (3,-2/7), (2,3), (0,-7/3), (4,0)]
pol1 = [(5,3/4), (0,7), (4,3/2), (5,-2/3), (0,1/2)]
pol2 = [(0,15), (4,3),(5,1)]
pol3 = [(0,-10), (2,7), (4,-3)]
pol4 = [(0,-26/15), (2,3), (3,5/7)]

test0, test1, test2, test3, test3a, test4 :: Linear
test0 = [[0,-2,-1,2],[0,-4,-5,3],[1,2,4,5]]
test1 = [[4,-3,2,-1,8],[3,-2,1,-3,7],[5,-3,1,-8,1]]
test2 = [[7,-2,-1,2],[6,-4,-5,3],[1,2,4,5]]
test3 = [[2,3,-1,1,1],[8,12,-9,8,3],[4,6,3,-2,3],[2,3,9,-7,3]]
test3a = [[0,-5,4,-1], [0,5,-4,1],[0,10,-8,2]]
test4 = [[6,1,2,21], [4,-6,16,2], [3,8,1,2]]

res3, res4 :: [PolinomOne]
res3 = [[(0,3/5),(2,-3/2),(4,-1/10)],[(2,1)],[(0,1/5),(4,4/5)],[(4,1)]]
res4 = [[(0,62/15)], [(0,-17/15)], [(0,-4/3)]]

sol1,sol2,sol3,sol4 :: Solution
sol1 = Empty
sol2 = Empty
sol3 = Many res3
sol4 = One [62/15, -17/15 -4/3]