{-# OPTIONS_GHC -Wall #-}
module Pinkevych04 where

import Data.List(nub,sort)

data Going = No | Lt | Rt deriving (Show,Eq,Ord)
type Table = [((Int,Char),(Int,Char,Going))]
type Machine = (Int, Table)
type Config = (String,(Int,Char), String, Int)
 
data Complex = Join [Complex]
             | While Char Complex
             | If Char Complex Complex
             | P Char
             | L | R | G
             deriving (Show, Eq)

-- Function 1.a -----------------------------------------
alphabet :: Table -> [Char]
alphabet [] = []
alphabet go = sort (nub (findAllSymbols go))
                    where findAllSymbols :: Table -> [Char]
                          findAllSymbols [] = []
                          findAllSymbols tb = snd (fst (head tb)) : findAllSymbols (tail tb)

-- Function 1.b ----------------------------------------- 
states :: Machine -> [Int]
states m = sort (nub (fst m : findAllStates (snd m)))
                 where findAllStates :: Table -> [Int]
                       findAllStates [] = []
                       findAllStates tb = fst (fst (head tb)) : findAllStates (tail tb)

-- Function 2 -----------------------------------------
iswfMachine :: Machine -> Bool
iswfMachine m = correctFirstState m && null (filter (<= 0) (states m)) && statesCorrect m

correctFirstState :: Machine -> Bool
correctFirstState mc = null (filter (\key -> fst mc < fst key) (getKeys mc))

statesCorrect :: Machine -> Bool
statesCorrect mc = equalLists (sort (getKeys mc)) ([(st,ch) | st <- states mc, ch <- alphabet (snd mc)])

equalLists :: [(Int,Char)] -> [(Int,Char)] -> Bool
equalLists [] [] = True
equalLists _ [] = False
equalLists [] _ = False
equalLists (el1: ls1) (el2: ls2) | (fst el1 == fst el2) && (snd el1 == snd el2) = equalLists ls1 ls2
                                 | otherwise = False

getKeys :: Machine -> [(Int, Char)]
getKeys mc = getKeysFromTable (snd mc)
       where getKeysFromTable :: Table -> [(Int, Char)]
             getKeysFromTable [] = []
             getKeysFromTable tb = fst (head tb) : getKeysFromTable (tail tb)

-- Function 3.a -----------------------------------------
isFinal ::  Int -> Config -> Bool
isFinal mx cnf = (mx == getFrtF cnf) || (mx > getFrtF cnf && fst (getSndF cnf) == 0)

-- Function 3.b -----------------------------------------
stepM :: Machine -> Config -> Config
stepM m cnf | fst (getSndF cnf) == 0 = cnf
            | otherwise = performStep cnf (findState m (getSndF cnf))
                    where performStep :: Config -> (Int, Char, Going) -> Config
                          performStep conf transf | getTrdT transf == Lt = performLeftStep conf transf
                                                  | getTrdT transf == Rt = performRightStep conf transf
                                                  | otherwise = (getFstF conf, (getFstT transf, getSndT transf), getTrdF conf, getFrtF conf + 1)

performLeftStep :: Config -> (Int, Char, Going) -> Config
performLeftStep conf transf = ((\str -> if null str then str else init str) (getFstF conf), --get new left word
                              (getFstT transf, (\str -> if null str then ' ' else last str) (getFstF conf)), --new current state
                              getSndT transf : getTrdF conf, getFrtF conf + 1) --get new right word and incr num of steps

performRightStep :: Config -> (Int, Char, Going) -> Config
performRightStep conf transf = (getFstF conf ++ [getSndT transf], --get new left word
                               (getFstT transf, getProperChar (getTrdF conf)), --new current state
                               getProperStr (getTrdF conf), getFrtF conf + 1) --get new right word and incr num of steps

getProperChar :: [Char] -> Char
getProperChar [] = ' '
getProperChar [x] = x
getProperChar (ch:_) = ch

getProperStr :: [Char] -> [Char]
getProperStr [] = []
getProperStr [_] = []
getProperStr str = tail str

findState :: Machine -> (Int, Char) -> (Int, Char, Going)
findState m key = getStep (filter (\currSt -> fst currSt == key) (snd m))
            where getStep :: Table -> (Int, Char, Going)
                  getStep [] = error "State is not present in machine!"
                  getStep tb = snd (head tb)

-- Function 4 -----------------------------------------
eval :: Machine -> Int -> String -> Maybe String
eval m mx u = process m mx (initCon m u)

process :: Machine -> Int -> Config -> Maybe String
process m mx cnf | isFinal mx cnf = if fst (getSndF cnf) == 0 then Just (clearStr (getFstF cnf ++ (snd (getSndF cnf) : getTrdF cnf))) else Nothing
                 | otherwise = process m mx (stepM m cnf)

initCon :: Machine -> String -> Config
initCon (is,_) ""     = ("", (is,' '), "", 0)
initCon (is,_) (c:cx) = ("", (is, c), cx, 0)

-- Function 5.a -----------------------------------------
renum :: Int -> Machine -> Machine
renum k m = (fst m + k, map (\pair -> (transfKey k (fst pair), transfVal k (snd pair))) (snd m))
                  where transfKey :: Int -> (Int, Char) -> (Int, Char)
                        transfKey incr key = (fst key + incr, snd key)
                        transfVal :: Int -> (Int, Char, Going) -> (Int, Char, Going)
                        transfVal incr val = (if getFstT val == 0 then 0 else getFstT val + incr, getSndT val, getTrdT val)

-- Function 5.b -----------------------------------------
connect :: Int -> Table -> Table
connect p = map (\pair -> (fst pair, transfVal p (snd pair)))
                  where transfVal :: Int -> (Int, Char, Going) -> (Int, Char, Going)
                        transfVal incr val = (if getFstT val == 0 then incr else getFstT val, getSndT val, getTrdT val)

-- Function 6.a -----------------------------------------
seqJoin :: Machine -> Machine -> Machine
seqJoin m1 m2 = (fst (renum (fst m2) m1), sort (connect (fst m2) (snd (renum (fst m2) m1)) ++ snd m2))

-- Function 6.b -----------------------------------------
ifJoin :: Char -> String -> Machine -> Machine -> Machine
ifJoin c alf m1 m2 = completeTable c alf m2 (fst (renum (fst m2) m1) + 1, snd (renum (fst m2) m1))
               where completeTable :: Char -> String -> Machine -> Machine -> Machine
                     completeTable ch alfb mc2 preReady = (fst preReady,
                                                           sort (snd preReady
                                                           ++ snd mc2
                                                           ++ [((fst preReady,s), (if s==ch then fst preReady - 1 else fst mc2,s,No))  | s<-alfb ]))

-- Function 6.c -----------------------------------------
cycleJoin :: Char -> String -> Machine -> Machine
cycleJoin c alf m = completeTable c alf (fst m + 1, connect (fst m + 1) (snd m))
              where completeTable :: Char -> String -> Machine -> Machine
                    completeTable ch alfb preReady = (fst preReady,
                                                      sort (snd preReady
                                                      ++ [((fst preReady, s), (if s==ch then fst preReady - 1 else 0,s,No))  | s <- alfb]))

-- Function 7 -----------------------------------------
build ::  String -> Complex -> Machine
build alf L = (1, map (\ch -> ((1, ch), (0, ch, Lt))) alf)
build alf R = (1, map (\ch -> ((1, ch), (0, ch, Rt))) alf)
build alf G = (1, map (\ch -> ((1, ch), (0, ch, No))) alf)
build alf (P c) = (1, map (\ch -> ((1, ch), (0, c, No))) alf)
build alf (If ch cm1 cm2) = ifJoin ch alf (build alf cm1) (build alf cm2)
build alf (While ch cm) = cycleJoin ch alf (build alf cm)
build alf (Join cmls) = foldl1 seqJoin (map (build alf) cmls)

-- Function 8.a -----------------------------------------
subtractAbs :: Complex
subtractAbs = If '#' (Join [P ' ', R])
              (Join [While '|' (Join [right, R, If ' ' (Join [L, P ' '])
              (Join [right, L, P ' ', left, L, If ' ' (Join [R, P ' '])
              (Join [left, R, P ' ', R, If '#' (P ' ') G])])])])

-- Function 8.b -----------------------------------------
subtraction :: Complex
subtraction = Join [While '|' (Join [right, right, L, If '#' (P ' ') (Join [P ' ', left, L, left, R, P ' ', R])]),
                    P ' ', R, If ' ' G (Join[right, While ' ' R])]

--------------------------------------------------------
test1, test2 :: Machine

test1 = (1, [ ((1,'a'),(0,'b',No)), ((1,'b'),(1,'b',Rt))
            , ((1,'c'),(1,'c',Rt)), ((1,' '),(1,' ',Rt))])

test2 = (2,[((2,'a'),(2,'a',Rt)),((2,' '),(1,' ',Rt)),((1,' '),(0,' ',Rt))])


rht, putO, putW :: Machine
rht  = (1, map (\c->((1,c),(0,c,Rt))) " #|")
putO = (1, map (\c->((1,c),(0,'|',No))) " #|")
putW = (1, map (\c->((1,c),(0,' ',No))) " #|")

rightO, rightM, main, additionM :: Machine
rightO = cycleJoin '|' " #|" rht
rightM = seqJoin rht rightO 
main   = seqJoin (seqJoin putW rightM) putO
additionM = ifJoin '|' " #|" main putW

right, left, copy, addition :: Complex

right = Join [R,While '|' R]

left  = Join [L,While '|' L]

addition = If '|' (Join [P ' ',right,P '|']) (P ' ')

copy = Join [While '|' (Join [P ' ',right,right,P '|',left,left,P '|',R])
            ,Join [left,R]
            ]

rightOT, rightMT, mainT, additionMT :: Machine
rightOT = (2,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))])
rightMT = (3,
  [((1,' '),(2,' ',Rt)),((1,'#'),(2,'#',Rt)),((1,'|'),(2,'|',Rt))
  ,((2,' '),(0,' ',No)),((2,'#'),(0,'#',No)),((2,'|'),(1,'|',No))
  ,((3,' '),(2,' ',Rt)),((3,'#'),(2,'#',Rt)),((3,'|'),(2,'|',Rt))])
mainT = (5,
  [((1,' '),(0,'|',No)),((1,'#'),(0,'|',No)),((1,'|'),(0,'|',No))
  ,((2,' '),(3,' ',Rt)),((2,'#'),(3,'#',Rt)),((2,'|'),(3,'|',Rt))
  ,((3,' '),(1,' ',No)),((3,'#'),(1,'#',No)),((3,'|'),(2,'|',No))
  ,((4,' '),(3,' ',Rt)),((4,'#'),(3,'#',Rt)),((4,'|'),(3,'|',Rt))
  ,((5,' '),(4,' ',No)),((5,'#'),(4,' ',No)),((5,'|'),(4,' ',No))])
additionMT = (7,
  [((1,' '),(0,' ',No)),((1,'#'),(0,' ',No)),((1,'|'),(0,' ',No))
  ,((2,' '),(0,'|',No)),((2,'#'),(0,'|',No)),((2,'|'),(0,'|',No))
  ,((3,' '),(4,' ',Rt)),((3,'#'),(4,'#',Rt)),((3,'|'),(4,'|',Rt))
  ,((4,' '),(2,' ',No)),((4,'#'),(2,'#',No)),((4,'|'),(3,'|',No))
  ,((5,' '),(4,' ',Rt)),((5,'#'),(4,'#',Rt)),((5,'|'),(4,'|',Rt))
  ,((6,' '),(5,' ',No)),((6,'#'),(5,' ',No)),((6,'|'),(5,' ',No))
  ,((7,' '),(1,' ',No)),((7,'#'),(1,'#',No)),((7,'|'),(6,'|',No))])

getFstT :: (a, b, c) -> a
getFstT (el, _, _) = el

getSndT :: (a, b, c) -> b
getSndT (_, el, _) = el

getTrdT :: (a, b, c) -> c
getTrdT (_, _, el) = el

getFstF :: (a, b, c, d) -> a
getFstF (el, _, _, _) = el

getSndF :: (a, b, c, d) -> b
getSndF (_, el, _, _) = el

getTrdF :: (a, b, c, d) -> c
getTrdF (_, _, el, _) = el

getFrtF :: (a, b, c, d) -> d
getFrtF (_, _, _, el) = el

-- removes spaces from left and right parts of string -----
clearStr :: [Char] -> [Char]
clearStr [] = []
clearStr [ch] = [ch]
clearStr str = clearStrR (clearStrL str)
         where clearStrL :: [Char] -> [Char]
               clearStrL [] = []
               clearStrL [x] = [x | x /= ' ']
               clearStrL (ch:chs) | ch /= ' ' = ch:chs
                                  | otherwise = clearStrL chs
               clearStrR :: [Char] -> [Char]
               clearStrR [] = []
               clearStrR [x] = [x | x /= ' ']
               clearStrR chs | last chs /= ' ' = chs
                             | otherwise = clearStrR (init chs)
