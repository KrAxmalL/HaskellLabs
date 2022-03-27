{-# OPTIONS_GHC -Wall #-}
module Lab03 where

type Graph  = [[Int]]

-- Function 1 ------------------------------------
isGraph :: Graph -> Bool
isGraph [] = False
isGraph gr = and (map isSet gr) && and (map (and . map (\neighbour -> neighbour < length gr)) gr)

-- Function 2 ------------------------------------
isTournament :: Graph -> Bool
isTournament gr | not (isGraph gr) = False
                | otherwise = isTournTailRec gr 0 (nodes gr)

isTournTailRec :: Graph -> Int -> [Int] -> Bool
isTournTailRec gr currVert verticles | currVert == length gr = True
                                     | connectedWithAll gr currVert (filter (/= currVert) verticles) = isTournTailRec gr (currVert + 1) verticles
                                     | otherwise = False

connectedWithAll :: Graph -> Int -> [Int] -> Bool
connectedWithAll gr vert verts | null verts = True
                               | edgeIsUnique gr vert (head verts) || edgeIsUnique gr (head verts) vert = connectedWithAll gr vert (tail verts)
                               | otherwise = False

edgeIsUnique :: Graph -> Int -> Int -> Bool
edgeIsUnique gr vert1 vert2 = edgeIn gr (vert1, vert2) && not (edgeIn gr (vert2, vert1))

-- Function 3 ------------------------------------
isTransitive :: Graph -> Bool
isTransitive gr | not (isGraph gr) = False
                | otherwise = isTransTailRec gr [[x, y, z] | x <- nodes gr, y <- filter (/= x) (nodes gr), z <- nodes gr]

isTransTailRec :: Graph -> [[Int]] -> Bool
isTransTailRec gr allTriplets | null allTriplets = True
                              | isTransitiveTriplet gr (head allTriplets) = isTransTailRec gr (tail allTriplets)
                              | otherwise = False

isTransitiveTriplet :: Graph -> [Int] -> Bool
isTransitiveTriplet gr verts | edgeIn gr (head verts, verts !! 1) && edgeIn gr (verts !! 1, verts !! 2) = edgeIn gr (head verts, verts !! 2)
                             | otherwise = True

-- Function 4 ------------------------------------
buildTransitive :: Graph -> Graph
buildTransitive gr | not (isGraph gr) = []
                   | otherwise = createGraph gr [] 0

createGraph :: Graph -> Graph -> Int -> Graph
createGraph grs grf vert | length grs <= vert = reverse grf
                         | otherwise = createGraph grs (findTransVerts grs (grs !! vert) : grf) (vert + 1)

findTransVerts :: Graph -> [Int] -> [Int]
findTransVerts gr verts = sortInsert (set (snd (findAllVertsForCur gr (verts, verts))))

findAllVertsForCur :: Graph -> ([Int], [Int]) -> ([Int], [Int])
findAllVertsForCur gr verts | null (fst verts) = verts
                            | otherwise = findAllVertsForCur gr (listOfTrans gr (snd verts), snd verts ++ listOfTrans gr (snd verts))

listOfTrans :: Graph -> [Int] -> [Int]
listOfTrans gr verts = [vertToAdd | secondVert <- verts, vertToAdd <- gr !! secondVert, vertToAdd `notElem` verts]

-- Function 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr a b | not (isGraph gr) = Nothing
               | otherwise = longWayTailRecursive (allWays gr a) b

longWayTailRecursive :: [[[Int]]] -> Int -> Maybe [Int]
longWayTailRecursive [] _ = Nothing
longWayTailRecursive ways b | findCorrectWay (head ways) b /= Nothing = findCorrectWay (head ways) b
                            | otherwise = longWayTailRecursive (tail ways) b

findCorrectWay :: [[Int]] -> Int -> Maybe [Int]
findCorrectWay [] _ = Nothing
findCorrectWay ways b | not (isSet (head ways)) = findCorrectWay (tail ways) b
                      | length (head ways) < 2 = findCorrectWay (tail ways) b
                      | b == head (head ways) = Just (reverseList (head ways))
                      | otherwise = findCorrectWay (tail ways) b

-- Function 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr | not (isGraph gr) = Nothing
               | otherwise = findGamiltonWay gr (allWays gr 0 !! 1)

findGamiltonWay :: Graph -> [[Int]] -> Maybe [Int]
findGamiltonWay _ [] = Nothing
findGamiltonWay gr (w:ways) | length w < 2 = findGamiltonWay gr ways
                            | head w == last w && not (any (\vert -> notElem vert w) (nodes gr)) = Just (reverseList w)
                            | otherwise = findGamiltonWay gr ways

-- Function 7 ------------------------------------
isAcyclic :: Graph -> Bool
isAcyclic gr | not (isGraph gr) = False
             | otherwise = isAcyclicTailRec (allWays gr 0) gr 0

isAcyclicTailRec :: [[[Int]]] -> Graph -> Int -> Bool
isAcyclicTailRec ways gr position | position >= length ways = True
                                  | pathIsNotCycle (head ways) gr = isAcyclicTailRec (tail ways) gr (position + 1)
                                  | otherwise = False

pathIsNotCycle :: [[Int]] -> Graph -> Bool
pathIsNotCycle [] _ = True
pathIsNotCycle _ [] = True
pathIsNotCycle ways gr | not (isSet (head ways)) || edgeIn gr (head (head ways), last (head ways)) = False
                       | otherwise = pathIsNotCycle (tail ways) gr

-- Function 8 ------------------------------------
topolSort :: Graph -> Maybe [Int]
topolSort gr | not (isGraph gr) || not (isAcyclic gr) = Nothing
             | otherwise = Just (myTopolSort gr (filter (\vert -> hasZeroIndegree gr vert (nodes gr)) (nodes gr)))

myTopolSort :: Graph -> [Int] -> [Int]
myTopolSort gr stack | length gr <= length stack = stack
                     | otherwise = myTopolSort (replAtPositions gr [] (findCorVerts gr)) (set (stack ++ findCorVerts gr))

hasZeroIndegree :: Graph -> Int -> [Int] -> Bool
hasZeroIndegree gr vert verticles | null verticles = True
                                  | edgeIn gr (head verticles, vert) = False
                                  | otherwise = hasZeroIndegree gr vert (tail verticles)

findCorVerts :: Graph -> [Int]
findCorVerts gr = filter (\vert -> hasZeroIndegree gr vert (nodes gr)) (nodes gr)

replAtPositions :: Graph -> [Int] -> [Int] -> Graph
replAtPositions gr _ [] = gr
replAtPositions gr newElem positions | null positions = gr
                                     | otherwise = replAtPositions (replaceElementAt (head positions) newElem gr) newElem (tail positions)

-- Function 9 ------------------------------------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort [] _ = False
isTopolSort _ [] = False
isTopolSort gr topol | not (isGraph gr) || not (isAcyclic gr) = False
                     | length gr /= length topol = False
                     | not (isSet topol) = False
                     | otherwise = checkIsSorted gr topol

checkIsSorted :: Graph -> [Int] -> Bool
checkIsSorted [[]] [x] = x == 0 --check for one element
checkIsSorted gr topol | null topol = True
                       | checkEdges gr (head topol) (tail topol) = checkIsSorted gr (tail topol)
                       | otherwise = False

checkEdges :: Graph -> Int -> [Int] -> Bool
checkEdges gr vert topol | null topol = True
                         | edgeIn gr (head topol, vert) = False
                         | otherwise = checkEdges gr vert (tail topol)

--------------------- Graphs -------

gr1, gr2, gr3, gr4:: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]

------ Helpful functions(graphs) ----

adj :: Graph -> Int -> [Int]
adj g v = g !! v

nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

edgeIn :: Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = y `elem` (g!!x)

edges :: Graph -> [(Int,Int)]
edges g = [(x,y) | x<-nodes g, y <- g!!x]

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: [[[Int]]] -> Bool
condW wss = null ( head wss)

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) =
  [t:w | w@(x:xs) <- wsn, x `notElem` xs, t<- gr!!x] : wss
stepW _ []  = error "allWays:stepW"

------ Helpful functions(lists) ------

set :: [Int] -> [Int]
set [] = []
set (x:xs) = x : set (filter(/= x) xs)

isSet :: [Int] -> Bool
isSet [] = True
isSet [_] = True
isSet (x:xs) = notElem x xs && isSet xs

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

sortInsert :: [Int] -> [Int]
sortInsert [] = []
sortInsert [x] = [x]
sortInsert (x:xs) = insert (sortInsert xs) x
          where insert :: [Int] -> Int -> [Int]
                insert [] n = [n]
                insert ys n | n > head ys = head ys : insert (tail ys) n
                            | otherwise = n : ys

replaceElementAt :: Int -> a -> [a] -> [a]
replaceElementAt pos newElem ls | pos >= length ls = error "Index is too big!"
                                | otherwise = repElemHelp pos newElem ls 0
                                where repElemHelp :: Int -> a -> [a] -> Int -> [a]
                                      repElemHelp _ _ [] _ = []
                                      repElemHelp position element (x:xs) counter | counter == position = element : xs
                                                                                  | otherwise = x : repElemHelp position element xs (counter + 1)
