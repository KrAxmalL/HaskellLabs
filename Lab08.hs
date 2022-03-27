{-# OPTIONS_GHC -Wall #-}
module Lab08 where

data BinTree a = EmptyB
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a
               | Node2 (Tree23 a) a (Tree23 a)
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23 
                   deriving (Eq, Show)

-- Function 1 -----------------------------------------			   
isSearch :: (Ord a) => BinTree a -> Bool
isSearch tr = case tr of
        EmptyB -> True
        Node _ EmptyB EmptyB -> True
        Node v EmptyB (Node r rl rr) -> (v < r) && isSearch (Node r rl rr)
        Node v (Node l ll lr) EmptyB -> (v > l) && isSearch (Node l ll lr)
        Node v (Node l ll lr) (Node r rl rr) -> (l < v) && (v < r) && isSearch (Node l ll lr) && isSearch(Node r rl rr)

-- Function 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch tr el = case tr of
        EmptyB -> False
        Node v l r -> (el == v) || (if el < v then elemSearch l el
                                    else elemSearch r el)

-- Function 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a
insSearch tr el = case tr of
        EmptyB -> Node el EmptyB EmptyB
        Node v l r -> if el == v then Node v l r
                      else if el < v then Node v (insSearch l el) r
                      else Node v l (insSearch r el)

-- Function 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a
delSearch tr el = case tr of
        EmptyB -> EmptyB
        Node v l r -> if el < v then Node v (delSearch l el) r
                      else if el > v then Node v l (delSearch r el)
                      else case (l, r) of
                              (EmptyB, EmptyB) -> EmptyB
                              (EmptyB, rt) -> rt
                              (lt, EmptyB) -> lt
                              (lt, rt) -> let mx = treeMax lt in Node mx (delSearch l mx) rt

treeMax :: (Ord a) => BinTree a -> a
treeMax tr = case tr of
        EmptyB -> error "can't find max value in empty tree"
        Node v _ EmptyB -> v
        Node _ _ r -> treeMax r

-- Function 5 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList ls = treeToList (foldl insSearch EmptyB ls) []

treeToList :: (Ord a) => BinTree a -> [a] -> [a]
treeToList tr ls = case tr of
        EmptyB -> ls
        Node v l r -> treeToList l ls ++ (v : treeToList r ls)

-- Function 6-----------------------------------------
isTree23 :: (Ord a) => Tree23 a -> Bool
isTree23 Empty23 = False
isTree23 (Leaf _) = True
isTree23 tr = isTree23H tr

isTree23H :: (Ord a) => Tree23 a -> Bool
isTree23H tr = case tr of
        Empty23 -> False
        Leaf _ -> True
        Node2 l v r -> correctNode2 (Node2 l v r)
        Node3 l lv m rv r -> correctNode3 (Node3 l lv m rv r)

correctNode2 :: (Ord a) => Tree23 a -> Bool
correctNode2 nd = case nd of
        Node2 l v r -> correctLeft v l && correctRight v r
        _ -> error "Not a node2!"
        where correctLeft :: (Ord a) => a -> Tree23 a -> Bool
              correctLeft v l =  case l of
                Empty23 -> False
                Leaf vv -> vv <= v
                Node2 ll vv rr -> isTree23H ll && (vv <= v) && isTree23H rr
                Node3 ll lv m rv rr -> isTree23H ll && (lv <= rv) && isTree23H m && (rv <= v) && isTree23H rr

              correctRight :: (Ord a) => a -> Tree23 a -> Bool
              correctRight v r = case r of
                Empty23 -> False
                Leaf vv -> vv == v
                Node2 ll vv rr -> isTree23H ll && (v <= vv) && isTree23H rr
                Node3 ll lv m rv rr -> isTree23H ll && (v <= lv) && isTree23H m && (lv <= rv) && isTree23H rr

correctNode3 :: (Ord a) => Tree23 a -> Bool
correctNode3 nd = case nd of
        Node3 l lv m rv r -> (lv <= rv) && crL lv l && crM lv rv m && crR rv r
        _ -> error "Not a node3!"
        where crL :: (Ord a) => a -> Tree23 a -> Bool
              crL v l = case l of
                Empty23 -> False
                Leaf vv -> vv <= v
                Node2 ll vv rr -> isTree23H ll && (vv <= v) && isTree23H rr
                Node3 ll lv m rv rr -> isTree23H ll && (lv <= rv) && isTree23H m && (rv <= v) && isTree23H rr
              crM :: (Ord a) => a -> a -> Tree23 a -> Bool
              crM lv rv l = case l of
                Empty23 -> False
                Leaf vv -> (lv == vv) && (vv <= rv)
                Node2 ll vv rr -> isTree23H ll && (lv <= vv) && (vv <= rv) && isTree23H rr
                Node3 ll llv m rrv rr -> isTree23H ll && (lv <= llv) && (llv <= rrv) && isTree23H m && (rrv <= rv) && isTree23H rr
              crR :: (Ord a) => a -> Tree23 a -> Bool
              crR v l = case l of
                Empty23 -> False
                Leaf vv -> vv == v
                Node2 ll vv rr -> isTree23H ll && (v <= vv) && isTree23H rr
                Node3 ll lv m rv rr -> isTree23H ll && (lv <= rv) && isTree23H m && (v <= lv) && isTree23H rr

-- Function 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 tr el = case tr of
        Empty23 -> False
        Leaf v -> el == v
        Node2 l _ r -> elemTree23 l el || elemTree23 r el
        Node3 l _ m _ r -> elemTree23 l el || elemTree23 m el || elemTree23 r el

-- Function 8-----------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 trl trr = tree23ToList trl [] == tree23ToList trr []

tree23ToList :: (Ord a) => Tree23 a -> [a] -> [a]
tree23ToList tr ls = case tr of
        Empty23 -> ls
        Leaf v -> v : ls
        Node2 l _ r -> tree23ToList l ls ++ tree23ToList r ls
        Node3 l _ m _ r -> tree23ToList l ls ++ tree23ToList m ls ++ tree23ToList r ls

-- Function 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 Empty23 el = Leaf el
insTree23 (Leaf v) el | el < v = Node2 (Leaf el) v (Leaf v)
                      | otherwise = Node2 (Leaf v) el (Leaf el)
insTree23 tr v = case insert v tr of
        (res, Nothing) -> res
        (trl, Just(val, trr)) -> Node2 trl val trr

isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm el tr = case tr of
        Node2 (Leaf l) v (Leaf r) -> if el < l then (Node3 (Leaf el) l (Leaf l) v (Leaf r), Nothing)
                                     else if el < v then (Node3 (Leaf l) el (Leaf el) v (Leaf r), Nothing)
                                     else (Node3 (Leaf l) v (Leaf r) el (Leaf el), Nothing)

        Node3 (Leaf l) lv (Leaf m) rv (Leaf r) -> if el < l then (Node2 (Leaf el) l (Leaf l), Just(lv, Node2 (Leaf m) rv (Leaf r)))
                                                  else if el < m then (Node2 (Leaf l) el (Leaf el), Just(lv, Node2 (Leaf m) rv (Leaf r)))
                                                  else if el < r then (Node2 (Leaf l) lv (Leaf m), Just(el, Node2 (Leaf el) rv (Leaf r)))
                                                  else (Node2 (Leaf l) lv (Leaf m), Just(rv, Node2 (Leaf r) el (Leaf el)))

        _ -> error "not a terminal!"
 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode el tr = case tr of
        Node2 l v r -> if el < v then case insert el l of
                (res, Nothing) -> (Node2 res v r, Nothing)
                (a, Just(w, b)) -> (Node3 a w b v r, Nothing)
                       else case insert el r of
                (res, Nothing) -> (Node2 l v res, Nothing)
                (a, Just(w, b)) -> (Node3 l v a w b, Nothing)

        Node3 l lv m rv r -> if el < lv then case insert el l of
                (res, Nothing) -> (Node3 res lv m rv r, Nothing)
                (a, Just(w, b)) -> (Node2 a w b, Just(lv, Node2 m rv r))
                             else if el < rv then case insert el m of
                (res, Nothing) -> (Node3 l lv res rv r, Nothing)
                (a, Just(w, b)) -> (Node2 l lv a, Just(w, Node2 b rv r))
                             else case insert el r of
                (res, Nothing) -> (Node3 l lv m rv res, Nothing)
                (a, Just(w, b)) -> (Node2 l lv m, Just(rv, Node2 a w b))

        _ -> error "not a node!"

bt1, bt2 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB)
                       EmptyB)

tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1))
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5))
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8))
            )            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )
