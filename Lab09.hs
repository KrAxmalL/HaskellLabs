{-# OPTIONS_GHC -Wall #-}
module Lab09 where
        
import Text.ParserCombinators.Parsec
import Data.Char (isDigit)
import Data.List (nub)

data Term   =  Nmb Int       
            | Var String  
            | App Term Term
            | Abs String Term
           deriving (Show, Eq)
type Contex = [(String,Term)]

-- Function 1.a -----------------------------------------
addVar :: String -> [String] -> [String]
addVar v vs = if elem v vs then vs else v : vs

-- Function 1.b ----------------------------------------- 
delVar :: String -> [String] -> [String]
delVar v = filter (/= v)

-- Function 1.c -----------------------------------------
unionV :: [String] -> [String] -> [String]
unionV vs1 vs2 = nub (vs1 ++ vs2)

-- Function 1.d ----------------------------------------- 
freeVars :: Term -> [String]
freeVars t = case t of
  Nmb _ -> []
  Var s -> [s]
  App tm1 tm2 -> unionV (freeVars tm1) (freeVars tm2)
  Abs s tm -> delVar s (freeVars tm)

-- Function 2.a -----------------------------------------
deleteSyn :: String -> Contex -> Contex
deleteSyn nm = filter (\pr -> fst pr /= nm)

-- Function 2.b -----------------------------------------
iswfTerm :: Term -> Contex -> Bool
iswfTerm t cnt = identsExist (freeVars t) (map fst cnt)
           where identsExist :: [String] -> [String] -> Bool
                 identsExist vars ids = all (\e -> elem e ids) vars

-- Function 2.c -----------------------------------------
iswfContex :: Contex -> Bool
iswfContex [] = True
iswfContex cnt = iswfContexH cnt (head cnt) []

iswfContexH :: Contex -> (String, Term) -> Contex -> Bool
iswfContexH [] t cntF = iswfTerm (snd t) cntF
iswfContexH cntS t cntF = iswfTerm (snd t) cntF && iswfContexH (tail cntS) (head cntS) (t : cntF)

-- Function 3.a -----------------------------------------
isNumber :: Term -> Bool
isNumber t = case t of
        Abs ap (Abs lst tr) -> (ap /= lst) && isNumTail ap lst tr
        _ -> False

isNumTail :: String -> String -> Term -> Bool
isNumTail ap lst t = case t of
        App (Var v) tr -> (ap == v) && isNumTail ap lst tr
        Var v -> lst == v
        _ -> False

-- Function 3.b -----------------------------------------
inNumber :: Term -> Term
inNumber t | isNumber t = termToInt t 0
           | otherwise = error "Not a number in inNumber!"

termToInt :: Term -> Int -> Term
termToInt t cn = case t of
        Abs _ (Abs _ tr) -> termToInt tr cn
        App (Var _) tr -> termToInt tr (cn + 1)
        Var _ -> Nmb cn
        _ -> error "Not a number in termToInt"

-- Function 3.c -----------------------------------------
compress ::  Term -> Term
compress t | isNumber t = inNumber t
           | otherwise = getTerm t

getTerm :: Term -> Term
getTerm t = case t of
  Nmb _ -> t
  Var _ -> t
  App tm1 tm2 -> App (compress tm1) (compress tm2)
  Abs s tm -> Abs s (compress tm)

-- Function 4 -----------------------------------------
reduce :: Term -> String -> Term -> Term
reduce w x s = case w of
        Nmb _ -> w
        Var v -> if v == x then s else w
        App t1 t2 -> App (reduce t1 x s) (reduce t2 x s)
        Abs y t -> absReduce y t x s

absReduce :: String -> Term -> String -> Term -> Term
absReduce y t x s | y == x = Abs y t
                  | (y /= x) && notElem y (freeVars s) = Abs y (reduce t x s)
                  | otherwise = let z = newVar (unionV (freeVars s) (freeVars t)) y in Abs z (reduce (reduce t y (Var z)) x s)

-- Function 5 -----------------------------------------
evalStep :: Term -> Contex -> Maybe Term
evalStep t cnt = case t of
  Nmb n -> Just (integerTerm n)
  Var s -> case findSyn s cnt of
          Just tm -> Just (reduce t s tm)
          Nothing -> Nothing
  App tm1 tm2 -> case evalStep tm1 cnt of
          Just tmr1 -> Just (App tmr1 tm2)
          Nothing -> case evalStep tm2 cnt of
                  Just tmr2 -> Just (App tm1 tmr2)
                  Nothing -> case tm1 of
                          Abs s tma -> Just (reduce tma s tm2)
                          _ -> Nothing
  Abs s tm -> case evalStep tm (deleteSyn s cnt) of
          Just tmr -> Just (Abs s tmr)
          Nothing -> Nothing

findSyn :: String -> Contex -> Maybe Term
findSyn _ [] = Nothing
findSyn v (t:cnt) | v == fst t = Just (snd t)
                  | otherwise = findSyn v cnt

-- Function 6 -----------------------------------------
eval :: Int -> Term -> Contex -> Maybe Term
eval st t ctx = case evalStep t ctx of
                Just tn -> eval (st - 1) tn ctx
                Nothing -> if st > 0 then Just (compress t) else Nothing

-- Function 7 -----------------------------------------
parseTerm :: String -> Maybe Term
parseTerm str = case parse expr "" str of
  Left _ -> Nothing
  Right t -> Just t

func :: Parser Term
func = do _ <- slash
          _ <- spaces
          idLs <- allIds
          _ <- dot
          _ <- spaces
          t <- term
          _ <- spaces
          return (buildAbs idLs t)

fact :: Parser Term
fact = try parenTerm
       <|> try variable
       <|> try numb
       <|> func

term :: Parser Term
term = do t <- fact
          ts <- many fact
          return (bldTerm t ts)

expr :: Parser Term
expr = do _ <- spaces
          t <- term
          _ <- eof
          return t

buildAbs :: [String] -> Term -> Term
buildAbs [] _ = error "can't build Abs without variables!"
buildAbs [v] t = Abs v t
buildAbs (v:vs) t = Abs v (buildAbs vs t)

bldTerm :: Term -> [Term] -> Term
bldTerm t [] = t
bldTerm t [tm] = App t tm
bldTerm t (tmf:tms:tml) = App (App t tmf) (bldTerm tms tml)

allIds :: Parser [String]
allIds = many1 idWithSpaces

idWithSpaces :: Parser String
idWithSpaces = do nm <- iden
                  _ <- spaces
                  return nm

dot :: Parser String
dot = string "."

slash :: Parser String
slash = string "\\"

iden :: Parser String
iden = do f <- letter
          v <- many letOrDg
          return (f:v)

letOrDg :: Parser Char
letOrDg = letter <|> digit

numb :: Parser Term
numb = do v <- many1 digit
          _ <- spaces
          return (Nmb (read v))

variable :: Parser Term
variable = do Var <$> idWithSpaces

parenTerm :: Parser Term
parenTerm = do _ <- string "("
               _ <- spaces
               v <- term
               _ <- spaces
               _ <- string ")"
               _ <- spaces
               return v

--------------------------------------------------------
integerTerm :: Int ->  Term
integerTerm n  = (Abs "s" (Abs "z" (buildTerm n)))
  where buildTerm 0 = Var "z"
        buildTerm j = (App (Var "s") (buildTerm (j-1)))

newVar :: [String] -> String -> String
newVar fvs nm = (until (\n -> notElem n fvs) next) (next nm)   -- flip elem fvs
  where next n@(c:_)| c=='9'    = '0':n
        next (c:cx) | isDigit c = (succ c):cx
        next n      = '0':n

--------------------------------------------------------
term0, term0a, term1, term1a, term1b, term1c :: Term
term0 = Abs "s" (Abs "z" (App (Var "s") (App (Var "s") (Var "z"))))
term0a = Abs "z" (App (Var "s") (App (Var "s") (Var "z")))
term1 = Abs "y" (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))
term1a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")
term1b = Abs "x" (Abs "y" (App (Var "x") (Var "y")))
term1c = Abs "y" (App (Var "x") (Var "y"))

term2, term2a, termAnd, termTest :: Term
term2 = App (App (Abs "f" (Abs "g" (Abs "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
                 (Abs "x" (Abs "y" (Var "x")))
            )
            (Abs "x" (Abs "y" (Var "x")))
term2a = App (Var "x") (App (Abs "x" (Var "x")) (App (Abs "x" (Var "x")) (Var "z")))
termAnd = Abs "x" (Abs "y" (App (App  (Var "x") (Var "y")) (Var "false")))
termTest = Abs "x" (Abs "x" (Abs "y" (Var "y")))

cont1 :: Contex
cont1 = [("true",Abs "x" (Abs "y" (Var "x")))
        ,("false",Abs "x" (Abs "y" (Var "y")))
        ,("test",Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n")))))
        ,("iszero",Abs "n" (App (App (Var "n") (Abs "x" (Var "false"))) (Var "true")))
        ,("plus",Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
        ,("mult",Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f"))))))
        ,("pred",Abs "n" (Abs "f" (Abs "x" (App (App (App (Var "n") (Abs "g" (Abs "h" (App (Var "h") (App (Var "g") (Var "f")))))) (Abs "u" (Var "x"))) (Abs "x" (Var "x"))))))
        ,("fixM",Abs "f" (App (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y"))))) (Abs "x" (App (Var "f") (Abs "y" (App (App (Var "x") (Var "x")) (Var "y")))))))
        ,("sumR",Abs "r" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 0)) (App (App (Var "plus") (Var "n")) (App (Var "r") (App (Var "pred") (Var "n")))))))
        ,("factR",Abs "fact" (Abs "n" (App (App (App (Var "test") (App (Var "iszero") (Var "n"))) (Nmb 1)) (App (App (Var "mult") (Var "n")) (App (Var "fact") (App (Var "pred") (Var "n")))))))
        ,("sum",App (Var "fixM") (Var "sumR"))
        ,("factor",App (Var "fixM") (Var "factR"))
        ]

termS2 :: String
termS2 = "(\\f g x. f x (g x))   (\\x y .x) (\\x y .x)"