{-# OPTIONS_GHC -Wall #-}
module Lab10 where

import Text.ParserCombinators.Parsec

data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String
         | Const Value
         | Op Exp Bop Exp
                 deriving (Show, Eq)

data Bop =  Plus | Minus | Times | Div
          | Gt | Ge | Lt | Le| Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Read String
          | Write Exp
          | Incr String
          | If Exp Stmt
          | While Exp Stmt
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = ([String], [(String,Value)], [String])

type VarEnv  = [(String,Type)]

-- Function 1.a -----------------------------------------
getValue ::  StateW -> String -> Value
getValue st = findInMap (gsnd st)

findInMap :: [(String,Value)] -> String -> Value
findInMap [] _ = error "Value is not present!"
findInMap (x:xs) var | fst x == var = snd x
                     | otherwise = findInMap xs var

-- Function 1.b -----------------------------------------
updValue :: StateW -> String -> Value -> StateW
updValue st var val = replaceInMap st (gsnd st) var val []

replaceInMap :: StateW -> [(String,Value)] -> String -> Value -> [(String,Value)] -> StateW
replaceInMap _ [] _ _ _ = error "Value is not present!"
replaceInMap st (x:xs) var val rs | fst x == var = (gfst st, rs ++ ((var, val) : xs), gtrd st)
                                  | otherwise = replaceInMap st xs var val (rs ++ [x])

-- Function 2 ----------------------------------------- 
readValue :: StateW -> Type -> (StateW,Value)
readValue st tp = case st of
     ([], _, _) -> case tp of
          It -> (st, I 0)
          Bt -> (st, B False)
     (x:xs, mp, out) -> case tp of
       It -> if isNum x then ((xs, mp, out), I (read x)) else (st, I 0)
       Bt -> if isBool x then ((xs, mp, out), B (strToBool x)) else (st, B False)

-- Function 3 -----------------------------------------
writeValue :: StateW -> Value -> StateW
writeValue st val = case val of
  I v -> (gfst st, gsnd st, gtrd st ++ [show v])
  B v -> (gfst st, gsnd st, gtrd st ++ [show v])

-- Function 4.a ----------------------------------------- 
evExp :: StateW -> Exp -> Value
evExp st e = case e of
  Var s -> getValue st s
  Const va -> va
  Op expL bop expR -> evOp bop (evExp st expL) (evExp st expR)

evOp :: Bop -> Value -> Value -> Value
evOp bop valL valR = case bop of
  Plus -> case (valL, valR) of
       (I l, I r) -> I (l + r)
       _ -> error "Invalid operands"
  Minus -> case (valL, valR) of
       (I l, I r) -> I (l - r)
       _ -> error "Invalid operands"
  Times -> case (valL, valR) of
       (I l, I r) -> I (l * r)
       _ -> error "Invalid operands"
  Div -> case (valL, valR) of
       (I l, I r) -> I (div l  r)
       _ -> error "Invalid operands"
  Gt -> case (valL, valR) of
       (I l, I r) -> B (l > r)
       _ -> error "Invalid operands"
  Ge -> case (valL, valR) of
       (I l, I r) -> B (l >= r)
       _ -> error "Invalid operands"
  Lt -> case (valL, valR) of
      (I l, I r) -> B (l < r)
      _ -> error "Invalid operands"
  Le -> case (valL, valR) of
       (I l, I r) -> B (l <= r)
       _ -> error "Invalid operands"
  Eql -> case (valL, valR) of
       (I l, I r) -> B (l == r)
       _ -> error "Invalid operands"
  And -> case (valL, valR) of
       (B l, B r) -> B (l && r)
       _ -> error "Invalid operands"
  Or -> case (valL, valR) of
       (B l, B r) -> B (l || r)
       _ -> error "Invalid operands"

-- Function 4.b -----------------------------------------
evStmt :: StateW -> Stmt -> StateW
evStmt st stmnt = case stmnt of
  Assign s ex -> updValue st s (evExp st ex)

  Read s -> let stateAndVal = readValue st (getType (getValue st s))
            in updValue (fst stateAndVal) s (snd stateAndVal)

  Write ex -> writeValue st (evExp st ex)

  Incr s -> updValue st s (doIncr st s)

  If ex stm -> case evExp st ex of
       B b -> if b then evStmt st stm else st
       _ -> error "Not a Bool in if statement!"

  While ex stm -> case evExp st ex of
    B b -> if b then evStmt (evStmt st stm) (While ex stm) else st
    _ -> error "Not a Bool in While cycle!"

  For initC ex stmC stmB -> evStmt (evStmt st initC) (While ex (Block [] [stmB, stmC]))

  Block vars opers -> let initedVars = map setDefault vars
                          finalState = foldl evStmt (gfst st, initedVars ++ gsnd st, gtrd st) opers
                      in (gfst finalState, drop (length initedVars) (gsnd finalState), gtrd finalState)

doIncr :: StateW -> String -> Value
doIncr st s = case getValue st s of
  I n -> I (n + 1)
  B _ -> error "Can't increment Bool!"

getType :: Value -> Type
getType val = case val of
  I _ -> It
  B _ -> Bt

setDefault :: (String, Type) -> (String, Value)
setDefault pr = case snd pr of
  It -> (fst pr, I 0)
  Bt -> (fst pr, B False)

-- Function 4.c -----------------------------------------
evProgram :: Program -> [String] -> [String]
evProgram prog args = gtrd (evStmt (args, [], []) prog)

-- Function 5.a -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type
iswfOp op [valL, valR] = case op of
     Plus -> case (valL, valR) of
          (It, It) -> Just It
          _ -> Nothing
     Minus -> case (valL, valR) of
          (It, It) -> Just It
          _ -> Nothing
     Times -> case (valL, valR) of
          (It, It) -> Just It
          _ -> Nothing
     Div -> case (valL, valR) of
          (It, It) -> Just It
          _ -> Nothing
     Gt -> case (valL, valR) of
          (It, It) -> Just Bt
          _ -> Nothing
     Ge -> case (valL, valR) of
          (It, It) -> Just Bt
          _ -> Nothing
     Lt -> case (valL, valR) of
          (It, It) -> Just Bt
          _ -> Nothing
     Le -> case (valL, valR) of
          (It, It) -> Just Bt
          _ -> Nothing
     Eql -> case (valL, valR) of
          (It, It) -> Just Bt
          _ -> Nothing
     And -> case (valL, valR) of
          (Bt, Bt) -> Just Bt
          _ -> Nothing
     Or -> case (valL, valR) of
          (Bt, Bt) -> Just Bt
          _ -> Nothing
iswfOp _ _ = Nothing

-- Function 5.b -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type
iswfExp e ve = case e of
  Var s -> findVar ve s
  Const var -> Just (getType var)
  Op expL bop expR -> case (iswfExp expL ve, iswfExp expR ve) of
       (Just lt, Just rt) -> iswfOp bop [lt, rt]
       _ -> Nothing

findVar :: VarEnv -> String -> Maybe Type
findVar [] _ = Nothing
findVar (x:xs) var | fst x == var = Just (snd x)
                   | otherwise = findVar xs var

-- Function 5.c -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool
iswfStmt stm ve = case stm of
  Assign s ex -> case (findVar ve s, iswfExp ex ve) of
       (Just tl, Just tr) -> tl == tr
       _ -> False

  Read s -> case findVar ve s of
    Nothing -> False
    Just _ -> True

  Write ex -> case iswfExp ex ve of
    Nothing -> False
    Just _ -> True

  Incr s -> case findVar ve s of
    Nothing -> False
    Just tp -> tp == It

  If ex st -> case iswfExp ex ve of
    Nothing -> False
    Just tp -> (tp == Bt) && iswfStmt st ve

  While ex st -> case iswfExp ex ve of
    Nothing -> False
    Just tp -> (tp == Bt) && iswfStmt st ve

  For initC ex stmB stmC -> case (iswfStmt initC ve, iswfExp ex ve, iswfStmt stmB ve, iswfStmt stmC ve) of
       (True, Just tp, True, True) -> tp == Bt
       _ -> False

  Block vars sts -> all (\st -> iswfStmt st (vars ++ ve)) sts

iswfProgram :: Program -> Bool
iswfProgram st = iswfStmt st []

iden :: Parser String
iden = try( do {nm <- identif;
                if (any(nm==) ["int","bool","read","write","if","while","for","true","false"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm
               } )

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop
mulOp = (oper "*" Times) <|> (oper "/" Div)

disOp :: Parser Bop
disOp = (oper "&" And)

conOp :: Parser Bop
conOp = (oper "|" Or)

lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

--   :type Op -----> Exp -> Bop -> Exp -> Exp 
--   :type flip Op -------> Bop -> Exp -> Exp -> Exp         
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char ->  Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try( lexem( string st >> notFollowedBy alphaNum))

typev :: Parser Type
typev = do {keyword "int"; return It}
        <|> do {keyword "bool"; return Bt}

-- Function 6.a -----------------------------------------
identif :: Parser String
identif = do fs <- letter
             tl <- many (letter <|> digit)
             return (fs:tl)

number :: Parser Int
number = do read <$> (many1 digit)

addOp :: Parser Bop
addOp = (oper "+" Plus) <|> (oper "-" Minus)

relOp :: Parser Bop
relOp =  try (oper ">=" Ge) <|> try (oper ">" Gt) <|> try (oper "<=" Le) <|> try (oper "<" Lt) <|> (oper "==" Eql)

-------------------------------------------------------
factor :: Parser Exp
factor = do { symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

-- Function 6.b -----------------------------------------
term :: Parser Exp
term = exprParser factor mulOp

relat :: Parser Exp
relat = exprParser term addOp

conj :: Parser Exp
conj = exprParser relat relOp

disj :: Parser Exp
disj = exprParser conj conOp

expr :: Parser Exp
expr = exprParser disj disOp

exprParser :: Parser Exp -> Parser Bop -> Parser Exp
exprParser exP bopP = try (do lf <- lexem exP
                              op <- lexem bopP
                              rt <- lexem exP
                              return (Op lf op rt))
                       <|>
                           do lexem exP

------------------------------------------------------
stmt :: Parser Stmt
stmt = do {keyword "for"; forSt}
       <|> do {keyword "while"; whileSt}
       <|> do {keyword "if"; ifSt}
       <|> do {keyword "read"; inSt}
       <|> do {keyword "write"; outSt}
       <|> do {var <- lexem iden; assignSt var}
       <|> blockSt
       <?> "statement"

-- Function 6.c -----------------------------------------
forSt :: Parser Stmt
forSt = do _ <- symbol '('
           initC <- lexem stmt
           _ <- symbol ';'
           ex <- lexem expr
           _ <- symbol ';'
           stmC <- lexem stmt
           _ <- symbol ')'
           stmB <- lexem stmt
           return (For initC ex stmC stmB)

whileSt :: Parser Stmt
whileSt = do _ <- symbol '('
             ex <- lexem expr
             _ <- symbol ')'
             stm <- lexem stmt
             return (While ex stm)

ifSt :: Parser Stmt
ifSt = do _ <- symbol '('
          ex <- lexem expr
          _ <- symbol ')'
          stm <- lexem stmt
          return (If ex stm)

inSt :: Parser Stmt
inSt = do Read <$> lexem iden

outSt :: Parser Stmt
outSt = do Write <$> lexem expr

assignSt :: String -> Parser Stmt
assignSt str = try (do _ <- lexem (string "++")
                       return (Incr str))
               <|>
                    do _ <- lexem (string ":=")
                       ex <- lexem expr
                       return (Assign str ex)

blockSt :: Parser Stmt
blockSt = do _ <- symbol '{'
             defs <- many (lexem defin)
             stms <- lexem listSt
             _ <- symbol '}'
             return (Block (concat defs) stms)

defin :: Parser [(String, Type)]
defin = do tp <- lexem typev
           idens <- lexem listId
           _ <- symbol ';'
           return (map (\ident -> (ident, tp)) idens)

listId :: Parser [String]
listId = do fsid <- lexem iden
            otherIds <- many (do _ <- symbol ','
                                 lexem iden)
            return (fsid:otherIds)

listSt :: Parser [Stmt]
listSt = do fsstm <- lexem stmt
            otherStms <- many (do _ <- symbol ';'
                                  lexem stmt)
            return (fsstm:otherStms)

--------------------------------------------
gfst :: (a, b, c) -> a
gfst tp = case tp of
     (f, _, _) -> f

gsnd :: (a, b, c) -> b
gsnd tp = case tp of
     (_, s, _) -> s

gtrd :: (a, b, c) -> c
gtrd tp = case tp of
     (_, _, th) -> th

isNum :: String -> Bool
isNum str = case parse (lexem number) "" str of
  Left _ -> False
  Right _ -> True

isBool :: String -> Bool
isBool str = case str of
     "True" -> True
     "False" -> True
     _ -> False

strToBool :: String -> Bool
strToBool str = case str of
  "True" -> True
  "False" -> False
  _ -> error "Not a boolean value!"

---------------------------------------------		
program :: Parser Stmt
program = do {spaces; r <- stmt; eof; return r}

parseL :: String -> Either ParseError Program
parseL s = parse program "" s

--------------------------------------------
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Read "a", Assign "b" (Const (I 0))
                   , If (Op (Var "a") Ge (Const(I 0)))
                        (Block [("c", Bt)]
                               [Assign "c" (Const (B True)),
                                While (Var "c")
                                 (Block []
                                   [(Incr "b"),
                                    Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                               ]
                        )
                   , Write (Op (Var "b") Minus (Const (I 1)))
                   ]

squareRootS :: String
squareRootS =
   "{int a, b; \
   \ read a; b := 0; \
   \ if (a>= 0)\
   \    {bool c; c:=true; while(c) {b++; c:= a >= b*b}\
   \    };\
   \  write (b-1)\
   \ }"

fibonacci :: Program
fibonacci =
    Block [("in",It), ("out",It)]
          [ Read "in",  Assign "out" (Const (I 0))
          , If (Op (Var "in") Ge (Const(I 0)))
               (Block [("f0",It), ("f1",It), ("c",It)]
                     [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)), Assign "out" (Const (I 1)),
                      If (Op (Var "in") Gt (Const (I 1)))
                         (For (Assign "c" (Const (I 1)))
                             (Op (Var "c") Lt (Var "in"))
                             (Incr "c")
                             (Block []
                                    [Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                    , Assign "f0" (Var "f1")
                                    , Assign "f1" (Var "out")
                                    ]
                              )
                         )
                     ])
          , Write (Var "out")
          ]

fibonacciS :: String
fibonacciS =
 " {int in, out; read in; out := 0; \n\
   \if (in>=0){int f0, f1,c; \n\
   \           f0 := 1; f1 := 1; out := 1; \n\
   \           if(in>1) \n \
   \              for (c := 1; c < in; c++) {\n\
   \                   out := f0 + f1; f0 := f1; f1 := out\n\
   \              }\n\
   \          }; \n\
   \write out \n\
  \}"