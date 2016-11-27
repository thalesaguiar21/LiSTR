import Lexical.Parser
import DataTypes.Racional

import System.IO
import System.IO.Unsafe

import Data.Char
import Text.Parsec
import Text.Parsec.String

type Name = String
type Symbol = (Name, Value, Type)
data SymTableOrNull = SymTable SymTable | Null deriving Show
type SymTable = ([Symbol], SymTableOrNull)

addSymb :: [Symbol] -> [Name] -> String -> [Symbol]
addSymb [] ids t = [(id, IntV 0, stringToType t) | id <- ids]
addSymb st ids t = st ++ [(id, IntV 0, stringToType t) | id <- ids]

rmSymb :: [Symbol] -> Name -> [Symbol]
rmSymb [] _ = []
rmSymb (h : t) name1 = let (name0, value, type0) = h in
                        if (name0 == name1) then t
                        else h : rmSymb t name1

findSymb :: SymTable -> Name -> Symbol
findSymb ([], Null) _ = (" Not Found", (IntV (-1)), Int)
findSymb ([], SymTable anc) name = findSymb anc name
findSymb (h:t, anc) n0 = let (n1, _, _) = h in
                             if (n0 == n1) then h
                             else findSymb (t, anc) n0

atribSymb :: String -> Assign-> (Value, Type) -> SymTable -> SymTable
atribSymb id _ _ ([], Null) = error ( "variable " ++ id ++ " not declared")
atribSymb id a v ([], SymTable anc) = ([], SymTable (atribSymb id a v anc))
atribSymb id0 Assign (value0, type0) (h:t, anc0) = let (id1, _, _) = h in
                                                       if (id0 == id1) then
                                                          (atrib h (value0, type0) : t, anc0)
                                                       else let (st, anc1) = atribSymb id0 Assign (value0, type0) (t, anc0) in
                                                            (h : st, anc1)
atribSymb id0 AssignPlus (value0, type0) ((h:t), anc0) = let (id1, value1, type1) = h in
                                                             if (id0 == id1) then
                                                                (atrib h (add (value1, type1) (value0, type0)) : t, anc0)
                                                             else let (st, anc1) = atribSymb id0 AssignPlus (value0, type0) (t, anc0) in
                                                                  (h : st, anc1)
atribSymb id0 AssignMinus (value0, type0) ((h:t), anc0) = let (id1, value1, type1) = h in
                                                              if (id0 == id1) then
                                                                 (atrib h (sub (value1, type1) (value0, type0)) : t, anc0)
                                                              else let (st, anc1) = atribSymb id0 AssignPlus (value0, type0) (t, anc0) in
                                                                   (h : st, anc1)

atrib :: Symbol -> (Value, Type) -> Symbol
atrib (id, value0, type0) (value1, type1) = if (superType type0 type1) then (id, convertToType value1 type1 type0, type0)
                                            else error ((show type0) ++ " not compatible with " ++ (show type1) ++ " use cast")

convertToType :: Value -> Type -> Type -> Value
convertToType value Bool Bool = value
convertToType value Char Char = value
convertToType (CharV c) Char String = StringV (c : [])
convertToType value Int Int = value
convertToType (IntV n) Int Racional = RacionalV (PRacional n 1)
convertToType (IntV n) Int Float = FloatV (fromIntegral n)
convertToType (RacionalV (PRacional n m)) Racional Int = IntV (n `div` m)
convertToType value Racional Racional = value
convertToType (RacionalV r) Racional Float = FloatV ((fromIntegral (numerator r)) / (fromIntegral (denominator r)))
convertToType (FloatV f) Float Int = IntV (truncate f)
convertToType (FloatV f) Float Racional = error $ "Ainda nao foi implementado"--let r = toRational f in RacionalV (PRacional (numerator r) (denominator r))
convertToType value Float Float = value
convertToType value String String = value
convertToType _ type0 type1 = error $ "Can't convert " ++ (show type0) ++ " to " ++ (show type1)

superType :: Type -> Type -> Bool
superType Bool Bool = True
superType Char Char = True
superType Int Int = True
superType Racional Int = True
superType Racional Racional = True
superType Float Int = True
superType Float Racional = True
superType Float Float = True
superType String Char = True
superType String String = True
superType Bool _ = False
superType Char _ = False
superType Int _ = False
superType Racional _ = False
superType Float _ = False
superType String _ = False

add :: (Value, Type) -> (Value, Type) -> (Value, Type)
add (IntV n, Int) (IntV m, Int) = (IntV (n + m), Int)
add (RacionalV n, Racional) (RacionalV m, Racional) = (RacionalV (n + m), Racional)
add (FloatV n, Float) (FloatV m, Float) = (FloatV (n + m), Float)
add (StringV s0, String) (StringV s1, String) = (StringV (s0 ++ s1), String)
add v1 v2 = conversion add v1 v2

sub :: (Value, Type) -> (Value, Type) -> (Value, Type)
sub (IntV n, Int) (IntV m, Int) = (IntV (n - m), Int)
sub (RacionalV n, Racional) (RacionalV m, Racional) = (RacionalV (n - m), Racional)
sub (FloatV n, Float) (FloatV m, Float) = (FloatV (n - m), Float)
sub v1 v2 = conversion sub v1 v2

prod :: (Value, Type) -> (Value, Type) -> (Value, Type)
prod (IntV n, Int) (IntV m, Int) = (IntV (n * m), Int)
prod (RacionalV n, Racional) (RacionalV m, Racional) = (RacionalV (n * m), Racional)
prod (FloatV n, Float) (FloatV m, Float) = (FloatV (n * m), Float)
prod v1 v2 = conversion prod v1 v2

divide :: (Value, Type) -> (Value, Type) -> (Value, Type)
divide (IntV n, Int) (IntV m, Int) = (RacionalV (PRacional n m), Racional)
divide (RacionalV n, Racional) (RacionalV m, Racional) = (RacionalV (divR n m), Racional)
divide (FloatV n, Float) (FloatV m, Float) = (FloatV (n / m), Float)
divide v1 v2 = conversion divide v1 v2

modulus :: (Value, Type) -> (Value, Type) -> (Value, Type)
modulus (IntV n, Int) (IntV m, Int) = (IntV (n `mod` m), Int)

lt :: (Value, Type) -> (Value, Type) -> Bool
lt (IntV n, Int) (IntV m, Int) = (n < m)
lt (RacionalV n, Racional) (RacionalV m, Racional) = ((numerator (signum (n - m))) < 0)
lt (FloatV n, Float) (FloatV m, Float) = (n < m)

gt :: (Value, Type) -> (Value, Type) -> Bool
gt (IntV n, Int) (IntV m, Int) = (n > m)
gt (RacionalV n, Racional) (RacionalV m, Racional) = ((numerator (signum (n - m))) > 0)
gt (FloatV n, Float) (FloatV m, Float) = (n > m)

leq :: (Value, Type) -> (Value, Type) -> Bool
leq (IntV n, Int) (IntV m, Int) = (n <= m)
leq (RacionalV n, Racional) (RacionalV m, Racional) = ((numerator (signum (n - m))) <= 0)
leq (FloatV n, Float) (FloatV m, Float) = (n <= m)

geq :: (Value, Type) -> (Value, Type) -> Bool
geq (IntV n, Int) (IntV m, Int) = (n >= m)
geq (RacionalV n, Racional) (RacionalV m, Racional) = ((numerator (signum (n - m))) >= 0)
geq (FloatV n, Float) (FloatV m, Float) = (n >= m)

eq :: (Value, Type) -> (Value, Type) -> Bool
eq (IntV n, Int) (IntV m, Int) = (n == m)
eq (RacionalV n, Racional) (RacionalV m, Racional) = (n == m)
eq (FloatV n, Float) (FloatV m, Float) = (n == m)

diff :: (Value, Type) -> (Value, Type) -> Bool
diff (IntV n, Int) (IntV m, Int) = (n /= m)
diff (RacionalV n, Racional) (RacionalV m, Racional) = (n /= m)
diff (FloatV n, Float) (FloatV m, Float) = (n /= m)

conversion :: ((Value, Type) -> (Value, Type) -> (Value, Type)) -> (Value, Type) -> (Value, Type) -> (Value, Type)
conversion op (v0, t0) (v1, t1) = if (superType t0 t1) then op (v0, t0) (convertToType v1 t1 t0, t0)
                                  else if (superType t1 t0) then op (convertToType v0 t0 t1, t1) (v1, t1)
                                       else error $ "error cannot convert from " ++ show t0 ++ " to " ++ show t1

valueToBool :: Value -> Bool
valueToBool (BoolV b) = b
valueToBool _ = error $ "expected bool type"

evalA :: ArithmeticExp -> SymTable -> (Value, Type)
evalA (ExpId id) st = let (name, value, type0) = findSymb st id in
                          if (name == " Not Found") then error $ "variable " ++ id ++ " not declared"
                          else (value, type0)
evalA (Const (CharV c)) _ = (CharV c, Char)
evalA (Const (IntV n)) _ = (IntV n, Int)
evalA (Const (RacionalV r)) _ = (RacionalV r, Racional)
evalA (Const (FloatV f)) _ = (FloatV f, Float)
evalA (Const (StringV s)) _ = (StringV s, String)
evalA (Exp1 Add exp1 exp2) st = add (evalA exp1 st) (evalA exp2 st)
evalA (Exp1 Sub exp1 exp2) st = sub (evalA exp1 st) (evalA exp2 st)
evalA (Exp2 Prod exp1 exp2) st = prod (evalA exp1 st) (evalA exp2 st)
evalA (Exp2 Div exp1 exp2) st = divide (evalA exp1 st) (evalA exp2 st)
evalA (Exp2 Mod exp1 exp2) st = modulus (evalA exp1 st) (evalA exp2 st)
--evalA (Exp2 VecProd exp1 exp2) st = vecProd (evalA exp1 st) (evalA exp2 st)
--evalA (Exp2 FunCall exp1 exp2) st = funCall fun
--operacoes posfixadas e prefixadas nao foram implementadas
evalA (Neg exp) st = sub (IntV 0, Int) (evalA exp st)
evalA exp _ = error $ "couldn't understand the expression " ++ show exp

evalL :: LogicExp -> SymTable -> Bool
evalL (BoolId id) st =  let (name, value, type0) = findSymb st id in
                          if (name == " Not Found") then error $ "variable " ++ id ++ " not declared"
                          else valueToBool value
evalL (LogicConst b) _ = b
evalL (LogicExp Lt exp1 exp2) st = lt (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp Gt exp1 exp2) st = gt (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp LEq exp1 exp2) st = leq (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp GEq exp1 exp2) st = geq (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp Eq exp1 exp2) st = eq (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp Diff exp1 exp2) st = diff (evalA exp1 st) (evalA exp2 st)
evalL (BoolExp And exp1 exp2) st = ((evalL exp1 st) && (evalL exp2 st))
evalL (BoolExp Or exp1 exp2) st = ((evalL exp1 st) || (evalL exp2 st))

eval :: Exp -> SymTable -> (Value, Type)
eval (LExp exp) st = (BoolV (evalL exp st), Bool)
eval (AExp exp) st = evalA exp st

varToSymbol :: Type -> IdOrAtrib -> SymTable -> Symbol
varToSymbol Bool (IdOrAtribI (Id id)) _ = (id, BoolV False, Bool)
varToSymbol Char (IdOrAtribI (Id id)) _ = (id, CharV ' ', Char)
varToSymbol Int (IdOrAtribI (Id id)) _ = (id, IntV 0, Int)
varToSymbol Racional (IdOrAtribI (Id id)) _ = (id, RacionalV (PRacional 0 0), Racional)
varToSymbol Float (IdOrAtribI (Id id)) _ = (id, FloatV 0.0, Float)
varToSymbol String (IdOrAtribI (Id id)) _ = (id, StringV "", String)
varToSymbol type0 (IdOrAtribA (Atrib (Id id) Assign exp)) st = let (v, type1) = eval exp st in
                                                                   atrib (id, (IntV 0), type0) (v, type1)

playWhile1 :: LogicExp -> Stmt -> SymTable -> (IO SymTable)
playWhile1 l s st = if (evalL l st)
                    then playWhile2 l s s st
                    else return st

playWhile2 :: LogicExp -> Stmt -> Stmt -> SymTable -> (IO SymTable)
playWhile2 l b (Stmts []) st = playWhile1 l b st
playWhile2 l b (Stmts (Continue:t)) st = playWhile1 l b st
playWhile2 _ _ (Stmts (Break:t)) st = return st
playWhile2 l b (Stmts (IfS (If exp (Stmts stmt)):t)) st = if (evalL exp st)
                                                          then do { s0 <- playWhile2 l b (Stmts (stmt ++ t)) ([], SymTable st)
                                                                  ; return (ancestor s0)}
                                                          else playWhile2 l b (Stmts t) st
playWhile2 l b (Stmts (IfS (IfElse exp (Stmts stmt0) (Stmts stmt1)):t)) st = if (evalL exp st)
                                                                             then do { s0 <- playWhile2 l b (Stmts (stmt0 ++ t)) ([], SymTable st)
                                                                                     ; return (ancestor s0)
                                                                                     }
                                                                             else do { s1 <- playWhile2 l b (Stmts (stmt1 ++ t)) ([], SymTable st)
                                                                                     ; return (ancestor s1)
                                                                                     }
playWhile2 l b (Stmts (h:t)) st = do { s0 <- playStmt h st
                                     ; s1 <- playWhile2 l b (Stmts t) (s0)
                                     ; return s1
                                     }

isString :: Value -> (Bool, String)
isString (StringV v) = (True, v)
isString _ = (False, "")

write :: [Exp] -> SymTable -> IO ()
write [] _ = return ()
write (h:t) st = let (v, t0) = eval h st 
                     (b, str) = isString v in
                     if (b) then putStr str >> write t st--Allows \n
                     else putStr ((show v) ++ " ") >> write t st

readV :: [Id] -> SymTable -> (IO SymTable)
readV [] st = return st
readV ids st = do { ist <- readT ids [] st
                    ; return (snd ist)
                    }

readT :: [Id] -> String -> SymTable -> (IO ([id], SymTable))
readT [] _ st = return ([], st)
readT ids [] st = do { ln <- getLine ; readT ids (whites ln) st}
readT ((Id id):t) s st0 = do { let (_, _, t0) = findSymb st0 id
                                   n = readT2 t0 s
                                   st1 = atribSymb id Assign (fst n, t0) st0
                             ; readT t (whites (snd n)) st1
                             }

whites :: String -> String
whites str = dropWhile isSpace str--isSpace includes control characters

invertCase :: Char -> Char
invertCase c = if (isUpper c) then toLower c else toUpper c

--must not receive the empty string("", [])
readT2 :: Type -> String -> (Value, String)
readT2 Bool (h:t) = let ((v, r):_) = reads ((invertCase h):t)::[(Bool,String)] in (BoolV v, r) 
readT2 Char s = let (h:t) = whites s in (CharV h, t)
readT2 Int s = let ((v, r):_) = reads s ::[(Int, String)] in (IntV v, r)
readT2 Racional s = let ((v, r):_) = reads s ::[(Racional, String)] in (RacionalV v, r)
readT2 Float s = let ((v, r):_) = reads s ::[(Double, String)] in (FloatV v, r)
readT2 String s = let ((v, r)) = break isSpace s in (StringV v, r)

playStmt :: Stmt -> SymTable -> (IO SymTable)
playStmt (Stmts []) st = return st
playStmt (Stmts (h:t)) st = do { s0 <- playStmt h st
                               ; s1 <- playStmt (Stmts t) s0
                               ; return s1
                               }
playStmt (VarS (VarDecl type0 [])) st = (return st)
playStmt (VarS (VarDecl type0 (h:t))) (st0, anc0) = let (n, _, _) = (findSymb (st0, Null) (getName h)) in
                                                       if (n == " Not Found") then 
                                                          do { (st1, anc1) <- playStmt (VarS (VarDecl type0 t)) (st0, anc0)
                                                             ; return ((varToSymbol type0 h (st1, anc1)) : st1, anc1)}
                                                       else error $ "variable " ++ n ++" already declared"
playStmt (AtribS (Atrib (Id id) assign exp)) st = return (atribSymb id assign (eval exp st) st)
playStmt (IfS (If exp stmt)) st = if (evalL exp st)
                                  then do { s <- playStmt stmt ([], SymTable st)
                                          ; return (ancestor s)
                                          }
                                  else return st
playStmt (IfS (IfElse exp stmt0 stmt1)) st = if (evalL exp st)
                                             then do { s <- playStmt stmt0 ([], SymTable st)
                                                     ; return (ancestor s)
                                                     }
                                             else do { s <- playStmt stmt1 ([], SymTable st)
                                                     ; return (ancestor s)
                                                     }
playStmt (While exp stmt) st = do { s0 <- (playWhile1 exp stmt ([], SymTable st))
                                  ; return (ancestor s0)
                                  }
playStmt (Read []) st = error "read must receive an argument"
playStmt (Read l) st = readV l st
playStmt (Write []) st = error "write must receive an argument"
playStmt (Write exps) st = write exps st >> return st
playStmt _ st = return st

ancestor :: SymTable -> SymTable
ancestor (_, SymTable st) = st
ancestor (_, _) = ([], Null)

getName :: IdOrAtrib -> Name
getName (IdOrAtribI (Id id)) = id
getName (IdOrAtribA (Atrib (Id id) _ _)) = id

playProgram :: P -> [Symbol] -> (IO [Symbol])
playProgram (P []) st = return st
playProgram (P ((FunP (Proc (Id id) (ParamDecl []) stmt)) : t)) st0 = if (id == "main") then 
                                                                        do { st1 <- playStmt stmt ([], SymTable (st0, Null))
                                                                           ; return ( fst( ancestor(st1) ) )
                                                                           }
                                                                     else playProgram (P t) st0
playProgram (P ((FunP (Proc (Id id) (ParamDecl _) stmt)) : t)) st = if (id == "main") then
                                                                       error ("main must receive only one argument")
                                                                    else playProgram (P t) st 
playProgram (P ((FunP (Fun _ (Id id) _ _)) : t)) st = if (id == "main") then error $ "main must be a procedure" else playProgram (P t) st 
playProgram (P ((VarP v) : t)) st = do { s0 <- playProgram (VarP v) st
                                       ; s1 <- playProgram (P t) (s0)
                                       ; return s1
                                       }
playProgram (VarP (VarDecl type0 [])) st = return st
playProgram (VarP (VarDecl type0 (h:t))) st = let (n, _, _) = findSymb (st, Null) (getName h) in
                                                  do { s <- playProgram (VarP (VarDecl type0 t)) st
                                                     ; if (n == " Not Found") then return ((varToSymbol type0 h (st, Null)) : s)
                                                     ; else error $ "variable " ++ n ++ " already declared"
                                                     }

m :: String -> IO ()
m f = do {playProgram (principal2 f) []; return ()}

play2 :: String -> P
play2 inp = case parse mainparser "" inp of
             Left e  -> error $ show e
             Right r -> r

principal2 :: String -> P
principal2 fn =  play2 (unsafePerformIO (leia fn))

play :: String -> IO ()
play inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

principal :: String -> IO ()
principal fn =  play (unsafePerformIO (leia fn))

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (y)