import Lexical.Types
import Lexical.Parser
import DataTypes.Racional
import Lexical.VarTable

import System.IO
import System.IO.Unsafe

import Data.Char
import Text.Parsec
import Text.Parsec.String

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
isString (CharV c) = (True, c:[])
isString _ = (False, "")

write :: [Exp] -> SymTable -> IO ()
write [] _ = return ()
write (h:t) st = let (v, t0) = eval h st 
                     (b, str) = isString v in
                     if (b) then putStr str >> write t st--Allows \n
                     else putStr ((show v) ++ " ") >> write t st

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

playStmt :: Stmt -> SymTable -> (IO (SymTable))
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
playStmt (Read ids) st = do { ist <- readT ids [] st
                          ; return (snd ist)
                          }
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
                                                                     else playProgram (P t) st0--Criar procedimento
playProgram (P ((FunP (Proc (Id id) (ParamDecl _) stmt)) : t)) st = if (id == "main") then
                                                                       error ("main must receive only one argument")
                                                                    else playProgram (P t) st--criar funcao 
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
playProgram (StructDecl (Struct (Id id) l)) st = return st

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