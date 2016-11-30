module Lexical.Interpreter where

import Lexical.Types
import Lexical.Parser
import DataTypes.Racional
import Lexical.TypeTable
import Lexical.VarTable
import Lexical.FunTable

import System.IO
import System.IO.Unsafe

import Data.Char
import Text.Parsec
import Text.Parsec.String

listTypeToValue :: [(Type, Id)] -> [Value]
listTypeToValue l = map (typeToValue . fst) l

varToSymbol :: Type -> IdOrAtrib -> SymTable -> Symbol
varToSymbol (Struct name l) (IdOrAtribI (Id id)) _ = (id, StructV (Struct name l) (listTypeToValue l), Struct name l)
varToSymbol t (IdOrAtribI (Id id)) _ = (id, (typeToValue t), t)
varToSymbol type0 (IdOrAtribA (Atrib (Id id) Assign exp)) st = let (v, type1) = eval exp st in
                                                                   atrib (id, (IntV 0), type0) (v, type1)

playWhile1 :: LogicExp -> Stmt -> SymTable -> TypeTable -> FunTable-> (IO (SymTable, TypeTable, FunTable))
playWhile1 l s st tt ft = if (evalL l st)
                          then playWhile2 l (Stmts []) s st tt ft
                          else return (st, tt, ft)

playWhile2 :: LogicExp -> Stmt -> Stmt -> SymTable -> TypeTable -> FunTable -> (IO (SymTable, TypeTable, FunTable))
playWhile2 l done (Stmts []) st tt ft = playWhile1 l done st tt ft
playWhile2 l (Stmts done) (Stmts (Continue:t)) st tt ft = playWhile1 l (Stmts (done ++ t)) st tt ft
playWhile2 _ _ (Stmts (Break:t)) st tt ft = return (st, tt, ft)
playWhile2 l done (Stmts ((VarS v): t)) st tt ft = do { (s0, t0, f0) <- playStmt (VarS v) st tt ft
                                                   ; playWhile2 l done (Stmts t) s0 t0 f0--don't declare the variable again
                                                   }
playWhile2 l (Stmts done) (Stmts (h:t)) st tt ft = do { (s0, t0, f0) <- playStmt h st tt ft
                                                   ; playWhile2 l (Stmts (done ++ [h])) (Stmts t) s0 t0 f0
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
readT ((StructId ((Id id):ids)):t) s st0 = do { let (_, v0, t0) = findSymb st0 id
                                                    (_, t1) = getValueFromStruct ids v0 t0
                                                    n = readT2 t1 s
                                                    st1 = atribSymb (StructId ((Id id):ids)) Assign (fst n, t1) st0
                                              ; readT t (whites (snd n)) st1
                                              }
readT ((Id id):t) s st0 = do { let (_, _, t0) = findSymb st0 id
                                   n = readT2 t0 s
                                   st1 = atribSymb (Id id) Assign (fst n, t0) st0
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

-- Mudar
playStmt :: Stmt -> SymTable -> TypeTable -> FunTable -> (IO (SymTable, TypeTable, FunTable))
playStmt (Stmts []) st tt ft    = return (st, tt, ft)
playStmt (Stmts (h:t)) st tt ft = do { (s0, tt0, ft0) <- playStmt h st tt ft
                                     ; (s1, tt1, ft1) <- playStmt (Stmts t) s0 tt ft
                                     ; return (s1, tt1, ft1)
                                     }
playStmt (VarS (VarDecl (StructAux name) l)) st tt ft = let (Struct (Id n) s) = (findType name tt) in
                                                           if (n == " Not Found")
                                                           then error $ "type \"" ++ name ++"\" not declared"
                                                           else playStmt (VarS (VarDecl (Struct (Id n) s) l)) st tt ft
playStmt (VarS (VarDecl type0 [])) st tt ft = return (st, tt, ft)
playStmt (VarS (VarDecl type0 (h:t))) (st, anc) tt ft = let (n, _, _) = (findSymb (st, Null) (getName h)) in
                                                           if (n == " Not Found")
                                                           then playStmt (VarS (VarDecl type0 t))
                                                                         (((varToSymbol type0 h (st, anc)) : st), anc) tt ft
                                                           else error $ "variable " ++ n ++" already declared"
playStmt (AtribS (Atrib id assign exp)) st tt ft = return ((atribSymb id assign (eval exp st) st), tt, ft)
playStmt (IfS (If exp stmt)) st tt ft = if (evalL exp st)
                                        then do { (s, t, f) <- playStmt stmt ([], SymTable st) tt ft
                                                ; return ((ancestor s), t, f)
                                                }
                                        else return (st, tt, ft)
playStmt (IfS (IfElse exp stmt0 stmt1)) st tt ft = if (evalL exp st)
                                                   then do { (s, t, f) <- playStmt stmt0 ([], SymTable st) tt ft
                                                           ; return ((ancestor s), t, f)
                                                           }
                                                   else do { (s, t, f) <- playStmt stmt1 ([], SymTable st) tt ft
                                                           ; return ((ancestor s), t, f)
                                                           }
playStmt (While exp stmt) st tt ft = do { (s0, t0, f0) <- (playWhile1 exp stmt ([], SymTable st) tt ft)
                                        ; return ((ancestor s0), t0, f0)
                                        }
playStmt (Read []) st tt ft  = error "read must receive an argument"
playStmt (Read ids) st tt ft = do { ist <- readT ids [] st
                                  ; return ((snd ist), tt, ft)
                                  }
playStmt (Write []) st tt ft   = error "write must receive an argument"
playStmt (Write exps) st tt ft = write exps st >> return (st, tt, ft)
playStmt _ st tt ft = return (st, tt, ft)

ancestor :: SymTable -> SymTable
ancestor (_, SymTable st) = st
ancestor (_, _) = ([], Null)

getName :: IdOrAtrib -> Name
getName (IdOrAtribI (Id id)) = id
getName (IdOrAtribA (Atrib (Id id) _ _)) = id

-- Ajeitando
{-type FunTable = [Fun]

data Fun = FunC Name Type [(Name, Type)] [Stmt] | Proc' Name [(Name, Type)] [Stmt]-}
playProgram :: P -> [Symbol] -> TypeTable -> FunTable -> (IO ([Symbol], TypeTable, FunTable))
playProgram (P []) st tt ft = return (st, tt, ft)
playProgram (P (h:t)) st0 tt0 ft0 = do {(st1, tt1, ft1) <- (playProgram h st0 tt0 ft0); playProgram (P t) st1 tt1 ft1}
playProgram (FunP (Proc (Id id) (ParamDecl []) stmt)) st0 tt0 ft0 = if (id == "main") then 
                                                                    do { (st1, tt1, ft1) <- playStmt stmt ([], SymTable (st0, Null)) tt0 ft0
                                                                       ; return ( fst( ancestor(st1) ), tt1, ft1 )
                                                                       }
                                                                    else return (st0, tt0, (addFun ft0  (Proc' (Id id) [] stmt)))--Criar procedimento
playProgram (FunP (Proc (Id id) (ParamDecl params) stmt)) st tt ft = if (id == "main") 
                                                                     then error ("main must not receive an argument")
                                                                     else return (st, tt, (addFun ft (Proc' (Id id) params stmt)))--criar procedimento 
playProgram (FunP (Fun tp (Id id) (ParamDecl params) stmt)) st tt ft = if (id == "main") 
                                                                       then error $ "main must be a procedure" 
                                                                       else return (st, tt, addFun ft (FunC (Id id) tp params stmt))--criar funcao
playProgram (VarP (VarDecl type0 [])) st tt ft = return (st, tt, ft)
playProgram (VarP (VarDecl (StructAux name) l)) st tt ft = let (Struct (Id n) s) = (findType name tt) in
                                                               if (n == " Not Found")
                                                               then error $ "type \"" ++ name ++"\" not declared"
                                                               else playProgram (VarP (VarDecl (Struct (Id n) s) l)) st tt ft
playProgram (VarP (VarDecl type0 (h:t))) st tt ft = let (n, _, _) = findSymb (st, Null) (getName h) in
                                                        if (n == " Not Found")
                                                        then playProgram (VarP (VarDecl type0 t)) ((varToSymbol type0 h (st, Null)) : st) tt ft
                                                        else error $ "variable " ++ n ++ " already declared"
playProgram ( StructDecl (Struct (Id name) l)) st tt ft = let (Struct (Id n) _) = (findType name tt) in
                                                              if (n == " Not Found") then return (st, ((Struct (Id name) l):tt), ft)
                                                              else error $ "type \"" ++ name ++"\" already declared"
playProgram _ st tt ft = return (st, tt, ft)

m :: String -> IO ()
m f = do {playProgram (principal2 f) [] [] []; return ()}

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