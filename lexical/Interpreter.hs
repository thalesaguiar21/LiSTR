import Lexical.Types
import Lexical.Parser
import DataTypes.Racional
import Lexical.TypeTable
import Lexical.VarTable

import System.IO
import System.IO.Unsafe

import Data.Char
import Text.Parsec
import Text.Parsec.String

typeToValue :: Type -> Value
typeToValue Bool = BoolV False
typeToValue Char = CharV ' '
typeToValue Int = IntV 1
typeToValue Racional = RacionalV (PRacional 0 0)
typeToValue Float = FloatV 0.0
typeToValue String = StringV ""

listTypeToValue :: [(Type, Id)] -> [Value]
listTypeToValue l = map (typeToValue . fst) l

varToSymbol :: Type -> IdOrAtrib -> SymTable -> Symbol
varToSymbol (Struct name l) (IdOrAtribI (Id id)) _ = (id, StructV (Struct name l) (listTypeToValue l), Struct name l)
varToSymbol t (IdOrAtribI (Id id)) _ = (id, (typeToValue t), t)
varToSymbol type0 (IdOrAtribA (Atrib (Id id) Assign exp)) st = let (v, type1) = eval exp st in
                                                                   atrib (id, (IntV 0), type0) (v, type1)

playWhile1 :: LogicExp -> Stmt -> SymTable -> TypeTable -> (IO (SymTable, TypeTable))
playWhile1 l s st tt = if (evalL l st)
                       then playWhile2 l (Stmts []) s st tt
                       else return (st, tt)

playWhile2 :: LogicExp -> Stmt -> Stmt -> SymTable -> TypeTable -> (IO (SymTable, TypeTable))
playWhile2 l done (Stmts []) st tt = playWhile1 l done st tt
playWhile2 l (Stmts done) (Stmts (Continue:t)) st tt = playWhile1 l (Stmts (done ++ t)) st tt
playWhile2 _ _ (Stmts (Break:t)) st tt = return (st, tt)
playWhile2 l done (Stmts ((VarS v): t)) st tt = do { (s0, t0) <- playStmt (VarS v) st tt
                                                   ; playWhile2 l done (Stmts t) s0 t0--don't declare the variable again
                                                   }
playWhile2 l (Stmts done) (Stmts (h:t)) st tt = do { (s0, t0) <- playStmt h st tt
                                           ; playWhile2 l (Stmts (done ++ [h])) (Stmts t) s0 t0
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

playStmt :: Stmt -> SymTable -> TypeTable -> (IO (SymTable, TypeTable))
playStmt (Stmts []) st tt = return (st, tt)
playStmt (Stmts (h:t)) st tt = do { (s0, tt0) <- playStmt h st tt
                                  ; (s1, tt1) <- playStmt (Stmts t) s0 tt
                                  ; return (s1, tt1)
                                  }
playStmt (VarS (VarDecl (StructAux name) l)) st tt = let (Struct (Id n) s) = (findType name tt) in
                                                         if (n == " Not Found")
                                                         then error $ "type \"" ++ name ++"\" not declared"
                                                         else playStmt (VarS (VarDecl (Struct (Id n) s) l)) st tt
playStmt (VarS (VarDecl type0 [])) st tt = return (st, tt)
playStmt (VarS (VarDecl type0 (h:t))) (st, anc) tt = let (n, _, _) = (findSymb (st, Null) (getName h)) in
                                                         if (n == " Not Found")
                                                         then playStmt (VarS (VarDecl type0 t))
                                                                      (((varToSymbol type0 h (st, anc)) : st), anc) tt
                                                         else error $ "variable " ++ n ++" already declared"
playStmt (AtribS (Atrib (Id id) assign exp)) st tt = return ((atribSymb id assign (eval exp st) st), tt)
playStmt (IfS (If exp stmt)) st tt = if (evalL exp st)
                                     then do { (s, t) <- playStmt stmt ([], SymTable st) tt
                                             ; return ((ancestor s), t)
                                             }
                                     else return (st, tt)
playStmt (IfS (IfElse exp stmt0 stmt1)) st tt = if (evalL exp st)
                                                then do { (s, t) <- playStmt stmt0 ([], SymTable st) tt
                                                        ; return ((ancestor s), t)
                                                        }
                                                else do { (s, t) <- playStmt stmt1 ([], SymTable st) tt
                                                        ; return ((ancestor s), t)
                                                        }
playStmt (While exp stmt) st tt = do { (s0, t0) <- (playWhile1 exp stmt ([], SymTable st) tt)
                                     ; return ((ancestor s0), t0)
                                     }
playStmt (Read []) st tt = error "read must receive an argument"
playStmt (Read ids) st tt = do { ist <- readT ids [] st
                               ; return ((snd ist), tt)
                               }
playStmt (Write []) st tt = error "write must receive an argument"
playStmt (Write exps) st tt = write exps st >> return (st, tt)
playStmt _ st tt = return (st, tt)

ancestor :: SymTable -> SymTable
ancestor (_, SymTable st) = st
ancestor (_, _) = ([], Null)

getName :: IdOrAtrib -> Name
getName (IdOrAtribI (Id id)) = id
getName (IdOrAtribA (Atrib (Id id) _ _)) = id

playProgram :: P -> [Symbol] -> TypeTable -> (IO ([Symbol], TypeTable))
playProgram (P []) st tt = return (st, tt)
playProgram (P (h:t)) st0 tt0 = do {(st1, tt1) <- (playProgram h st0 tt0); playProgram (P t) st1 tt1}
playProgram (FunP (Proc (Id id) (ParamDecl []) stmt)) st0 tt0 = if (id == "main") then 
                                                                   do { (st1, tt1) <- playStmt stmt ([], SymTable (st0, Null)) tt0
                                                                      ; return ( fst( ancestor(st1) ), tt1 )
                                                                      }
                                                                else return (st0, tt0)--Criar procedimento
playProgram (FunP (Proc (Id id) (ParamDecl _) stmt)) st tt = if (id == "main") then
                                                                error ("main must not receive an argument")
                                                             else return (st, tt)--criar procedimento 
playProgram (FunP (Fun _ (Id id) _ _)) st tt = if (id == "main") then error $ "main must be a procedure" else return (st, tt)--criar funcao
playProgram (VarP (VarDecl type0 [])) st tt = return (st, tt)
playProgram (VarP (VarDecl (StructAux name) l)) st tt = let (Struct (Id n) s) = (findType name tt) in
                                                            if (n == " Not Found")
                                                            then error $ "type \"" ++ name ++"\" not declared"
                                                            else playProgram (VarP (VarDecl (Struct (Id n) s) l)) st tt
playProgram (VarP (VarDecl type0 (h:t))) st tt = let (n, _, _) = findSymb (st, Null) (getName h) in
                                                     if (n == " Not Found")
                                                     then playProgram (VarP (VarDecl type0 t)) ((varToSymbol type0 h (st, Null)) : st) tt
                                                     else error $ "variable " ++ n ++ " already declared"
playProgram ( StructDecl (Struct (Id name) l)) st tt = let (Struct (Id n) _) = (findType name tt) in
                                                           if (n == " Not Found") then return (st, ((Struct (Id name) l):tt))
                                                           else error $ "type \"" ++ name ++"\" already declared"
playProgram _ st tt = return (st, tt)

m :: String -> IO ()
m f = do {playProgram (principal2 f) [] []; return ()}

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