import Parser

import System.IO
import System.IO.Unsafe
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Typeable
    
type Name = String
type Escope = Integer
type Symbol = (Name, Value, Type, Escope)

findSymb :: [Symbol] -> (Name, Escope) -> Symbol
findSymb [] _ = ("", (IntV (-1)), Integer, -1)
findSymb (h:t) (n0, e0) = let (n1, _, _, e1) = h in
                        if ((n0 == n1) && (e0 == e1)) then h
                        else findSymb t (n0, e0) 

add :: [Symbol] -> Symbol -> [Symbol]
add [] symb = [symb]
add table symb = let (n, _, _, e) = symb
                     found = findSymb table (n, e)
                 in if found == ("", (IntV (-1)), Integer, -1) then symb:table
                    else table

addSymb :: [Symbol] -> [Name] -> String -> [Symbol]
addSymb [] ids t = [(id, IntV 0, stringToType t, 0) | id <- ids]
addSymb st ids t = st ++ [(id, IntV 0, stringToType t, 0) | id <- ids]

rmSymb :: [Symbol] -> Escope -> [Symbol]
rmSymb [] _ = []
rmSymb (h : t) e0 = let (_, _, _, e1) = h in
                        if (e0 == e1) then rmSymb t e0
                        else h : rmSymb t e0   
    
updateSymb :: [Symbol] -> (Name, Escope) -> Value -> [Symbol]
updateSymb [] _ _         = []
updateSymb table symb val = let (n, v, t, e) = findSymb table symb
                            in if (n, v, t, e) == ("", (IntV (-1)), Integer, -1) then table
                               else updateAux table (n, v, t, e) val

updateAux :: [Symbol] -> Symbol -> Value -> [Symbol]
updateAux [] _ _ = []
updateAux (h:table) symb val = let (n, v, t, e) = h
                                   (tn, tv, tt, te) = symb
                               in if n == tn && t == tt 
                                  then do let h = (tn, val, tt, te)
                                          h:table
                                  else h : updateAux table symb val


superType :: Type -> Type -> Bool
superType Char Char = True
superType Char _ = False
superType Integer Integer = True
superType Integer _ = False
superType Float Integer = True
superType Float Float = True 
superType Float _ = False
superType String Char = True
superType String String = True
superType String _ = False

--funToSymbol :: FunDecl -> Symbol

eval :: Exp -> (Value, Type)
eval _ = (IntV 0, Integer)

varToSymbol :: Type -> IdOrAtrib -> Symbol
varToSymbol Integer (IdOrAtribI (Id id)) = (id, IntV 0, Integer, 0)
varToSymbol Float (IdOrAtribI (Id id)) = (id, FloatV 0.0, Float, 0)
varToSymbol String (IdOrAtribI (Id id)) = (id, StringV "", String, 0)
varToSymbol Char (IdOrAtribI (Id id)) = (id, CharV ' ', Char, 0)
varToSymbol t0 (IdOrAtribA (Atrib (Id id) Assign exp)) = let (v, t1) = eval exp in
                                                                   if (superType t0 t1) then (id, v, t0, 0)
                                                                   else error ((show t0) ++ " not compatible with " ++ (show t1))

play3 :: P -> [Symbol]
play3 (P []) = []
play3 (P (h:t)) = (play3 h) ++ (play3 (P t))
play3 (FunP f) = {-funToSymbol f :-} []
play3 (VarP (VarDecl t [])) = []
play3 (VarP (VarDecl t0 (h:t1))) = (varToSymbol t0 h) : (play3 (VarP (VarDecl t0 t1)))

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