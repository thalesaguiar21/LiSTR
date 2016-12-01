module Lexical.Types where

import Lexical.VarTable
import Lexical.Parser
import DataTypes.Racional

import System.IO
import System.IO.Unsafe

import Data.Char
import Text.Parsec
import Text.Parsec.String

atribSymb :: Id -> Assign-> (Value, Type) -> SymTable -> SymTable
atribSymb id _ _ ([], Null) = error ( "variable " ++ (show id) ++ " not declared")
atribSymb id a v ([], SymTable anc) = ([], SymTable (atribSymb id a v anc))
atribSymb (StructId ((Id id0):ids)) Assign (value0, type0) (h:t, anc) = let (id1, value1, type1) = h in
                                                                            if (id0 == id1) then
                                                                               (((id0, (atribStruct ids (value1, type1) (value0, type0)), type1) : t), anc)
                                                                            else let (st, anc1) = atribSymb (StructId ((Id id0):ids)) Assign (value0, type0) (t, anc) in
                                                                                 (h : st, anc1)
atribSymb (Id id0) Assign (value0, type0) (h:t, anc0) = let (id1, _, _) = h in
                                                       if (id0 == id1) then
                                                          (atrib h (value0, type0) : t, anc0)
                                                       else let (st, anc1) = atribSymb (Id id0) Assign (value0, type0) (t, anc0) in
                                                            (h : st, anc1)
atribSymb (Id id0) AssignPlus (value0, type0) ((h:t), anc0) = let (id1, value1, type1) = h in
                                                             if (id0 == id1) then
                                                                (atrib h (add (value1, type1) (value0, type0)) : t, anc0)
                                                             else let (st, anc1) = atribSymb (Id id0) AssignPlus (value0, type0) (t, anc0) in
                                                                  (h : st, anc1)
atribSymb (Id id0) AssignMinus (value0, type0) ((h:t), anc0) = let (id1, value1, type1) = h in
                                                              if (id0 == id1) then
                                                                 (atrib h (sub (value1, type1) (value0, type0)) : t, anc0)
                                                              else let (st, anc1) = atribSymb (Id id0) AssignPlus (value0, type0) (t, anc0) in
                                                                   (h : st, anc1)

atrib :: Symbol -> (Value, Type) -> Symbol
atrib (id, value0, type0) (value1, type1) = if (superType type0 type1) then (id, convertToType value1 type1 type0, type0)
                                            else error ((show type0) ++ " not compatible with " ++ (show type1) ++ " use cast")

splitStruct :: String -> [(Type, Id)] -> [Value] -> ([Value], (Value, Type), [Value])
splitStruct (id) ((t0, (Id id0)):t) (v0:v) = if(id == id0) then ([], (v0, t0), v)
                                        else let (vl, vm, vr) = splitStruct id t v in
                                             ((v0:vl), vm, vr)

atribStruct :: [Id] -> (Value, Type) -> (Value, Type) -> Value
atribStruct [] (value0, type0) (value1, type1) = if (superType type0 type1) then convertToType value1 type1 type0
                                                 else error ((show type0) ++ " not compatible with " ++ (show type1) ++ " use cast")
atribStruct ((Id id):t) (StructV type0 (values), Struct _ l) v = let (vl, vm, vr) = splitStruct id l values in
                                                                     StructV type0 (vl ++ ((atribStruct t vm v) : vr))

convertToType :: Value -> Type -> Type -> Value
convertToType (CharV c) Char String = StringV (c : [])
convertToType (IntV n) Int Racional = RacionalV (PRacional n 1)
convertToType (IntV n) Int Float = FloatV (fromIntegral n)
convertToType (RacionalV (PRacional n m)) Racional Int = IntV (n `div` m)
convertToType (RacionalV r) Racional Float = FloatV ((fromIntegral (numerator r)) / (fromIntegral (denominator r)))
convertToType (FloatV f) Float Int = IntV (truncate f)
convertToType (FloatV f) Float Racional = error $ "Ainda nao foi implementado"--let r = toRational f in RacionalV (PRacional (numerator r) (denominator r))
convertToType v type0 type1 = if (type0 == type1) then v 
                              else error $ "Can't convert " ++ (show type0) ++ " to " ++ (show type1)

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
divide (RacionalV n, Racional) (RacionalV m, Racional) = (RacionalV (n/m), Racional)
divide (FloatV n, Float) (FloatV m, Float) = (FloatV (n / m), Float)
divide v1 v2 = conversion divide v1 v2

modulus :: (Value, Type) -> (Value, Type) -> (Value, Type)
modulus (IntV n, Int) (IntV m, Int) = (IntV (n `mod` m), Int)

conversion :: ((Value, Type) -> (Value, Type) -> (Value, Type)) -> (Value, Type) -> (Value, Type) -> (Value, Type)
conversion op (v0, t0) (v1, t1) = if (superType t0 t1) then op (v0, t0) (convertToType v1 t1 t0, t0)
                                  else if (superType t1 t0) then op (convertToType v0 t0 t1, t1) (v1, t1)
                                       else error $ "error cannot convert from " ++ show t0 ++ " to " ++ show t1

valueToBool :: Value -> Bool
valueToBool (BoolV b) = b
valueToBool _ = error $ "expected bool type"

findInStruct :: Id -> [Value] -> [(Type, Id)] -> (Value, Type)
findInStruct id (v0:v1) ((type0, id0):l) = if (id == id0) then (v0, type0)
                                           else findInStruct id v1 l

getValueFromStruct :: [Id] -> Value -> Type -> (Value, Type)
getValueFromStruct [] v t = (v, t)
getValueFromStruct (h:t) (StructV _ v) (Struct _ l) = let (v0, t0) = findInStruct h v l in
                                                          getValueFromStruct t v0 t0

evalA :: ArithmeticExp -> SymTable -> (Value, Type)
evalA (ExpId (Id id)) st = let (name, value, type0) = findSymb st id in
                          if (name == " Not Found") then error $ "variable " ++ id ++ " not declared"
                          else (value, type0)
evalA (ExpId (StructId ((Id id):ids))) st = let (name, value0, type0) = findSymb st id in
                                 if (name == " Not Found") then error $ "variable " ++ id ++ " not declared"
                                 else getValueFromStruct ids value0 type0
evalA (Const (CharV c)) _ = (CharV c, Char)
evalA (Const (IntV n)) _ = (IntV n, Int)
evalA (Const (RacionalV r)) _ = (RacionalV r, Racional)
evalA (Const (FloatV f)) _ = (FloatV f, Float)
evalA (Const (StringV s)) _ = (StringV s, String)
evalA (Exp Add exp1 exp2) st = add (evalA exp1 st) (evalA exp2 st)
evalA (Exp Sub exp1 exp2) st = sub (evalA exp1 st) (evalA exp2 st)
evalA (Exp Prod exp1 exp2) st = prod (evalA exp1 st) (evalA exp2 st)
evalA (Exp Div exp1 exp2) st = divide (evalA exp1 st) (evalA exp2 st)
evalA (Exp Mod exp1 exp2) st = modulus (evalA exp1 st) (evalA exp2 st)
--evalA (Exp VecProd exp1 exp2) st = vecProd (evalA exp1 st) (evalA exp2 st)
--evalA (Exp FunCall exp1 exp2) st = funCall fun
--operacoes posfixadas e prefixadas nao foram implementadas
evalA (Neg exp) st = sub (IntV 0, Int) (evalA exp st)
evalA exp _ = error $ "couldn't understand the expression " ++ show exp

comp :: (Value -> Value -> Bool) -> (Value, Type) -> (Value, Type) -> Bool
comp op (v0, t0) (v1, t1) = if (t0 == t1) then (op v0 v1) else error $ "the types aren't comparable"

evalL :: LogicExp -> SymTable -> Bool
evalL (BoolId (Id id)) st =  let (name, value, type0) = findSymb st id in
                          if (name == " Not Found") then error $ "variable " ++ id ++ " not declared"
                          else valueToBool value
{-evalL (BoolId id) st =  let (name, value, type0) = findSymb st id in
                          if (name == " Not Found") then error $ "variable " ++ id ++ " not declared"
                          else valueToBool value-}
evalL (LogicConst b) _ = b
evalL (LogicExp Lt exp1 exp2) st = comp (<) (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp Gt exp1 exp2) st = comp (>) (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp LEq exp1 exp2) st = comp (<=) (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp GEq exp1 exp2) st = comp (>=) (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp Eq exp1 exp2) st = comp (==) (evalA exp1 st) (evalA exp2 st)
evalL (LogicExp Diff exp1 exp2) st = comp (/=) (evalA exp1 st) (evalA exp2 st)
evalL (BoolExp And exp1 exp2) st = ((evalL exp1 st) && (evalL exp2 st))
evalL (BoolExp Or exp1 exp2) st = ((evalL exp1 st) || (evalL exp2 st))

eval :: Exp -> SymTable -> (Value, Type)
eval (LExp exp) st = (BoolV (evalL exp st), Bool)
eval (AExp exp) st = evalA exp st

