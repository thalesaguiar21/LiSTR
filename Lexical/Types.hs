module Lexical.Types where

import Lexical.VarTable
import Lexical.Parser
import DataTypes.Racional

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

atribListSymb :: [Symbol] -> SymTable -> SymTable
atribListSymb [] st = st
atribListSymb ((name, value, typo):t) st = atribListSymb t (atribSymb (Id name) Assign (value, typo) st)

updateSymTable :: SymTable -> [(Type, Id, Bool)] -> [Exp] -> SymTable
updateSymTable (atual, Null) pr p = (atual, Null)
updateSymTable (atual, SymTable (anc, t)) pr p =  let inout = [ x | x<-pr, (ter x)]
                                                      aux = [y | (x, y) <- zip pr p, (ter x)]
                                                      ios = [ let (_, nmformal, _) = (inout!!i)
                                                                  (AExp (ExpId nmreal)) = (aux!!i)
                                                                  (_, v, _) = findSymb (atual, Null) (show nmformal)
                                                              in
                                                                  ((show nmreal), v, (pri (inout!!i))) | i<-[0..(length inout)-1] ]
                                                  in
                                                      (atual, SymTable (atribListSymb ios (anc, t)))

convertToType :: Value -> Type -> Type -> Value
convertToType (CharV c) Char String = StringV (c : [])
convertToType (IntV n) Int Racional = RacionalV (PRacional n 1)
convertToType (IntV n) Int Float = FloatV (fromIntegral n)
convertToType (RacionalV (PRacional n m)) Racional Int = IntV (n `div` m)
convertToType (RacionalV r) Racional Float = FloatV ((fromIntegral (numerator r)) / (fromIntegral (denominator r)))
convertToType (FloatV f) Float Int = IntV (truncate f)
convertToType (FloatV f) Float Racional = error $ "Ainda nao foi implementado"
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
divide (RacionalV n, Racional) (RacionalV m, Racional) = (RacionalV (divR n m), Racional)
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
