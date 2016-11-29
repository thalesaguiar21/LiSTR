module Lexical.VarTable where

import Lexical.Parser

type Name = String
type Symbol = (Name, Value, Type)
data SymTableOrNull = SymTable SymTable | Null deriving Show
type SymTable = ([Symbol], SymTableOrNull)

findSymb :: SymTable -> Name -> Symbol
findSymb ([], Null) _ = (" Not Found", (IntV (-1)), Int)
findSymb ([], SymTable anc) name = findSymb anc name
findSymb (h:t, anc) n0 = let (n1, _, _) = h in
                             if (n0 == n1) then h
                             else findSymb (t, anc) n0