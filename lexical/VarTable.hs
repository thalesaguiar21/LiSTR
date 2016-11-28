module Lexical.VarTable where

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