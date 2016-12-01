module Lexical.VarTable where

import Lexical.Parser

type Name = String
type Symbol = (Name, Value, Type)
data SymTableOrNull = SymTable SymTable | Null deriving Show
type SymTable = ([Symbol], SymTableOrNull)

findSymb :: SymTable -> Name -> Symbol
findSymb ([], Null) _ = (" Not Found", (IntV (-1)), Int)
findSymb (h:t, anc) n0 = let (n1, _, _) = h in
                             if (n0 == n1) then h
                             else findSymb (t, anc) n0
findSymb ([], SymTable anc) name = findSymb anc name


updateSymTable :: SymTable -> [(Type, Id, Bool)] -> SymTable
updateSymTable (atual, Null) pr = (atual, Null)
updateSymTable (atual, SymTable (anc, t)) pr =  let inout = [ x | x<-pr, (ter x)]
                                                    ios = [ let (_, nm, _) = (inout!!i)
                                                                (_, v, _) = findSymb (atual, Null) (show nm)
                                                            in
                                                                ((show nm), v, (pri (inout!!i))) | i<-[0..(length inout)-1] ]
                                                    notInout = [ x | x<-anc, not (elem x ios)]
                                                in
                                                    (atual, SymTable (ios++notInout, t))


pri :: (a, b, c) -> a
pri (n,_,_) = n

seg ::  (a, b, c) -> b
seg (_, v,_) = v

ter ::  (a, b, c) -> c
ter (_,_,t) = t

--addSymb :: SymTable -> Symbol -> SymTable

