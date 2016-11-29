module Lexical.FunTable where

import Lexical.Parser
import Lexical.VarTable
import Lexical.Interpreter (typeToValue)

import Data.Typeable

type FunTable = [Fun]

data Fun = FunC Name Type [(Name, Type)] [Stmt] | Proc' Name [(Name, Type)] [Stmt]

instance Show (Fun) where
    show (FunC n1 t1 p1 _ ) = show t1 ++ " " ++ n1 ++ "(" ++ (printParam p1) ++ "); "
    show (Proc' n1 p1 _ ) = n1 ++ "(" ++ (printParam p1) ++ ");"

printParam :: [(Name, Type)] -> String
printParam [] = ""
printParam (h:t) =  let pn = fst h
                        pt = snd h
                        virg = if (length t)>0 then ", " else ""
                    in show pt ++ " " ++ pn ++ virg ++ (printParam t)

instance Eq (Fun) where
    (FunC _ _ _ _ ) == (Proc' _ _ _ ) = False
    (Proc' _ _ _ ) == (FunC _ _ _ _ ) = False
    (FunC n1 t1 p1 _ ) == (FunC n2 t2 p2 _ ) = if (n1==n2) then True else False
    (Proc' n1 p1 _ ) == (Proc' n2 p2 _ ) = if (n1==n2) then True else False


addFun :: FunTable -> Fun -> FunTable
addFun ft f = if (elem f ft) then ft else (ft ++ [f])

regParams :: SymTable -> [(Name, Type)] -> SymTable
regParams sTab []                          = sTab
regParams (sTab, sTabNull) ((nm, tp):funs) = if fun `elem` sTab 
                                                then error "SymTable :: variable already decalred!"
                                                else (fun:fst(regParams (sTab, sTabNull) funs), sTabNull)
                                             where fun = (nm, (typeToValue tp), tp)

rmFun :: FunTable -> Fun -> FunTable
rmFun [] f = []
rmFun (h:t) f = if h==f then t else [h]++(rmFun t f)


findFun :: FunTable -> Name -> Fun
findFun [] n = error ("findFun:: Function " ++ n ++ " not declared.")
findFun ((FunC name t p s):c) n =   if name==n then (FunC name t p s)
                                    else (findFun c n)
findFun ((Proc' name p s):c) n =    if name==n then (Proc' name p s)
                                    else (findFun c n)

mySTB = ([], Null)
f1 = FunC "func1" Float [("p1", Float), ("p2", Char)] [] --[ VarS (VarDecl (VarDecl Float [(IdOrAtribI (Id "yay"))])) ]
f2 = FunC "func2" Float [("p1", Float), ("p2", Char)] []
f3 = FunC "func3" Float [("p1", Float)] []
f4 = FunC "func4" Float [] []
f5 = Proc' "proced1" [("p", Int), ("p2", Int)] []
f6 = FunC "func5" Float [("p1", Float), ("p", Int)] []
f7 = Proc' "proced2" [("p1", Int), ("p2", Char)] []
listf = []

