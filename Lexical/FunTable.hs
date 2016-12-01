module Lexical.FunTable where

import Lexical.Parser
import Lexical.VarTable
import Lexical.Types (typeToValue)

import Data.Typeable

type FunTable = [Fun]

data Fun = FunC Id Type [(Type, Id, Bool)] Stmt | Proc' Id [(Type, Id, Bool)] Stmt

instance Show (Fun) where
    show (FunC n1 t1 p1 _ ) = show t1 ++ " " ++ show n1 ++ "(" ++ (printParam p1) ++ "); "
    show (Proc' n1 p1 _ )   = show n1 ++ "(" ++ (printParam p1) ++ ");"

printParam :: [(Type, Id, Bool)] -> String
printParam [] = ""
printParam (h:t) =  let (pt, pn, pp) = h
                        virg = if (length t)>0 then ", " else ""
                        passType = if pp then "" else "inout"
                    in show pt ++ " " ++ show pn ++ " " ++ passType ++ virg ++ (printParam t)

instance Eq (Fun) where
    (FunC fn _ _ _ )   == (Proc' pn _ _ )    = fn == pn
    (Proc' pn _ _ )    == (FunC fn _ _ _ )   = pn == fn
    (FunC n1 t1 p1 _ ) == (FunC n2 t2 p2 _ ) = if (n1==n2) then True else False
    (Proc' n1 p1 _ )   == (Proc' n2 p2 _ )   = if (n1==n2) then True else False


addFun :: FunTable -> Fun -> FunTable
addFun ft f = if (elem f ft) then ft else (ft ++ [f])

regParams :: SymTable -> [(Type, Id, Bool)] -> SymTable
regParams sTab []                          = sTab
regParams (sTab, sTabNull) ((tp, nm, pp):funs) = if fun `elem` sTab 
                                                    then error "SymTable :: variable already decalred!"
                                                    else (fun:fst(regParams (sTab, sTabNull) funs), sTabNull)
                                                 where fun = ((show nm), (typeToValue tp), tp)

rmFun :: FunTable -> Fun -> FunTable
rmFun [] f = []
rmFun (h:t) f = if h==f then t else [h]++(rmFun t f)


findFun :: FunTable -> Id -> Fun
findFun [] n = (Proc' (Id " Not found") [] (Stmts []))
findFun ((FunC name t p s):c) n =   if name==n then (FunC name t p s)
                                           else (findFun c n)
findFun ((Proc' name p s):c) n =    if name==n then (Proc' name p s)
                                           else (findFun c n)