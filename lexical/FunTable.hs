import Parser
import VarTable

import System.IO
import System.IO.Unsafe
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Typeable

type Parametro = (Name, Type)
type Fun = (Name, [Parametro], Type, Escope)

funER    = ("ERROR", [("ERROR", Integer)], Integer, 45)
fun1     = ("f1", [("pI", Integer)], Integer, 1)
pI       = ("pI", (IntV 0), Integer, 1)
myP      = ("pI", Char)
symTable = [("v1", (IntV 2), Integer, 0)]
funTable = [fun1]


findFun :: [Fun] -> Fun -> Fun
findFun [] _         = ("", [], Integer, -1)
findFun (f:funs) fun = let (fNm, _, _, fEs)     = f
                           (funNm, _, _, funEs) = fun
                       in if fNm == funNm && fEs == funEs then fun
                              else findFun funs fun

addFun :: [Symbol] -> [Fun] -> Fun -> ([Symbol], [Fun])
addFun symTab [] fun       = let (nm, params, t, e) = fun
                                 cntSymTab = addSymbols symTab [paramToSymbol param e | param <- params]
                             in  (cntSymTab, [fun])
addFun symTab funTable fun = if findFun funTable fun == ("", [], Integer, -1)
                                 then let (nm, params, t, e) = fun
                                          cntSymTab = addSymbols symTab [paramToSymbol param e | param <- params]
                                      in  (cntSymTab, fun:funTable) 
                                 else (symTab, funTable)

rmFun :: [Symbol] -> [Fun] -> (Name, Escope) -> ([Symbol], [Fun])
rmFun symTab [] _ = (symTab, [])
rmFun symTab (f:funTab) (nm, es) = let (fNm, _, _, fEs) = f
                                   in if fNm == nm && fEs == es
                                          then do let cntSymTab = rmSymb symTab es
                                                  (cntSymTab, funTab)
                                          else let (symT, funT) = rmFun symTab funTab (nm, es)
                                               in (symT, f:funT)
    
paramToSymbol :: Parametro -> Escope -> Symbol
paramToSymbol (pName, Integer) e = (pName, IntV 0, Integer, e)
paramToSymbol (pName, Float) e   = (pName, FloatV 0.0, Float, e)
paramToSymbol (pName, String) e  = (pName, StringV "", String, e)
paramToSymbol (pName, Char) e    = (pName, CharV ' ', Char, e)