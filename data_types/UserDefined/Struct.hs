module Struct where

import qualified Lexical.VarTable as VT
import qualified Lexical.Parser as PS

type Var = (VT.Name, PS.Type)
type Struct = (VT.Name, [Var], VT.Escope)

