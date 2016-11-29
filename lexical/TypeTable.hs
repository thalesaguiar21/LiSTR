module Lexical.TypeTable where

import Lexical.Parser

type TypeTable = [Type]

findType :: String -> TypeTable -> Type
findType _ [] = (Struct (Id " Not Found") [])
findType n0 ((Struct (Id id) l):t) = if(id == n0) then Struct (Id id) l
                                     else findType n0 t