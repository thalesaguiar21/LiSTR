module Lexical.Parser where

import DataTypes.Racional

import System.IO
import System.IO.Unsafe
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data P = FunP FunDecl | VarP VarDecl | StructDecl Type | P [P] deriving Show
data FunDecl = Fun Type Id ParamDecl Stmt | Proc Id ParamDecl Stmt deriving Show
data Stmt = AtribS Atrib | IfS If {-| ForS ForRule-} | While LogicExp Stmt | VarS VarDecl | FunS FunCall | Return Exp | Break | Continue | Write [Exp] | Read [Id] | Stmts [Stmt] deriving Show
data Atrib = Atrib Id Assign Exp deriving Show
data Exp = AExp ArithmeticExp | LExp LogicExp deriving Show
data ArithmeticExp = Exp BinaryOp ArithmeticExp ArithmeticExp | ExpId Id | Neg ArithmeticExp | FunE FunCall | Const Value | Post PostFixOp Id | Pre PreFixOp Id deriving Show
data BinaryOp = Add | Sub | Prod | Div | Mod | VecProd deriving Show
data Assign = Assign | AssignPlus | AssignMinus deriving Show
data PostFixOp = PlusPlusPost | MinusMinusPost deriving Show
data PreFixOp = Negate | PlusPlusPre | MinusMinusPre deriving Show
data Id = Id String | StructId [Id] deriving (Eq, Show)
data Type = Int | Float | String | Char | Racional | Bool | StructAux String | Struct Id [(Type, Id)] deriving (Eq, Show)--struct id = type name, [(type, Id)] = field type and id
data Value = IntV Int | FloatV Double | CharV Char | StringV String | RacionalV Racional | BoolV Bool | StructV Type [Value]--structV type = struct type, contains field types and name, value = field value
data If = If LogicExp Stmt | IfElse LogicExp Stmt Stmt deriving Show
data LogicExp = LogicExp LogicOp ArithmeticExp ArithmeticExp | BoolExp BoolOp LogicExp LogicExp | Not LogicExp | LogicConst Bool | BoolId Id deriving Show
data LogicOp = Lt | Gt | LEq | GEq | Eq | Diff deriving Show
data BoolOp = And | Or deriving Show
data VarDecl = VarDecl Type [IdOrAtrib] deriving Show
data IdOrAtrib = IdOrAtribI Id | IdOrAtribA Atrib deriving Show
data ParamDecl = ParamDecl [(Type, Id, Bool)] deriving Show
data FunCall = FunCall Id Param deriving Show
data Param = Param [Exp] deriving Show
{-data ForRule = For111 Type ForAtrib ForComp ForRight Stmt | For110 Type ForAtrib ForComp          Stmt
             | For101 Type ForAtrib         ForRight Stmt | For100 Type ForAtrib                  Stmt
             | For011               ForComp ForRight Stmt | For010               ForComp          Stmt
             | For001                       ForRight Stmt | For000                                Stmt
              deriving Show
data ForAtrib = Ids [Id] | IdAssign [Atrib] deriving Show
data ForComp = ForId Id | ForComp LogicExp deriving Show
data ForRight = ForRight [Exp] {-| Id AddAssign Exp1 | Id SubAssign Exp1 | Id AddAssign Exp1 Comma ForRight | Id SubAssign Exp1 Comma ForRight-} deriving Show-}

instance Show Value where
   show (IntV v) = show v
   show (FloatV v) = show v
   show (RacionalV v) = show v
   show (BoolV v) = if v then "true" else "false"
   show (CharV v) = show v
   show (StringV v) = show v
   show (StructV _ t) = "( " ++ concat (map ((++ ", ") . show) t) ++ " )"

instance Eq Value where
    (BoolV x) == (BoolV y)         = x == y
    (IntV x) == (IntV y)           = x == y
    (RacionalV x) == (RacionalV y) = x == y
    (FloatV x) == (FloatV y)       = x == y
    (CharV x) == (CharV y)         = x == y
    (StringV x) == (StringV y)     = x == y

instance Ord Value where
    IntV x <= IntV y           = x <= y
    RacionalV x <= RacionalV y = x <= y
    FloatV x <= FloatV y       = x <= y

stringToType :: String -> Type
stringToType "bool" = Bool
stringToType "int" = Int
stringToType "float" = Float
stringToType "char" = Char
stringToType "string" = String
stringToType "rational" = Racional
stringToType s = StructAux s

def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , identStart = letter <|> char '_'
              , identLetter = alphaNum <|> char '_'
              , opStart = oneOf "+-*/#><!%^~&|="
              , opLetter = oneOf "+-=&|"
              , reservedOpNames = ["+", "-", "*", "/", "#", ">", "<", "!", "%", "^", "~", "&", "|", "="
                                  ,"+=", "-=", "*=", "/=", "#=", ">=", "<=", "!=", "%=", "^=", "~=", "&=", "|="
                                  ,"==", "++", "--", "&&", "||"]
              , reservedNames = ["true", "false", "if", "else", "while", "return", "continue", "break"
                                ,"read", "write", "proc", "fun", "in", "inout", "struct"]
              , caseSensitive = True}

TokenParser{ parens = m_parens
           , braces = m_braces
           , identifier = m_identifier
           , integer = m_int
           , float = m_float
           , charLiteral = m_char
           , stringLiteral = m_string
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , dot = m_dot
           , semi = m_semi
           , semiSep = m_semiSep
           , semiSep1 = m_semiSep1
           , comma = m_comma
           , commaSep = m_commaSep
           , commaSep1 = m_commaSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

arithmeticexpparser :: Parser ArithmeticExp
arithmeticexpparser = try(buildExpressionParser table term) <?> "expression"

table = [ [Prefix (m_reservedOp "-" >> return (Neg))]--Alterar ordem para mudar a precedencia
        , [Infix (m_reservedOp "*" >> return (Exp Prod)) AssocLeft--Operadores numa mesma lista possuem a mesma precedencia
        ,  Infix (m_reservedOp "/" >> return (Exp Div)) AssocLeft
        ,  Infix (m_reservedOp "#" >> return (Exp VecProd)) AssocLeft
        ,  Infix (m_reservedOp "%" >> return (Exp Mod)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Exp Add)) AssocLeft
        ,  Infix (m_reservedOp "-" >> return (Exp Sub)) AssocLeft]
        ]

term = m_parens arithmeticexpparser
       <|> try(arithmeticexpparser_aux)
       <|> do { f <- funcallparser; return (FunE f)}
       <|> do {id <- idparser; return (ExpId id)}

arithmeticexpparser_aux = try(do { float <- m_float;
                    ; return (Const (FloatV float))
                    })
             <|> do { int <- m_int;
                    ; return (Const (IntV (fromIntegral int)))
                    }
             <|> do { char <- m_char;
                    ; return (Const (CharV char))
                    }
             <|> do { str <- m_string;
                    ; return (Const (StringV str))
                    }

idparser = do { id <- m_identifier
              ; ids <- many (do{m_dot; x <- m_identifier; return (Id x)})
              ; if (null ids) then return (Id id)
                else return (StructId ((Id id) : ids))
              }
                    {-
             <|> do { id <- m_identifier
                    ; inc  <- ((m_reservedOp "++" >> return (PlusPlusPost)) <|> (m_reservedOp "--" >> return (MinusMinusPost)))
                    ; return (Post inc (Id id))
                    }-}

logicexpparser :: Parser LogicExp
logicexpparser = buildExpressionParser logictable logicterm <?> "logicexpression"

logictable = [ [Prefix (m_reservedOp "!" >> return (Not))]
             , [Infix (m_reservedOp "||" >> return (BoolExp Or)) AssocLeft]
             , [Infix (m_reservedOp "&&" >> return (BoolExp And)) AssocLeft]
             ]

logicterm = m_parens logicexpparser
       <|> (m_reserved "true"  >> return (LogicConst True ))
       <|> (m_reserved "false" >> return (LogicConst False))
       <|> try (logicexpparser_aux)
       <|> fmap BoolId idparser

logicexpparser_aux = 
    do exp1 <- arithmeticexpparser
       rop  <- relationalop
       exp2 <- arithmeticexpparser
       return (LogicExp rop exp1 exp2)

relationalop = (m_reservedOp "<"  >> return (Lt))
           <|> (m_reservedOp "<=" >> return (LEq))
           <|> (m_reservedOp ">"  >> return (Gt))
           <|> (m_reservedOp ">=" >> return (GEq))
           <|> (m_reservedOp "==" >> return (Eq))
           <|> (m_reservedOp "!=" >> return (Diff))

expparser :: Parser Exp
expparser = do { a <- arithmeticexpparser; return (AExp a)}
         <|> do { l <- logicexpparser; return (LExp l)}

ifparser :: Parser Stmt
ifparser = do { m_reserved "if"
              ; condition <- (m_parens logicexpparser)
              ; ifblock <- (m_braces stmtparser)
              ; do { m_reserved "else"; 
                   ; elseblock <- (m_braces stmtparser);
                   ; return (IfS (IfElse condition ifblock elseblock))}
            <|> do { return (IfS (If condition ifblock))}
             }

whileparser :: Parser Stmt
whileparser = do { m_reserved "while"
             ; condition <- (m_parens logicexpparser)
             ; block <- (m_braces stmtparser)
             ; return (While condition block)
             }

assignop = (m_reservedOp "+="  >> return (AssignPlus))
       <|> (m_reservedOp "-=" >> return (AssignMinus))
       <|> (m_reservedOp "="  >> return (Assign))

vardeclparser :: Parser VarDecl
vardeclparser = do { t <- m_identifier
                       ; ids <- (m_commaSep id1)
                       ; return (VarDecl (stringToType t) (ids))
                       }
             where id1 = do { a <- atribparser; return (IdOrAtribA a)}
                     <|> do { id <- m_identifier; return (IdOrAtribI (Id id))}

atribparser :: Parser Atrib
atribparser = try(do { id <- idparser
                 ; assign <- assignop
                 ; e <- expparser
                 ; return (Atrib id assign e)
                 })

funcallparser :: Parser FunCall
funcallparser = try(
                    do { id <- m_identifier
                       ; e <- m_parens (m_commaSep exps)
                       ; return (FunCall (Id id) (Param e))
                       }
                    )
                    where exps = do{ e <- expparser; return e}

stmtparser :: Parser Stmt
stmtparser = do { fmap Stmts (many (do{ x <- stmt1; return x }))}
stmt1 = do { m_reserved "return"; e <- expparser; m_semi; return (Return e)}
      <|> do { a <- atribparser; m_semi; return (AtribS a)}
      <|> do { f <- funcallparser; m_semi; return (FunS f)}
      <|> do { v <- vardeclparser; m_semi; return (VarS v)}
      <|> ifparser
      <|> whileparser
      <|> do { m_reserved "break"; m_semi; return Break}
      <|> do { m_reserved "continue"; m_semi; return Continue}
      <|> do { m_reserved "read"
             ; ids <- many (do{ id <- idparser; return id })
             ; m_semi
             ; return (Read ids)
             }
      <|> do { m_reserved "write"
             ; e <- many (exps)
             ; m_semi
             ; return (Write e)
             }
             where exps = do{ e <- expparser; return e}

fundeclparser :: Parser P
fundeclparser = do { m_reserved "fun"
                   ; funtype <- m_identifier
                   ; funid <- m_identifier
                   ; p <- m_parens (m_commaSep params)
                   ; stmt <- m_braces stmtparser
                   ; return (FunP (Fun (stringToType funtype) (Id funid) (ParamDecl p) stmt))
                   }
            <|> do { m_reserved "proc"
                   ; funid <- m_identifier
                   ; p <- m_parens (m_commaSep params)
                   ; stmt <- m_braces stmtparser
                   ; return (FunP (Proc (Id funid) (ParamDecl p) stmt))
                   }
                where params = do { m_reserved "inout"
                                  ; t <- m_identifier
                                  ; id <- m_identifier
                                  ; return (stringToType t, Id id, True)
                                  }
                           <|> do { m_reserved "in"
                                  ; t <- m_identifier
                                  ; id <- m_identifier
                                  ; return (stringToType t, Id id, False)
                                  }
                           <|> do { t <- m_identifier
                                  ; id <- m_identifier
                                  ; return (stringToType t, Id id, False)
                                  }

structparser :: Parser P
structparser = do { m_reserved "struct"
                  ; name <- m_identifier
                  ; f <- m_braces (fields `endBy1` m_semi) 
                  ; m_semi
                  ; return (StructDecl (Struct (Id name) f))
                  }
               where fields = do { t <- m_identifier
                                 ; id <- m_identifier
                                 ; return (stringToType t, Id id)
                                 }

programparser :: Parser P
programparser = do { fmap P (many (do{ x <- p1; return x }))}
p1 = fundeclparser
 <|> structparser
 <|> do { v <- vardeclparser; m_semi; return (VarP v)}

mainparser :: Parser P
mainparser = m_whiteSpace >> programparser <* eof