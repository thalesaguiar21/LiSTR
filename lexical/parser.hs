module Lex where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data P = FunP FunDecl [P] | VarP VarDecl [P] deriving Show
data FunDecl = Fun Type Id ParamDecl Stmt deriving Show
data Stmt = AtribS Atrib | IfS If {-| ForS ForRule-} | While LogicExp Stmt | VarS VarDecl | FunS FunCall | Return Exp | Break | Continue | Write Exp | Read Exp | Stmts [Stmt] deriving Show
data Atrib = Atrib Id Assign Exp {-| AtribPlus String += Exp1-} deriving Show
data Exp = Exp1 BinaryOp1 Exp Exp | Exp2 BinaryOp2 Exp Exp | ExpId String | Post PostFixOp Id | Pre PreFixOp Id deriving Show
data BinaryOp1 = Add | Sub deriving Show
data BinaryOp2 = Prod | Div | Mod | VecProd deriving Show
data Assign = Assign deriving Show
data PostFixOp = PlusPlusPost | MinusMinusPost deriving Show
data PreFixOp = Negate | PlusPlusPre | MinusMinusPre deriving Show
data Id = Id String deriving Show
data Type = Type String deriving Show
data If = If LogicExp Stmt | IfElse LogicExp Stmt Stmt deriving Show
data LogicExp = LogicExp LogicOp Exp Exp | BoolExp BoolOp LogicExp LogicExp | Not LogicExp | BoolId Id deriving Show
data LogicOp = Lt | Gt | LEq | GEq | Eq | Diff deriving Show
data BoolOp = And | Or deriving Show
{-data ForRule = For111 Type ForAtrib ForComp ForRight Stmt | For110 Type ForAtrib ForComp          Stmt
			 | For101 Type ForAtrib         ForRight Stmt | For100 Type ForAtrib                  Stmt
			 | For011               ForComp ForRight Stmt | For010               ForComp          Stmt
			 | For001                       ForRight Stmt | For000                                Stmt 
			  deriving Show
data ForAtrib = Ids [Id] | IdAssign [Atrib] deriving Show
data ForComp = ForId Id | ForComp LogicExp deriving Show
data ForRight = ForRight [Exp] {-| Id AddAssign Exp1 | Id SubAssign Exp1 | Id AddAssign Exp1 Comma ForRight | Id SubAssign Exp1 Comma ForRight-} deriving Show-}
data VarDecl = VarDecl Type [Id] | VarDeclA Type Id Assign Exp deriving Show
data ParamDecl = ParamDecl [Type] deriving Show
data FunCall = FunCall Id Param deriving Show
data Param = Param [Id] deriving Show

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
								,""]
              , caseSensitive = False}

TokenParser{ parens = m_parens
		   , braces = m_braces
           , identifier = m_identifier
           , reservedOp = m_reservedOp
		   , stringLiteral = m_stringLiteral
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
		   , commaSep1 = m_commaSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

exprparser :: Parser Exp
exprparser = buildExpressionParser table term <?> "expression"

table = [{- [Prefix (m_reservedOp "-" >> return (Pre Negate))]--Alterar ordem para mudar a precedência
		, [Postfix (m_reservedOp "++" >> return (Post PlusPlusPost))]
		, [Postfix (m_reservedOp "--" >> return (Post MinusMinusPost))]
		,-} [Infix (m_reservedOp "*" >> return (Exp2 Prod)) AssocLeft]
		, [Infix (m_reservedOp "/" >> return (Exp2 Div)) AssocLeft]
		, [Infix (m_reservedOp "#" >> return (Exp2 VecProd)) AssocLeft]
		, [Infix (m_reservedOp "%" >> return (Exp2 Mod)) AssocLeft]
		, [Infix (m_reservedOp "+" >> return (Exp1 Add)) AssocLeft]
		, [Infix (m_reservedOp "-" >> return (Exp1 Sub)) AssocLeft]
        ]

term = m_parens exprparser
       <|> fmap ExpId m_identifier

{-logicexprparser :: Parser LogicExp
logicexprparser = buildExpressionParser logictable logicterm <?> "logicexpression"

logictable = [ [Prefix (m_reservedOp "!" >> return (Not))]
			 , [Infix (m_reservedOp "<" >> return (LogicExp Lt)) AssocLeft]
			 , [Infix (m_reservedOp "<=" >> return (LogicExp LEq)) AssocLeft]
			 , [Infix (m_reservedOp ">" >> return (LogicExp Gt)) AssocLeft]
			 , [Infix (m_reservedOp ">=" >> return (LogicExp GEq)) AssocLeft]
			 , [Infix (m_reservedOp "==" >> return (LogicExp Eq)) AssocLeft]
			 , [Infix (m_reservedOp "!=" >> return (LogicExp Diff)) AssocLeft]
			 , [Infix (m_reservedOp "||" >> return (BoolExp Or)) AssocLeft]
			 , [Infix (m_reservedOp "&&" >> return (BoolExp And)) AssocLeft]
			 ]

logicterm = m_parens logicexprparser
       <|> fmap BoolId m_identifier-}

mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Stmts (m_semiSep1 stmt1)
      stmt1 =	  do { m_reserved "return"
					 ; e <- exprparser
					 ; return (Return e)
					 }
              <|> do { id <- m_identifier
                     ; m_reservedOp "="
                     ; e <- exprparser
                     ; return (AtribS (Atrib (Id id) (Assign) e))
                     }
					 --Nao testei, mas acho que a regra IfElse tem que vim antes do If, pois e possivel que a regra do IfElse seja tratada como If
              {-<|> do { m_reserved "if"
					 --; condition <- (m_parens logicexprparser)
                     ; ifblock <- (m_braces stmtparser)
					 ; m_reserved "else"
					 ; elseblock <- (m_braces stmtparser)
                     ; return (IfS (IfElse condition ifblock elseblock))
                     }
              <|> do { m_reserved "if"
					 --; condition <- (m_parens logicexprparser)
                     ; block <- (m_braces stmtparser)
                     ; return (IfS (If condition block))
                     }
              <|> do { m_reserved "while"
                     --; condition <- (m_parens logicexprparser)
                     ; block <- (m_braces stmtparser)
                     ; return (While condition block)
                     }
{-			  <|> do { m_stringLiteral ""
					 ,
					 }-}-}
play :: String -> IO ()
play inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }
--Exemplo abaixo
--link para tutorial https://wiki.haskell.org/Parsing_expressions_and_statements
{-
data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
           | Seq [Stmt]
     deriving Show
def = emptyDef{ commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["~", "&", "=", ":="]
              , reservedNames = ["true", "false", "nop",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od"]
              }
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def
exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))
mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (While b p)
                     }
play :: String -> IO ()
play inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }
-}