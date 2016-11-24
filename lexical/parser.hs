module Lex where

import System.IO
import System.IO.Unsafe
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data P = FunP FunDecl | VarP VarDecl | P [P] deriving Show
data FunDecl = Fun Type Id ParamDecl Stmt deriving Show
data Stmt = AtribS Atrib | IfS If {-| ForS ForRule-} | While LogicExp Stmt | VarS VarDecl | FunS FunCall | Return Exp | Break | Continue | Write [Exp] | Read [Id] | Stmts [Stmt] deriving Show
data Atrib = Atrib Id Assign Exp {-| AtribPlus String += Exp1-} deriving Show
data Exp = AExp ArithmeticExp | LExp LogicExp deriving Show
data ArithmeticExp = Exp1 BinaryOp1 ArithmeticExp ArithmeticExp | Exp2 BinaryOp2 ArithmeticExp ArithmeticExp | ExpId String | Neg ArithmeticExp | FunE FunCall | Const Value | Post PostFixOp Id | Pre PreFixOp Id deriving Show
data BinaryOp1 = Add | Sub deriving Show
data BinaryOp2 = Prod | Div | Mod | VecProd deriving Show
data Assign = Assign | AssignPlus | AssignMinus deriving Show
data PostFixOp = PlusPlusPost | MinusMinusPost deriving Show
data PreFixOp = Negate | PlusPlusPre | MinusMinusPre deriving Show
data Id = Id String deriving Show
data Type = Type String deriving Show
data Value = IntV Integer | FloatV Double | CharV Char | StringV String deriving Show
data If = If LogicExp Stmt | IfElse LogicExp Stmt Stmt deriving Show
data LogicExp = LogicExp LogicOp ArithmeticExp ArithmeticExp | BoolExp BoolOp LogicExp LogicExp | Not LogicExp | LogicConst Bool | BoolId String deriving Show
data LogicOp = Lt | Gt | LEq | GEq | Eq | Diff deriving Show
data BoolOp = And | Or deriving Show
data VarDecl = VarDecl Type [IdOrAtrib] deriving Show
data IdOrAtrib = IdOrAtribI Id | IdOrAtribA Atrib deriving Show
data ParamDecl = ParamDecl [(Type, Id)] deriving Show
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
								,"read", "write"]
              , caseSensitive = False}

TokenParser{ parens = m_parens
		   , braces = m_braces
           , identifier = m_identifier
		   , integer = m_integer
		   , float = m_float
		   , charLiteral = m_char
		   , stringLiteral = m_string
           , reservedOp = m_reservedOp
           , reserved = m_reserved
		   , semi = m_semi
           , semiSep = m_semiSep
           , semiSep1 = m_semiSep1
		   , comma = m_comma
		   , commaSep = m_commaSep
		   , commaSep1 = m_commaSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

expparser :: Parser ArithmeticExp
expparser = try(buildExpressionParser table term) <?> "expression"

table = [ [Prefix (m_reservedOp "-" >> return (Neg))]--Alterar ordem para mudar a precedência
		, [Infix (m_reservedOp "*" >> return (Exp2 Prod)) AssocLeft]
		, [Infix (m_reservedOp "/" >> return (Exp2 Div)) AssocLeft]
		, [Infix (m_reservedOp "#" >> return (Exp2 VecProd)) AssocLeft]
		, [Infix (m_reservedOp "%" >> return (Exp2 Mod)) AssocLeft]
		, [Infix (m_reservedOp "+" >> return (Exp1 Add)) AssocLeft]
		, [Infix (m_reservedOp "-" >> return (Exp1 Sub)) AssocLeft]
        ]

term = m_parens expparser
	   <|> try(expparser_aux)
	   <|> do { f <- funcallparser; return (FunE f)}
       <|> fmap ExpId m_identifier

expparser_aux = try(do { float <- m_float;
					; return (Const (FloatV float))
					})
			 <|> do { int <- m_integer;--Devido ao - funcionar como operador unario, coisas como -10 sao interpretadas como Neg const Type 10 e nao const Type 10
					; return (Const (IntV int))
					}
			 <|> do { char <- m_char;
					; return (Const (CharV char))
					}
			 <|> do { str <- m_string;
					; return (Const (StringV str))
					}
			 <|> do { id <- m_identifier
					; inc  <- ((m_reservedOp "++" >> return (PlusPlusPost)) <|> (m_reservedOp "--" >> return (MinusMinusPost)))
					; return (Post inc (Id id))
					}

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
       <|> fmap BoolId m_identifier

logicexpparser_aux = 
	do exp1 <- expparser
	   rop  <- relationalop
	   exp2 <- expparser
	   return (LogicExp rop exp1 exp2)

relationalop = (m_reservedOp "<"  >> return (Lt))
		   <|> (m_reservedOp "<=" >> return (LEq))
		   <|> (m_reservedOp ">"  >> return (Gt))
		   <|> (m_reservedOp ">=" >> return (GEq))
		   <|> (m_reservedOp "==" >> return (Eq))
		   <|> (m_reservedOp "!=" >> return (Diff))

exp_parser :: Parser Exp
exp_parser = do { a <- expparser; return (AExp a)}
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
					   ; return (VarDecl (Type t) (ids))
					   }
			 where id1 = do { a <- atribparser; return (IdOrAtribA a)}
					 <|> do { id <- m_identifier; return (IdOrAtribI (Id id))}

atribparser :: Parser Atrib
atribparser = try(do { id <- m_identifier
				 ; assign <- assignop
				 ; e <- exp_parser
				 ; return (Atrib (Id id) assign e)
				 })

funcallparser :: Parser FunCall
funcallparser = try(
					do { id <- m_identifier
					   ; e <- m_parens (m_commaSep exps)
					   ; return (FunCall (Id id) (Param e))
					   }
					)
					where exps = do{ e <- exp_parser; return e}

stmtparser :: Parser Stmt
stmtparser = do { fmap Stmts (stmt1 `endBy` m_semi)}
stmt1 = do { m_reserved "return"; e <- exp_parser; return (Return e)}
	  <|> do { a <- atribparser; return (AtribS a)}
	  <|> do { f <- funcallparser; return (FunS f)}
	  <|> do { v <- vardeclparser; return (VarS v)}
	  <|> ifparser
	  <|> whileparser
	  <|> do { m_reserved "break"; return Break}
	  <|> do { m_reserved "continue"; return Continue}
	  <|> do { m_reserved "read"
			 ; ids <- many (do{ id <- m_identifier; return (Id id) })
			 ; return (Read ids)
			 }
	  <|> do { m_reserved "write"
			 ; e <- many (exps)
			 ; return (Write e)
			 }
			 where exps = do{ e <- exp_parser; return e}

fundeclparser :: Parser P
fundeclparser = try (
					do { funtype <- m_identifier
					   ; funid <- m_identifier
					   ; p <- m_parens (m_commaSep params)
					   ; stmt <- m_braces stmtparser
					   ; return (FunP (Fun (Type funtype) (Id funid) (ParamDecl p) stmt))
					   }
					)
				where params = do { t <- m_identifier
								  ; id <- m_identifier
								  ; return (Type t, Id id)
								  }

programparser :: Parser P
programparser = do { fmap P (many (do{ x <- p1; return x }))}
p1 = fundeclparser
 <|> do { v <- vardeclparser; return (VarP v)}

mainparser :: Parser P
mainparser = m_whiteSpace >> programparser <* eof

play :: String -> IO ()
play inp = case parse mainparser "" inp of
             { Left err -> print err
             ; Right ans -> print ans
             }

principal :: String -> IO ()
principal fn =  play (unsafePerformIO (leia fn))

leia fn = do x <- openFile fn ReadMode
             y <- hGetContents x
             return (y)

--Exemplo de como usar parsec abaixo
--link para tutorial https://wiki.haskell.org/Parsing_expressions_and_statements
--outro tutorial https://wiki.haskell.org/Parsing_a_simple_imperative_language
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
expparser :: Parser Expr
expparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens expparser
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
                     ; e <- expparser
                     ; return (v := e)
                     }
              <|> do { m_reserved "if"
                     ; b <- expparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- expparser
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