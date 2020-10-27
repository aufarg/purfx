module Parser where

import qualified Text.Parsec            as P
import qualified Text.Parsec.String     as P(Parser)

import qualified Text.Parsec.Expr       as P
import qualified Text.Parsec.Token      as P

import qualified Lexer                  as L
import Syntax

binary s = P.Infix (L.reservedOp s >> return (BinaryOperator s))

table =
  [ [binary "*" P.AssocLeft, binary "/" P.AssocLeft]
  , [binary "+" P.AssocLeft, binary "-" P.AssocLeft]
  , [binary "==" P.AssocLeft, binary "<" P.AssocLeft, binary ">" P.AssocLeft]
  ]

string :: P.Parser Expression
string = String <$> L.stringLiteral

int :: P.Parser Expression
int = Integer <$> L.integer

floating :: P.Parser Expression
floating = Double <$> L.float

expression :: P.Parser Expression
expression = P.buildExpressionParser table factor

variable :: P.Parser Expression
variable = Variable <$> L.identifier

purefn :: P.Parser Definition
purefn = do
  L.reserved "pure"
  Pure <$> L.identifier <*> L.parens (P.many L.identifier) <*> expression

effect :: P.Parser Definition
effect = do
  L.reserved "effect"
  Effect <$> L.identifier <*> L.parens (P.many L.identifier) <*> expression

extern :: P.Parser Definition
extern = do
  L.reserved "extern"
  Extern <$> L.identifier <*> L.parens (P.many L.identifier)

call :: P.Parser Expression
call = Call <$> L.identifier <*> L.parens (L.commaSep expression)

factor :: P.Parser Expression
factor = P.try floating
      P.<|> P.try int
      P.<|> P.try call
      P.<|> P.try string
      P.<|> P.try conditional
      P.<|> variable
      P.<|> L.parens expression

conditional :: P.Parser Expression
conditional = do
    L.reserved "if"
    cond <- expression
    L.reserved "then"
    expr1 <- expression
    L.reserved "else"
    expr2 <- expression
    return $ Conditional cond expr1 expr2


definition :: P.Parser Definition
definition = P.try purefn P.<|> effect P.<|> P.try extern

contents :: P.Parser a -> P.Parser a
contents p = do
  P.whiteSpace L.lexer
  r <- p
  P.eof
  return r

parseModule :: String -> Either P.ParseError Module
parseModule = P.parse (contents toplevel) "<stdin>"
    where
        toplevel :: P.Parser Module
        toplevel = P.many $ do
            d <- definition
            L.reservedOp ";"
            return d

