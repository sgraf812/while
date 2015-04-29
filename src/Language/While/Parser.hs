module Language.While.Parser where

import Control.Applicative

import           Text.Parsec.String (Parser)
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec (letter, alphaNum)
import Text.Parsec.Combinator (eof)
import qualified Text.Parsec.Expr   as Ex
import qualified Text.Parsec.Token  as Tok

import qualified Language.While.Syntax as S

langDef :: Tok.LanguageDef ()
langDef = emptyDef
        { Tok.commentStart    = "/*"
        , Tok.commentEnd      = "*/"
        , Tok.commentLine     = "//"
        , Tok.nestedComments  = True
        , Tok.identStart      = letter
        , Tok.identLetter     = alphaNum
        , Tok.reservedNames   = ["if", "then", "else", "while", "do", "true", "false", "skip"]
        , Tok.reservedOpNames = ["&&", "==", "<=", "not", "-", "+", "*", ":="]
        , Tok.caseSensitive   = True
        }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

int :: Parser Int
int = fromIntegral <$> Tok.integer lexer


contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  whiteSpace
  eof
  return r


infixOp :: String -> (S.AExp -> S.AExp -> S.AExp) -> Parser S.AExp
infixOp op ctor = do
  lhs <- aexp
  reservedOp op
  rhs <- aexp
  return (ctor lhs rhs)

aexp :: Parser S.AExp
aexp = Ex.buildExpressionParser table term
  where
    table =
      [ [prefix "-" (S.Minus (S.Lit 0))]
      , [binary "*" S.Multiply]
      , [binary "-" S.Minus, binary "+" (\l r -> l `S.Minus` (S.Lit 0 `S.Minus` r))]
      ]
    prefix op f = Ex.Prefix (reservedOp op >> return f)
    binary op f = Ex.Infix (reservedOp op >> return f) Ex.AssocLeft
    term =  parens aexp
        <|> S.Lit <$> int
        <|> S.Var <$> identifier

{-
bexp :: Parser S.BExp
bexp =  tr
    <|> fl
    <|> leq
    <|> not
    <|> and
  where
    tr = reserved "true" *> pure S.Tr
    fl = reserved "false" *> pure (S.Not S.Tr)
    leq = infixOp "<=" S.LEQ
    not = do
      reservedOp "not"

-}
