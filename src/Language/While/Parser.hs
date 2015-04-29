module Language.While.Parser
       ( parseProgram
       ) where

import           Control.Applicative

import           Text.Parsec            (alphaNum, letter, parse, ParseError)
import           Text.Parsec.Combinator (eof)
import qualified Text.Parsec.Expr       as Ex
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as Tok

import qualified Language.While.Syntax  as S

langDef :: Tok.LanguageDef ()
langDef = emptyDef
        { Tok.commentStart    = "/*"
        , Tok.commentEnd      = "*/"
        , Tok.commentLine     = "//"
        , Tok.nestedComments  = True
        , Tok.identStart      = letter
        , Tok.identLetter     = alphaNum
        , Tok.reservedNames   = ["if", "then", "else", "while", "do", "true", "false", "skip"]
        , Tok.reservedOpNames = ["&&", "||", "==", "<=", "not", "-", "+", "*", ":="]
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

prefix op f = Ex.Prefix (reservedOp op >> return f)

binary op f = Ex.Infix (reservedOp op >> return f) Ex.AssocLeft

aexp :: Parser S.AExp
aexp = Ex.buildExpressionParser table term
  where
    table =
      [ [ prefix "-" (S.Minus (S.Lit 0)) ]
      , [ binary "*" S.Multiply ]
      , [ binary "-" S.Minus, binary "+" (\l r -> l `S.Minus` (S.Lit 0 `S.Minus` r)) ]
      ]
    term =  parens aexp
        <|> S.Lit <$> int
        <|> S.Var <$> identifier


bexp :: Parser S.BExp
bexp = Ex.buildExpressionParser table vals
  where
    table =
      [ [ prefix "not" S.Not ]
      , [ binary "&&" S.And ]
      , [ binary "||" (\l r -> S.Not (S.And (S.Not l) (S.Not r))) ]
      ]
    vals =  (reserved "true" >> return S.Tr)
        <|> (reserved "false" >> return (S.Not S.Tr))
        <|> cmp "<=" S.LEQ
        <|> cmp "==" (\l r -> S.And (S.LEQ l r) (S.LEQ r l))
    cmp op f = do
      l <- aexp
      reservedOp op
      r <- aexp
      return (f l r)


com :: Parser S.Com
com = Ex.buildExpressionParser table atoms
  where
    table = [ [ Ex.Infix (reservedOp ";" >> return S.Sequence) Ex.AssocRight ] ]
    atoms =  (reserved "skip" >> return S.Skip)
         <|> assignment
         <|> ifThenElse
         <|> while
    assignment = do
      name <- identifier
      reservedOp ":="
      val <- aexp
      return (S.Assignment name val)
    ifThenElse = do
      reserved "if"
      cond <- parens bexp
      reserved "then"
      then' <- com
      reserved "else"
      else' <- com
      return (S.If cond then' else')
    while = do
      reserved "while"
      cond <- parens bexp
      reserved "do"
      body <- com
      return (S.While cond body)


parseProgram :: String -> Either ParseError S.Com
parseProgram s = parse (contents com) "program" s
