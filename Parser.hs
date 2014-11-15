{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Parser (parser, stmt, expr) where

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec as Parsec

import Text.ParserCombinators.Parsec.Combinator (choice, eof, manyTill)
import Text.ParserCombinators.Parsec.Char (letter, digit, char, oneOf, spaces, newline, anyChar)
import Text.ParserCombinators.Parsec.Prim (many)
import Text.ParserCombinators.Parsec (Parser, (<|>), (<?>), sepBy, sepEndBy)

import Control.Monad.State (void)
import Control.Applicative ((*>), (<*), (<$>), (<*>)) 

import Ast


parser :: String -> String -> Either String Statement
parser source text = 
  case Parsec.parse (ignored *> stmt <* (ignored >> eof)) source text of 
    Left error -> Left (show error)
    Right stmt -> Right stmt


stmt = choice . map P.try $ [
    "definition" <??> (SDef  <$> defines)
  , "expression" <??> (SExpr <$> expr)
  ] 


expr = choice . map P.try $ [
    "number"      <??> number
  , "operator"    <??> operator
  , "boolean"     <??> ((XBool . (== 't')) <$> (char '#' *> oneOf "ft"))
  , "nil"         <??> (XNil `when` "nil") 
  , "if"          <??> ifexpr
  , "definition"  <??> (XDef <$> defines)
  , "lambda"      <??> lambdaParser
  , "reference"   <??> (XRef <$> identifier)
  , "application" <??> sexpr
  ]


ifexpr = inParens $ P.string "if" >> ignored >>
  XIf <$> (expr <* ignored)
      <*> (expr <* ignored)
      <*> (expr <* ignored)


sexpr = inParens $
  XApply <$> (ignored *> expr <* ignored)
         <*> (expr `sepEndBy` ignored)


operator = 
  XOperator <$>
    (Add `when` "+" <|> 
     Sub `when` "-" <|> 
     Div `when` "/" <|> 
     Mul `when` "*") 


number = (XNum . read) <$> sign <++> digits <++> decimal 
  where digits  = P.many1 digit
        sign    = P.option "" (P.string "-")
        decimal = P.option "" (char '.' <:> digits)


defines = inParens $ P.string "define" >> ignored >> (function <|> simple)
  where
    simple = Def <$> (identifier <* ignored) <*> expr

    function = do
      (name, args) <- inParens (identifier `sepEndBy` ignored) >>= \case
        name:args -> return (name, args)
        [       ] -> fail "cannot redefine unit"
      body <- functionBody
      return $ Def name (lambda args body)


lambdaParser = inParens $ P.string "lambda" >> do 
  args <- inParens (identifier `sepEndBy` ignored) <* ignored
  body <- functionBody
  return $ lambda args body
  

functionBody = expr `sepEndBy` ignored >>= \case
  [          ] -> return XNil 
  [expression] -> return expression
  expressions  -> return $ XSeq expressions


ignored = comment <|> spaces
  where 
    spaces = void (many $ oneOf " \t\n\f\r")
    comment = do
      spaces 
      many $ do
        P.string ";;" 
        manyTill anyChar newline
        spaces
      spaces


when :: a -> String -> Parser a
when value text = P.string text >> return value


identifier :: Parser String 
identifier = (letter <|> symbol) <:> many (letter <|> digit <|> symbol)
  where symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


inParens :: Parser a -> Parser a 
inParens parser = (char '(' >> spaces) *> parser <* (spaces >> char ')')


(<++>) a b = (++) <$> a <*> b
(<:>) a b  = (:)  <$> a <*> b
(<??>) a b = b <?> a



