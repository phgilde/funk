module Parsing.Lex (Lexeme (Let, In, Lambda, Equals, Arrow, ParensL, ParensR, LitBool, LitInt, VarName, Operator), doLex) where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import Text.ParserCombinators.Parsec

data Lexeme where
  Let :: Lexeme
  In :: Lexeme
  Lambda :: Lexeme
  Equals :: Lexeme
  Arrow :: Lexeme
  ParensL :: Lexeme
  ParensR :: Lexeme
  LitBool :: Bool -> Lexeme
  LitInt :: Int -> Lexeme
  VarName :: String -> Lexeme
  Operator :: String -> Lexeme
  CurlyL :: Lexeme
  CurlyR :: Lexeme
  Semicolon :: Lexeme
  deriving (Show, Eq)

lexer :: ParsecT String st Identity [Lexeme]
lexer =
  ( pLet
      <|> pIn
      <|> pLambda
      <|> pArrow
      <|> pLeftParens
      <|> pRightParens
      <|> pEquals
      <|> pLitBool
      <|> pLitInt
      <|> pVarName
      <|> pOperator
      <|> pCurlyL
      <|> pCurlyR
      <|> pSemicolon
  )
    `sepBy` (spaces <|> void newline)
pCurlyL = CurlyL <$ char '{'
pCurlyR = CurlyR <$ char '}'
pSemicolon = Semicolon <$ char ';'

pEquals = Equals <$ char '=' <* notFollowedBy opChar

pLet = Let <$ string "let" <* notFollowedBy varChar
pIn = In <$ string "in" <* notFollowedBy varChar
pLambda = Lambda <$ char '\\'
pArrow = Arrow <$ string "->"
pLeftParens = ParensL <$ char '('
pRightParens = ParensR <$ char ')'
pLitBool = LitBool True <$ string "True" <* notFollowedBy varChar <|> LitBool False <$ string "False" <* notFollowedBy varChar

pLitInt = LitInt . read <$> many1 digit <* notFollowedBy varChar

pVarName = VarName <$> liftA2 (:) varStart (many varChar)
pOperator = Operator <$> many1 opChar

opChar = oneOf "<>!?$&/=%+-*.:"
varChar = alphaNum <|> oneOf "\'_"
varStart = letter

doLex = parse lexer