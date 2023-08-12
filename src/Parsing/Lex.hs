module Parsing.Lex (Lexeme (..), doLex) where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Functor.Identity
import Text.Parsec

data Lexeme where
  LexLet :: Lexeme
  LexIn :: Lexeme
  LexLambda :: Lexeme
  LexEquals :: Lexeme
  LexArrow :: Lexeme
  LexParensL :: Lexeme
  LexParensR :: Lexeme
  LexLitBool :: Bool -> Lexeme
  LexLitInt :: Int -> Lexeme
  LexVarName :: String -> Lexeme
  LexOperator :: String -> Lexeme
  LexCurlyL :: Lexeme
  LexCurlyR :: Lexeme
  LexSemicolon :: Lexeme
  LexType :: Lexeme
  LexWhere :: Lexeme
  LexHasType :: Lexeme
  LexTypeName :: String -> Lexeme
  LexForall :: Lexeme
  LexPeriod :: Lexeme
  deriving (Show, Eq)

lexer :: ParsecT String st Identity [Lexeme]
lexer =
  ( try pLet
      <|> try pIn
      <|> try pLambda
      <|> try pArrow
      <|> try pLeftParens
      <|> try pRightParens
      <|> try pEquals
      <|> try pLitBool
      <|> try pLitInt
      <|> try pType
      <|> try pWhere
      <|> try pForall
      <|> try pPeriod
      <|> try pHasType
      <|> try pTypeName
      <|> try pVarName
      <|> try pOperator
      <|> try pCurlyL
      <|> try pCurlyR
      <|> try pSemicolon
  )
    `sepBy` (spaces <|> void newline)
pCurlyL :: ParsecT String u Identity Lexeme
pCurlyL = LexCurlyL <$ char '{'
pCurlyR :: ParsecT String u Identity Lexeme
pCurlyR = LexCurlyR <$ char '}'
pSemicolon :: ParsecT String u Identity Lexeme
pSemicolon = LexSemicolon <$ char ';'

pEquals :: ParsecT String u Identity Lexeme
pEquals = LexEquals <$ char '=' <* notFollowedBy opChar

pLet :: ParsecT String u Identity Lexeme
pLet = LexLet <$ string "let" <* notFollowedBy varChar
pIn :: ParsecT String u Identity Lexeme
pIn = LexIn <$ string "in" <* notFollowedBy varChar
pLambda :: ParsecT String u Identity Lexeme
pArrow :: ParsecT String u Identity Lexeme
pLeftParens :: ParsecT String u Identity Lexeme
pRightParens :: ParsecT String u Identity Lexeme
pLambda = LexLambda <$ char '\\'
pArrow = LexArrow <$ string "->" <* notFollowedBy opChar
pLeftParens = LexParensL <$ char '('
pRightParens = LexParensR <$ char ')'
pLitBool :: ParsecT String u Identity Lexeme
pLitBool = LexLitBool True <$ string "True" <* notFollowedBy varChar <|> LexLitBool False <$ string "False" <* notFollowedBy varChar

pForall :: ParsecT String u Identity Lexeme
pForall = LexForall <$ string "forall" <* notFollowedBy varChar

pPeriod :: ParsecT String u Identity Lexeme
pPeriod = LexPeriod <$ string "." <* notFollowedBy opChar

pLitInt :: ParsecT String u Identity Lexeme
pLitInt = LexLitInt . read <$> many1 digit <* notFollowedBy varChar


pVarName :: ParsecT String u Identity Lexeme
pVarName = LexVarName <$> liftA2 (:) varStart (many varChar)

pOperator :: ParsecT String u Identity Lexeme
pOperator = LexOperator <$> many1 opChar

opChar :: ParsecT String u Identity Char
opChar = oneOf "<>!?$&/=%+-*.:"
varChar :: ParsecT String u Identity Char
varChar = alphaNum <|> oneOf "\'_"
varStart :: ParsecT String u Identity Char
varStart = lower

typeStart :: ParsecT String u Identity Char
typeStart = upper

pType :: ParsecT String u Identity Lexeme
pType = LexType <$ string "type" <* notFollowedBy varChar

pTypeName :: ParsecT String u Identity Lexeme
pTypeName = LexTypeName <$> liftA2 (:) typeStart (many varChar)

pWhere :: ParsecT String u Identity Lexeme
pWhere = LexWhere <$ string "where" <* notFollowedBy varChar

pHasType :: ParsecT String u Identity Lexeme
pHasType = LexHasType <$ string "::" <* notFollowedBy opChar

doLex :: SourceName -> String -> Either ParseError [Lexeme]
doLex = parse lexer