module Parsing.Parse where

import Control.Monad
import CoreLanguage.CoreTypes
import Text.ParserCombinators.Parsec
import Data.Maybe

parseExpr fixity
    | fixity == 0 =
        spaces
            *> try parseLet
            <|> try parseAbs
            <|> parseExpr (fixity + 1)
    | fixity == 9 = spaces *> try parseApp <|> parseExpr (fixity + 1)
    | fixity > 10 =
        spaces
            *> try parseBool
            <|> try parseInt
            <|> try parseVar
            <|> try parens
            <?> "expression"
    | otherwise = parseExpr $ fixity + 1

parens =
    char '(' *> spaces *> parseExpr 0 <* spaces <* char ')'

parseVar = (CeVar <$> varName) <?> "variable"

varName =
    ( do
        l1 <- letter
        rest <- many alphaNum
        guard $ l1 : rest `notElem` ["in", "let"]
        return $ l1 : rest
    )
        <?> "variable name"
spaceString s = do
    spaces
    string s
    spaces
parseBool = (try (CeBool True <$ string "True") <|> CeBool False <$ string "False") <?> "boolean"
parseInt = (CeInt . read <$> many1 digit) <?> "integer"
parseLet =
    ( do
        string "let"
        space
        name <- varName
        spaceString "="
        e1 <- parseExpr 0 <* (space <?> "let") <* spaces <* string "in" <* space
        CeLet name e1 <$> parseExpr 0
    )
        <?> "let binding"

parseApp = do
    first <- parseExpr 10
    rest <- many1 (try $ space *> spaces *> parseExpr 10)
    return $ foldr CeApp first rest

parseAbs =
    ( do
        string "\\"
        spaces
        name <- varName
        spaceString "->"
        CeAbs name <$> parseExpr 0
    )
        <?> "abstraction"
