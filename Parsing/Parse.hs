module Parsing.Parse (doParse) where

import Control.Monad
import CoreLanguage.CoreTypes

import Control.Applicative (liftA3)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Map.Strict (Map, empty, insert, (!), findWithDefault)
import Data.Maybe
import Text.Parsec

data Infixity = LA Int | RA Int | NA Int deriving (Show, Eq, Ord)

parseExpr :: Int -> ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
parseExpr fixity
    | fixity < 12 =
        spaces
            *> (( case fixity of
                    _
                        | fixity == 0 ->
                            try parseLet
                                <|> try parseAbs
                    _ | fixity == 10 -> try parseApp
                    _
                        | fixity > 10 ->
                            try parseBool
                                <|> try parseInt
                                <|> try parseVar
                                <|> try parens
                    _ -> fail "expression"
               )
            <|> try (parseOps fixity)
            <|> parseExpr (fixity + 1))
            <?> "expression a"
    | otherwise = fail "expression < 12"

parens :: ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
parens =
    char '(' *> spaces *> parseExpr 0 <* spaces <* char ')'

parseVar :: ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
parseVar = (CeVar <$> varName) <?> "variable"

varName :: ParsecT [Char] u (Reader (Map Infixity [String])) [Char]
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

parseBool :: ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
parseBool =
    try (CeBool True <$ string "True")
        <|> try (CeBool False <$ string "False")
        <?> "boolean"
parseInt = (CeInt . read <$> many1 digit) <?> "integer"
parseLet :: ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
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

parseApp :: ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
parseApp = do
    first <- parseExpr 11
    rest <- many1 (try $ space *> spaces *> parseExpr 11)
    return $ foldr CeApp first rest

parseAbs :: ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
parseAbs =
    ( do
        string "\\"
        spaces
        name <- varName
        spaceString "->"
        CeAbs name <$> parseExpr 0
    )
        <?> "abstraction"

parseOps :: Int -> ParsecT [Char] u (Reader (Map Infixity [String])) CoreExpr
parseOps fixity =
    if fixity >= 12
        then fail "too fix"
        else do
            map <- lift ask
            let lops = findWithDefault [] (LA fixity) map
            let rops = findWithDefault [] (RA fixity) map
            let nops = findWithDefault [] (NA fixity) map
            try (chainl1 (parseExpr $ fixity + 1) (CeOp <$> parseOp lops))
                <|> try (chainr1 (parseExpr $ fixity + 1) (CeOp <$> parseOp rops))
                <|> try (liftA3 (flip CeOp) (parseExpr $ fixity + 1) (parseOp nops) (parseExpr $ fixity + 1))

parseOp ops = do
    spaces
    op <- many1 $ oneOf ".!<>+*|$%&/^?="
    spaces
    guard $ op `elem` ops
    return op

operatorMap = insert (LA 1) ["+", "-"] . insert (LA 2) ["*", "/"] $ empty
doParse string = runReader (runParserT (parseExpr 0 <* eof) () "<interactive>" string) operatorMap
doParseF parser string = runReader (runParserT parser () "<interactive>" string) operatorMap