{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Parsing.ParseEarley (doParse, Statement (..)) where

import Control.Applicative as App
import Control.Monad
import CoreLanguage.CoreTypes (CoreType (..), CoreScheme (Forall))
import Data.Map.Strict hiding (foldl)
import Parsing.FrontExpr
import Parsing.Lex (Lexeme (..))
import Text.Earley (namedToken)
import Text.Earley.Grammar
import Text.Earley.Parser

data Infixity = RA Int | LA Int | NA Int deriving (Show, Ord, Eq)
data Statement = Def String FeExpr | Expr FeExpr | TypeDef String [String] [DataCons] deriving (Show)
type DataCons = (String, CoreScheme)

pVarName :: Prod r e Lexeme String
pVarName =
    terminal
        ( \case
            (LexVarName name) -> Just name
            _ -> Nothing
        )
pTypeName =
    terminal
        ( \case
            (LexTypeName name) -> Just name
            _ -> Nothing
        )
pOp :: String -> Prod r e Lexeme String
pOp op =
    terminal
        ( \case
            (LexOperator op2) | op2 == op -> Just op
            _ -> Nothing
        )

operatorMap :: Map Infixity [String]
operatorMap = insert (LA 1) ["+", "-"] . insert (LA 2) ["*", "/"] $ Data.Map.Strict.empty

line :: Map Infixity [String] -> Grammar r (Prod r Lexeme Lexeme Statement)
line opsmap = mdo
    letRule <- rule (FeLet <$> (namedToken LexLet *> pVarName) <*> (namedToken LexEquals *> head exps) <*> (namedToken LexIn *> head exps))
    absRule <- rule (FeAbs <$> (namedToken LexLambda *> pVarName) <*> (namedToken LexArrow *> head exps))
    appRule <- rule (FeApp <$> exps !! 10 <*> exps !! 11)
    parensRule <- rule $ namedToken LexParensL *> head exps <* namedToken LexParensR
    exps <-
        forM
            [0 .. 12]
            ( \fixity ->
                rule
                    ( if fixity == 12
                        then App.empty
                        else
                            ( case fixity of
                                0 ->
                                    letRule
                                        <|> absRule
                                10 -> appRule
                                11 ->
                                    FeVar <$> pVarName
                                        <|> FeLitInt <$> terminal (\case (LexLitInt n) -> Just n; _ -> Nothing)
                                        <|> FeLitBool <$> terminal (\case (LexLitBool n) -> Just n; _ -> Nothing)
                                        <|> parensRule
                                        <|> FeCons <$> pTypeName
                                _ -> App.empty
                            )
                                <|> let opsl = findWithDefault [] (LA fixity) opsmap
                                        opsr = findWithDefault [] (RA fixity) opsmap
                                        opsn = findWithDefault [] (NA fixity) opsmap
                                     in foldl
                                            (<|>)
                                            App.empty
                                            ( fmap (\op -> liftA3 (flip FeOp) (exps !! (fixity + 1)) (pOp op) (exps !! (fixity + 1))) opsn
                                            )
                                            <|> foldl
                                                (<|>)
                                                App.empty
                                                ( fmap (\op -> liftA3 (flip FeOp) (exps !! fixity) (pOp op) (exps !! (fixity + 1))) opsl
                                                )
                                            <|> foldl
                                                (<|>)
                                                App.empty
                                                ( fmap (\op -> liftA3 (flip FeOp) (exps !! (fixity + 1)) (pOp op) (exps !! fixity)) opsr
                                                )
                                            <|> exps !! (fixity + 1)
                    )
            )
    statement <- rule $ expr <|> def <|> typeDef
    expr <- rule $ Expr <$> head exps
    typeDef <- rule $ TypeDef <$> (namedToken LexType *> pTypeName) <*> many pVarName <* namedToken LexWhere <* namedToken LexCurlyL <*> many consDef <* namedToken LexCurlyR
    consDef <- rule $ (,) <$> pTypeName <* namedToken LexHasType <*> scheme <* namedToken LexSemicolon
    typeRule <- rule $ TArr <$> typeRule <* namedToken LexArrow <*> typeRule
                        <|> TCons <$> pTypeName <*> many typeRule
                        <|> TVar <$> pVarName
                        <|> namedToken LexParensL *> typeRule <* namedToken LexParensR
    --pTypeName muss auch eine expr sein dürfen
    scheme <- rule $ Forall <$> (namedToken LexForall  *> many pVarName) <* namedToken LexPeriod <*> typeRule
    def <- rule $ Def <$> pVarName <*> (namedToken LexEquals *> head exps)
    return statement

doParse :: [Lexeme] -> ([Statement], Report Lexeme [Lexeme])
doParse = fullParses (parser (line operatorMap))
