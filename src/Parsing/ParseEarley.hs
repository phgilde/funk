{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Parsing.ParseEarley (doParse) where

import Control.Applicative as App
import Control.Monad
import CoreLanguage.CoreTypes (CoreScheme (Forall), CoreType (..))
import Data.Map.Strict hiding (foldl)
import Parsing.FrontExpr
import Parsing.Lex (Lexeme (..))
import Text.Earley (namedToken)
import Text.Earley.Grammar
import Text.Earley.Parser

data Infixity = RA Int | LA Int | NA Int deriving (Show, Ord, Eq)

pVarName :: Prod r e Lexeme String
pVarName =
    terminal
        ( \case
            (LexVarName name) -> Just name
            _ -> Nothing
        )
pTypeName :: Prod r e Lexeme String
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

pattern :: Grammar r (Prod r Lexeme Lexeme FePattern)
pattern = mdo
    pat <- rule $ patCons <|> patVar <|> patLitInt <|> patLitBool
    patCons <- rule $ FPaCons <$> pTypeName <*> many pat
    patVar <- rule $ FPaVar <$> pVarName
    patLitInt <- rule $ FPaLitInt <$> terminal (\case (LexLitInt n) -> Just n; _ -> Nothing)
    patLitBool <- rule $ FPaLitBool <$> terminal (\case (LexLitBool n) -> Just n; _ -> Nothing)
    return pat

line :: Map Infixity [String] -> Grammar r (Prod r Lexeme Lexeme Statement)
line opsmap = mdo
    pat <- pattern
    letRule <- rule (FeLet <$> (namedToken LexLet *> pVarName) <*> (namedToken LexEquals *> head exps) <*> (namedToken LexIn *> head exps))
    absRule <- rule (FeAbs <$> (namedToken LexLambda *> pVarName) <*> (namedToken LexArrow *> head exps))
    appRule <- rule (FeApp <$> exps !! 10 <*> exps !! 11)
    parensRule <- rule $ namedToken LexParensL *> head exps <* namedToken LexParensR
    caseRule <- rule $ (,) <$> pat <* namedToken LexArrow <*> head exps <* namedToken LexSemicolon
    casesRule <-
        rule $
            FeCases
                <$> (namedToken LexCase *> head exps)
                <* namedToken LexOf
                <* namedToken LexCurlyL
                <*> many caseRule
                <* namedToken LexCurlyR
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
                                        <|> casesRule
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
    consDef <- rule $ (,) <$> pTypeName <* namedToken LexHasType <*> arrowTypeRule <* namedToken LexSemicolon
    arrowTypeRule <-
        rule $
            TArr <$> applyTypeRule <* namedToken LexArrow <*> arrowTypeRule
            <|> applyTypeRule
    applyTypeRule <-
        rule $
            TCons <$> pTypeName <*> many parensTypeRule
            <|> parensTypeRule
    parensTypeRule <-
        rule $
            TVar <$> pVarName
            <|> namedToken LexParensL *> arrowTypeRule <* namedToken LexParensR
    -- pTypeName muss auch eine expr sein dürfen
    def <- rule $ Def <$> pVarName <*> (namedToken LexEquals *> head exps)
    return statement

doParse :: [Lexeme] -> ([Statement], Report Lexeme [Lexeme])
doParse = fullParses (parser (line operatorMap))
