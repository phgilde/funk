{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Parsing.ParseEarley (doParse, Statement (..)) where

import Control.Applicative as App
import Control.Monad
import Parsing.FrontExpr
import Data.Map.Strict hiding (foldl)
import Parsing.Lex (Lexeme (..))
import Text.Earley (namedToken)
import Text.Earley.Grammar
import Text.Earley.Parser

data Infixity = RA Int | LA Int | NA Int deriving (Show, Ord, Eq)
data Statement = Def String FeExpr | Expr FeExpr deriving (Show)


pVarName :: Prod r e Lexeme String
pVarName =
    terminal
        ( \case
            (VarName name) -> Just name
            _ -> Nothing
        )

pOp :: String -> Prod r e Lexeme String
pOp op =
    terminal
        ( \case
            (Operator op2) | op2 == op -> Just op
            _ -> Nothing
        )

operatorMap :: Map Infixity [String]
operatorMap = insert (LA 1) ["+", "-"] . insert (LA 2) ["*", "/"] $ Data.Map.Strict.empty

line :: Map Infixity [String] -> Grammar r (Prod r Lexeme Lexeme Statement)
line opsmap = mdo
    letRule <- rule (FeLet <$> (namedToken Let *> pVarName) <*> (namedToken Equals *> head exps) <*> (namedToken In *> head exps))
    absRule <- rule (FeAbs <$> (namedToken Lambda *> pVarName) <*> (namedToken Arrow *> head exps))
    appRule <- rule (FeApp <$> exps !! 10 <*> exps !! 11)
    parensRule <- rule $ namedToken ParensL *> head exps <* namedToken ParensR
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
                                        <|> FeLitInt <$> terminal (\case (LitInt n) -> Just n; _ -> Nothing)
                                        <|> FeLitBool <$> terminal (\case (LitBool n) -> Just n; _ -> Nothing)
                                        <|> parensRule
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
    statement <- rule $ Expr <$> head exps <|> def
    def <- rule $ Def <$> pVarName <*> (namedToken Equals *> head exps)
    return statement


doParse :: [Lexeme] -> ([Statement], Report Lexeme [Lexeme])
doParse = fullParses (parser (line operatorMap))