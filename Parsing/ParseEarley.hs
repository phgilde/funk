{-# LANGUAGE RecursiveDo #-}

module Parsing.ParseEarley where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import CoreLanguage.CoreTypes
import Data.Map.Strict
import Data.Maybe
import GHC (HsMatchContext (mc_fixity))
import Parsing.Lex (Lexeme (..))
import Text.Earley (namedToken)
import Text.Earley.Grammar

data Infixity = RA Int | LA Int | NA Int deriving (Show, Ord, Eq)
type ParseMonad res = forall r. ReaderT (Map Infixity [String]) (Grammar r) (Prod r Lexeme Lexeme res)
getOps fixity = do
    map <- ask
    return $ map ! fixity

pVarName = terminal (\(VarName name) -> Just name)
pOp op = terminal (\(Operator op2) -> if op2 == op then Just op else Nothing)

operatorMap = insert (LA 1) ["+", "-"] . insert (LA 2) ["*", "/"] $ Data.Map.Strict.empty

expr opsmap = mdo
    letRule <- rule (CeLet <$> (namedToken Let *> pVarName) <*> (namedToken Equals *> head exps) <*> (namedToken In *> head exps))
    absRule <- rule (CeAbs <$> (namedToken Lambda *> pVarName) <*> (namedToken Arrow *> head exps))
    appRule <- rule (CeApp <$> exps !! 11 <*> exps !! 11)
    exps <-
        forM
            [0 .. 12]
            ( \fixity ->
                rule
                    ( if fixity == 12
                        then Control.Applicative.empty
                        else
                            ( case fixity of
                                0 ->
                                    letRule
                                        <|> absRule
                                10 -> appRule
                                11 -> CeVar <$> pVarName <|> CeInt <$> terminal (\(LitInt n) -> Just n) <|> CeBool <$> terminal (\(LitBool n) -> Just n)
                                _ -> Control.Applicative.empty
                            )
                                <|> let opsl = findWithDefault [] (LA fixity) opsmap
                                        opsr = findWithDefault [] (RA fixity) opsmap
                                        opsn = findWithDefault [] (NA fixity) opsmap
                                     in foldl1 (<|>) (fmap (\op -> liftA3 (flip CeOp) (exps !! (fixity + 1)) (pOp op) (exps !! (fixity + 1))) opsn)
                                            <|> foldl1 (<|>) (fmap (\op -> liftA3 (flip CeOp) (exps !! fixity) (pOp op) (exps !! (fixity + 1))) opsl)
                                            <|> foldl1 (<|>) (fmap (\op -> liftA3 (flip CeOp) (exps !! (fixity + 1)) (pOp op) (exps !! fixity)) opsr)
                                            <|> exps !! (fixity + 1)
                    )
            )

    return $ head exps