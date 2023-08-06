module Parsing.ParseEarley where

import Control.Monad
import CoreLanguage.CoreTypes
import Text.Earley.Grammar
import Control.Applicative
import Data.Maybe
import Control.Monad.Trans.Reader
import Data.Map.Strict
import Text.Earley (namedToken)
import Parsing.Lex (Lexeme(..))
import GHC (HsMatchContext(mc_fixity))
import Control.Monad.Trans.Class

data Infixity = RA Int | LA Int | NA Int deriving (Show, Ord, Eq)
type ParseMonad res =forall r. ReaderT (Map Infixity [String]) (Grammar r) (Prod r Lexeme Lexeme res)
getOps fixity = do
    map <- ask
    return $ map ! fixity

pVarName :: ParseMonad String
pVarName = lift . rule $ terminal (\(VarName name) -> Just name)

expr :: Int -> ParseMonad CoreExpr
expr fixity
    | fixity == 0 = letExpr <|> absExpr <|> opExpr fixity <|> expr (fixity + 1)

letExpr :: ParseMonad CoreExpr
letExpr = do
    lift . rule $ namedToken Let 
    name <- pVarName
    lift . rule $ namedToken Equals
    e1 <- expr 0
    lift . rule $ namedToken In
    e2 <- expr 0
    return $ liftA3 CeLet name e1 e2

absExpr :: ParseMonad CoreExpr
absExpr = do
    lift . rule $ namedToken Lambda
    name <- pVarName
    lift . rule $ namedToken Arrow
    e <- expr 0
    return $ liftA2 CeAbs name e

opExpr fixity = do
    operators <- getOps $ RA fixity
    undefined
