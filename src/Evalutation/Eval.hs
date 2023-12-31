module Evalutation.Eval (reduce, VarEnv, reduceNormal) where

import Control.Arrow (Arrow (second))
import Control.Monad (forM_)
import Control.Monad.Trans.State
import CoreLanguage.CoreTypes (CoreExpr (..), CorePattern (..), getVars)
import Data.Map (insert)
import Data.Map as Map
import Data.Maybe
import Debug.Trace

type VarEnv = Map.Map String CoreExpr

builtIn :: String -> Maybe ([CoreExpr] -> CoreExpr)
builtIn "+" = Just $ \[CeInt a, CeInt b] -> CeInt (a + b)
builtIn "-" = Just $ \[CeInt a, CeInt b] -> CeInt (a - b)
builtIn "*" = Just $ \[CeInt a, CeInt b] -> CeInt (a * b)
builtIn "/" = Just $ \[CeInt a, CeInt b] -> CeInt (a `div` b)
builtIn _ = Nothing

isReduced :: CoreExpr -> Bool
isReduced (CeInt _) = True
isReduced (CeBool _) = True
isReduced (CeApp a b) = isReduced a && isReduced b
isReduced (CeCons _) = True
isReduced _ = False

isWeakReduced :: CoreExpr -> Bool
isWeakReduced (CeInt _) = True
isWeakReduced (CeBool _) = True
isWeakReduced (CeApp a _) = isWeakReduced a
isWeakReduced (CeCons _) = True
isWeakReduced _ = False

bind :: String -> CoreExpr -> State VarEnv ()
bind name val = modify (insert name val)

getName :: String -> State VarEnv CoreExpr
getName name = get >>= (return . (! name))

spine :: CoreExpr -> (CoreExpr, [CoreExpr])
spine = second reverse . spine'
  where
    spine' (CeApp e1 e2) = second (e2 :) $ spine' e1
    spine' x = (x, [])

fromSpine :: CoreExpr -> [CoreExpr] -> CoreExpr
fromSpine e es =
    let se = reverse es
     in case se of
            [] -> e
            s : ses -> CeApp (fromSpine e $ reverse ses) s

replace :: String -> CoreExpr -> CoreExpr -> CoreExpr
replace name val expr = case expr of
    CeVar n | n == name -> val
    CeVar n -> CeVar n
    CeAbs n e -> CeAbs n (replace name val e)
    CeApp e1 e2 -> CeApp (replace name val e1) (replace name val e2)
    CeCases e1 cases -> CeCases (replace name val e1) (fmap (second $ replace name val) cases)
    CeLet n e1 e2 -> CeLet n (replace name val e1) (replace name val e2)
    CeInt a -> CeInt a
    CeBool a -> CeBool a
    CeCons a -> CeCons a

replaceMany :: Map String CoreExpr -> CoreExpr -> CoreExpr
replaceMany map expr = case expr of
    CeVar n | n `member` map -> map ! n
    CeVar n -> CeVar n
    CeAbs n e -> CeAbs n (replaceMany (n `delete` map) e)
    CeApp e1 e2 -> CeApp (replaceMany map e1) (replaceMany map e2)
    CeCases e1 cases -> CeCases (replaceMany map e1) (fmap (\(pat,val)-> (pat, replaceMany (Prelude.foldl (flip delete) map $ getVars pat) val)) cases)
    CeLet n e1 e2 -> CeLet n (replaceMany (n `delete` map) e1) (replaceMany (n `delete` map) e2)
    CeInt a -> CeInt a
    CeBool a -> CeBool a
    CeCons a -> CeCons a

reduce :: CoreExpr -> State VarEnv CoreExpr
reduce expr = case expr of
    CeApp (CeAbs name e1) e2 -> do
        --traceM $ "Replacing " ++ name ++ " with \n" ++ show e1 ++ " \n in \n" ++ show e2
        return $ replace name e2 e1
    CeLet name e1 e2 -> do
        --traceM $ "Binding \n" ++ show e1 ++ "\nto name " ++ name
        bind name e1
        return e2
    CeVar name -> getName name
    a -> case spine a of
        (CeVar name, args) | isJust . builtIn $ name -> case span isReduced args of
            (_, []) -> return $ (fromJust . builtIn $ name) args
            (reduced, r : unreduced) -> do
                r' <- reduce r
                return $ fromSpine (CeVar name) (reduced ++ r' : unreduced)
        _ -> case a of
            CeApp a b | not $ isReduced a -> do
                red <- reduce a
                return $ CeApp red b
            CeApp a b | not $ isReduced b -> do
                red <- reduce b
                return $ CeApp a red
            CeCases e cases | not $ isWeakReduced e -> do
                red <- reduce e
                return $ CeCases red cases
            CeCases e cases | isWeakReduced e -> case spine e of
                (CeCons consName, args) -> do
                    let match = head [(pat, res) | (pat, res) <- cases, case pat of CPaCons pname _ -> pname == consName; CPaVar _ -> True; _ -> False]
                    case match of
                        (CPaCons _ names, res) -> do
                            let map = fromList $ zip names args
                            return $ replaceMany map res
                        (CPaVar name, res) -> do
                            return $ replace name e res
                (CeBool b, _) -> do
                    let match = head [(pat, res) | (pat, res) <- cases, case pat of CPaLitBool b' -> b == b'; CPaVar _ -> True; _ -> False]
                    case match of
                        (CPaLitBool _, res) -> return res
                        (CPaVar name, res) -> do
                            return $ replace name e res
                (CeInt n, _) -> do
                    let match = head [(pat, res) | (pat, res) <- cases, case pat of CPaLitInt n' -> n == n'; CPaVar _ -> True; _ -> False]
                    case match of
                        (CPaLitInt _, res) -> return res
                        (CPaVar name, res) -> do
                            return $ replace name e res
            a -> return a

reduceNormal :: CoreExpr -> State VarEnv CoreExpr
reduceNormal expr = do
    reduced <- reduce expr
    if reduced == expr then return expr else reduceNormal expr