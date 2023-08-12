module Evalutation.Eval (reduce, VarEnv, reduceNormal) where

import Control.Arrow (Arrow (second))
import Control.Monad (forM_)
import Control.Monad.Trans.State
import CoreLanguage.CoreTypes (CoreExpr (..), CorePattern (..))
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

reduce :: CoreExpr -> State VarEnv CoreExpr
reduce expr = case expr of
    CeApp (CeAbs name e1) e2 -> do
        bind name e2
        return e1
    CeLet name e1 e2 -> do
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
                            forM_ (zip names args) $ \(name, arg) -> name `bind` arg
                            return res
                        (CPaVar name, res) -> do
                            bind name e
                            return res
                (CeBool b, _) -> do
                    let match = head [(pat, res) | (pat, res) <- cases, case pat of CPaLitBool b' -> b == b'; CPaVar _ -> True; _ -> False]
                    case match of
                        (CPaLitBool _, res) -> return res
                        (CPaVar name, res) -> do
                            bind name e
                            return res
                (CeInt n, _) -> do
                    let match = head [(pat, res) | (pat, res) <- cases, case pat of CPaLitInt n' -> n == n'; CPaVar _ -> True; _ -> False]
                    case match of
                        (CPaLitInt _, res) -> return res
                        (CPaVar name, res) -> do
                            bind name e
                            return res
            a -> return a

reduceNormal :: CoreExpr -> State VarEnv CoreExpr
reduceNormal expr = do
    reduced <- reduce expr
    if reduced == expr then return expr else reduceNormal expr