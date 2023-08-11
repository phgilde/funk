module Evalutation.Eval (reduce, reduceSingle, VarEnv) where

import Control.Arrow (Arrow (second))
import Control.Monad.Trans.State
import CoreLanguage.CoreTypes (CoreExpr (..))
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
    CeVar name -> do
        e <- getName name {-
                          traceM name
                          traceM $ show e-}
        return e
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
            a -> return a

reduceSingle :: CoreExpr -> CoreExpr
reduceSingle ex = evalState (reduce ex) mempty