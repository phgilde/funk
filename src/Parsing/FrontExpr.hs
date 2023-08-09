module Parsing.FrontExpr (FeExpr(..)) where
{-# LANGUAGE GADTSyntax #-}

{-# LANGUAGE GADTs #-}
data FeExpr where
    FeVar :: String -> FeExpr
    FeLitInt :: Int -> FeExpr
    FeLitBool :: Bool -> FeExpr
    FeApp :: FeExpr -> FeExpr -> FeExpr
    FeAbs :: String -> FeExpr -> FeExpr
    FeLet :: String -> FeExpr -> FeExpr -> FeExpr
    FeOp :: String -> FeExpr -> FeExpr -> FeExpr
    deriving Show