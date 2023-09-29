{-# LANGUAGE GADTSyntax #-}

module Parsing.FrontExpr (FeExpr (..), Statement (..), FePattern (..)) where

import CoreLanguage.CoreTypes

data FeExpr where
    FeVar :: String -> FeExpr
    FeLitInt :: Int -> FeExpr
    FeLitBool :: Bool -> FeExpr
    FeApp :: FeExpr -> FeExpr -> FeExpr
    FeAbs :: String -> FeExpr -> FeExpr
    FeLet :: String -> FeExpr -> FeExpr -> FeExpr
    FeOp :: String -> FeExpr -> FeExpr -> FeExpr
    FeCons :: String -> FeExpr
    FeCases :: FeExpr -> [(FePattern, FeExpr)] -> FeExpr
    deriving (Show)

data Statement = Def String FeExpr | Expr FeExpr | TypeDef String [String] [DataCons] deriving (Show)
type DataCons = (String, CoreType)
data FePattern where
    FPaCons :: String -> [FePattern] -> FePattern
    FPaVar :: String -> FePattern
    FPaLitInt :: Int -> FePattern
    FPaLitBool :: Bool -> FePattern
    deriving Show