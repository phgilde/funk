module CoreLanguage.CoreTypes (CoreExpr(CeAbs, CeApp, CeBool, CeInt, CeLet, CeVar, CeOp))
where

type Name = String
data CoreExpr = CeBool Bool
            | CeInt Int
            | CeAbs Name CoreExpr
            | CeApp CoreExpr CoreExpr
            | CeLet Name CoreExpr CoreExpr
            | CeVar Name
            | CeOp Name CoreExpr CoreExpr
            deriving Show
