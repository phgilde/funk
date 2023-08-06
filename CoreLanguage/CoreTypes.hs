module CoreLanguage.CoreTypes (CoreExpr(CeAbs, CeApp, CeBool, CeInt, CeLet, CeVar))
where

type Name = String
data CoreExpr = CeBool Bool
            | CeInt Int
            | CeAbs Name CoreExpr
            | CeApp CoreExpr CoreExpr
            | CeLet Name CoreExpr CoreExpr
            | CeVar Name
            deriving Show
