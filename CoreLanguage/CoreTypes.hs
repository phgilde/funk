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

pretty (CeBool b) = show b
pretty (CeInt b) = show b
pretty (CeVar b) = b
pretty (CeAbs n b) = "\\" ++ n ++ " -> (" ++ pretty b ++ ")"
pretty (CeApp a b) = "(" ++ pretty a ++ ")(" ++ pretty b ++ ")"
pretty (CeLet n a b) = "let "++ n ++ " = " ++ pretty a ++ " in " ++ pretty b
pretty (CeOp n a b) = "(" ++ pretty a ++ " " ++ n ++ " " ++ pretty b ++ ")"