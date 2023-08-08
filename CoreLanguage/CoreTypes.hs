module CoreLanguage.CoreTypes (CoreExpr(..), CoreType(..), CoreScheme(..))
where

type Name = String
data CoreExpr where
    CeBool :: Bool -> CoreExpr
    CeInt :: Int -> CoreExpr
    CeAbs :: Name -> CoreExpr -> CoreExpr
    CeApp :: CoreExpr -> CoreExpr -> CoreExpr
    CeLet :: Name -> CoreExpr -> CoreExpr -> CoreExpr
    CeVar :: Name -> CoreExpr
    deriving Show

data CoreType where
    TVar :: Name -> CoreType
    TConstant :: Name -> CoreType
    TArr :: CoreType -> CoreType -> CoreType
    TCons :: Name -> [CoreType] -> CoreType

data CoreScheme = Forall [Name] CoreType

pretty (CeBool b) = show b
pretty (CeInt b) = show b
pretty (CeVar b) = b
pretty (CeAbs n b) = "\\" ++ n ++ " -> (" ++ pretty b ++ ")"
pretty (CeApp a b) = "(" ++ pretty a ++ ")(" ++ pretty b ++ ")"
pretty (CeLet n a b) = "let "++ n ++ " = " ++ pretty a ++ " in " ++ pretty b