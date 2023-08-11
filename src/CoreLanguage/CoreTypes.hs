module CoreLanguage.CoreTypes (CoreExpr(..), CoreType(..), CoreScheme(..)) where
import Data.List
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
type Name = String
data CoreExpr where
    CeBool :: Bool -> CoreExpr
    CeInt :: Int -> CoreExpr
    CeAbs :: Name -> CoreExpr -> CoreExpr
    CeApp :: CoreExpr -> CoreExpr -> CoreExpr
    CeLet :: Name -> CoreExpr -> CoreExpr -> CoreExpr
    CeVar :: Name -> CoreExpr
    CeCons :: Name -> CoreExpr
    deriving (Show, Eq)

data CoreType where
    TVar :: Name -> CoreType
    TArr :: CoreType -> CoreType -> CoreType
    TCons :: Name -> [CoreType] -> CoreType
    deriving (Eq, Ord)
instance Show CoreType where
    show (TVar n) = n
    show (TArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (TCons n p) = "(" ++ n ++ " " ++ (unwords . fmap show $ p) ++ ")"

data CoreScheme = Forall [Name] CoreType deriving Eq
instance Show CoreScheme where
    show (Forall a t) = "forall " ++ intercalate ", " a ++ " . " ++ show t

pretty (CeBool b) = show b
pretty (CeInt b) = show b
pretty (CeVar b) = b
pretty (CeAbs n b) = "\\" ++ n ++ " -> (" ++ pretty b ++ ")"
pretty (CeApp a b) = "(" ++ pretty a ++ ")(" ++ pretty b ++ ")"
pretty (CeLet n a b) = "let "++ n ++ " = " ++ pretty a ++ " in " ++ pretty b