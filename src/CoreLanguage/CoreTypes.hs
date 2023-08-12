module CoreLanguage.CoreTypes (CoreExpr(..), CoreType(..), CoreScheme(..), CorePattern (..)) where
import Data.List
type Name = String
data CoreExpr where
    CeBool :: Bool -> CoreExpr
    CeInt :: Int -> CoreExpr
    CeAbs :: Name -> CoreExpr -> CoreExpr
    CeApp :: CoreExpr -> CoreExpr -> CoreExpr
    CeLet :: Name -> CoreExpr -> CoreExpr -> CoreExpr
    CeVar :: Name -> CoreExpr
    CeCons :: Name -> CoreExpr
    CeCases :: CoreExpr -> [(CorePattern, CoreExpr)] -> CoreExpr
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

data CorePattern where
    CPaLitInt :: Int -> CorePattern
    CPaLitBool :: Bool -> CorePattern
    CPaVar :: String -> CorePattern
    CPaCons :: String -> [String] -> CorePattern
    deriving (Show, Eq)