module CoreLanguage.CoreTypes (CoreExpr (..), CoreType (..), CoreScheme (..), CorePattern (..), getVars) where

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
    deriving (Eq)

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

instance Show CoreExpr where
    show = prettyPrint 0
prettyPrint :: Int -> CoreExpr -> [Char]
prettyPrint depth (CeBool b) = duplicate "| " depth ++ show b
prettyPrint depth (CeInt b) = duplicate "| " depth ++ show b
prettyPrint depth (CeAbs n e) =
    duplicate "| " depth
        ++ "\\"
        ++ n
        ++ "->\n"
        ++ prettyPrint (depth + 1) e
prettyPrint depth (CeApp e1 e2) =
    prettyPrint depth e1
        ++ "\n"
        ++ prettyPrint (depth + 1) e2
prettyPrint depth (CeLet n e1 e2) =
    duplicate "| " depth
        ++ "let "
        ++ n
        ++ " = \n"
        ++ prettyPrint (depth + 1) e1
        ++ "\n"
        ++ duplicate "| " depth
        ++ "in\n"
        ++ prettyPrint (depth + 1) e2
prettyPrint depth (CeVar n) = duplicate "| " depth ++ n
prettyPrint depth (CeCons n) = duplicate "| " depth ++ n
prettyPrint depth (CeCases e cases) =
    duplicate "| " depth
        ++ "case\n"
        ++ prettyPrint (depth + 1) e
        ++ "\n"
        ++ duplicate "| " depth
        ++ "of {\n"
        ++ concat
            [ duplicate "| " (depth + 1)
                ++ show pattern
                ++ " ->\n"
                ++ prettyPrint (depth + 2) e'
                ++ ";\n"
            | (pattern, e') <- cases
            ]
        ++ duplicate "| " depth
        ++ "}"
data CoreType where
    TVar :: Name -> CoreType
    TArr :: CoreType -> CoreType -> CoreType
    TCons :: Name -> [CoreType] -> CoreType
    deriving (Eq, Ord)
instance Show CoreType where
    show (TVar n) = n
    show (TArr a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
    show (TCons n p) = "(" ++ n ++ " " ++ (unwords . fmap show $ p) ++ ")"

data CoreScheme = Forall [Name] CoreType deriving (Eq)
instance Show CoreScheme where
    show (Forall a t) = "forall " ++ intercalate ", " a ++ " . " ++ show t

data CorePattern where
    CPaLitInt :: Int -> CorePattern
    CPaLitBool :: Bool -> CorePattern
    CPaVar :: String -> CorePattern
    CPaCons :: String -> [String] -> CorePattern
    deriving (Show, Eq)

getVars :: CorePattern -> [String]
getVars (CPaVar name) = [name]
getVars (CPaCons _ subs) = subs
getVars _ = []
