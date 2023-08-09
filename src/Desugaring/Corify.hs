module Desugaring.Corify where
import CoreLanguage.CoreTypes
import Parsing.FrontExpr

corify :: FeExpr -> CoreExpr
corify e = case e of
    FeAbs n e -> CeAbs n $ corify e
    FeApp a b -> CeApp (corify a) (corify b)
    FeLet n a b -> CeLet n (corify a) (corify b)
    FeLitBool b -> CeBool b
    FeLitInt b -> CeInt b
    FeVar n -> CeVar n
