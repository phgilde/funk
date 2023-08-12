module Desugaring.Corify (corify) where

import Control.Arrow
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
    FeCons n -> CeCons n
    FeCases e c -> CeCases (corify e) . fmap (corifyPattern *** corify) $ c

corifyPattern :: FePattern -> CorePattern
corifyPattern p = case p of
    FPaLitBool b -> CPaLitBool b
    FPaLitInt n -> CPaLitInt n
    FPaVar n -> CPaVar n
    FPaCons n ps -> CPaCons n (fmap (\(FPaVar n) -> n) ps)