module Desugaring.DesugarOps (desugarOps) where
import Parsing.FrontExpr
import CoreLanguage.CoreTypes
import Control.Arrow

desugarOps :: FeExpr -> FeExpr
desugarOps e = case e of
    FeApp a b -> FeApp (desugarOps a) (desugarOps b)
    FeAbs n a -> FeAbs n (desugarOps a)
    FeLet n a b -> FeLet n (desugarOps a) (desugarOps b)
    FeOp n a b -> FeApp (FeApp (FeVar n) $ desugarOps a) $ desugarOps b
    FeCases e c -> FeCases (desugarOps e) (fmap (second desugarOps) c)
    a -> a
