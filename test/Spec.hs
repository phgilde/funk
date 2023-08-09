import CoreLanguage.CoreTypes
import Inference.Infer
import Test.QuickCheck

prop_fixType :: Bool
prop_fixType =
    inferExpr
        preludeEnv
        ( CeLet "fix" (CeAbs "f" (CeApp (CeVar "f") (CeApp (CeVar "fix") (CeVar "f")))) (CeVar "fix")
        )
        == Right (Forall ["a"] (TArr (TArr (TVar "a") (TVar "a")) (TVar "a")))

main = do
    quickCheck prop_fixType