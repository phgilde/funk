{-# LANGUAGE LambdaCase #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State (runState)
import CoreLanguage.CoreTypes
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.Map (assocs, insert)
import Desugaring.Corify (corify)
import Desugaring.DesugarOps (desugarOps)
import Evalutation.Eval (VarEnv, reduce)
import Inference.Infer
import Parsing.FrontExpr
import Parsing.Lex
import Parsing.ParseEarley
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    _ <- evalRWST runLine () (mempty, preludeEnv)
    return ()

runLine :: RWST () () (VarEnv, TypeEnv) IO ()
runLine = do
    (venv, tenv) <- get
    lift $ putStr "> "
    lift $ hFlush stdout
    x <- lift getLine
    let lexed = doLex "<interactive>" x
    case lexed of
        Left e -> do
            lift $ putStrLn "Lexing Error:"
            lift $ print e
            runLine
        _ -> return ()
    let lexed' = fromRight [] lexed
    lift $ putStrLn "lexed:"
    lift $ print lexed'
    let parsed = doParse lexed'
    case parsed of
        ([], report) -> do
            lift $ putStrLn "Parsing Error: "
            lift $ print report
            runLine
        _ -> return ()
    let parsed' = head . fst $ parsed
    lift $ putStrLn "parsed:"
    lift $ print parsed'
    lift $ putStrLn "desugared:"
    case parsed' of
        TypeDef _ vars constructors -> do
            put (venv, foldr (\(name, ctype) t -> insert name ctype t) tenv (map (second $ Forall vars) constructors))
            runLine
        _ -> return ()

    let desugared = desugarOps $ case parsed' of
            Expr e -> e
            Def n e -> FeLet n e (FeVar n) -- to enable recursive definitions
    lift $ print desugared
    let corified = corify desugared
    lift $ putStrLn "corified:"
    lift $ print corified
    let inferencer = constraintsExpr tenv corified
    (cs, su, t, sch) <- case inferencer of
        Left err -> do
            lift $ putStrLn "inference error:"
            lift $ print err
            runLine
            return undefined
        Right b -> return b
    lift $ print $ assocs su
    lift $ print t
    lift $ print cs
    lift $ print sch
    case parsed' of
        Def name _ -> do
            put (insert name corified venv, insert name sch tenv)
            runLine
        Expr _ -> do
            let (debug, result) = evalExpr corified venv
            lift . putStrLn $ debug
            lift . putStrLn $ result
            runLine

takeUntilEqual :: Eq a => Int -> [a] -> [a]
takeUntilEqual fuel (a : b : r)
    | fuel < 0 = [a]
    | a /= b = a : takeUntilEqual (fuel - 1) (b : r)
    | a == b = [a]
takeUntilEqual _ a = a

evalExpr :: CoreExpr -> VarEnv -> (String, String)
evalExpr expr env =
    let states = iterate (>>= reduce) (return expr)

        states' = takeUntilEqual 100000 . fmap (`runState` env) $ states
     in ( intercalate "\n\n\n\n" $
            zipWith
                ( \(a, b) n ->
                    show n
                        ++ ": VAL\n"
                        ++ show a
                         ++ "\nENV:\n"
                         ++ show b
                )
                states'
                [0 ..]
        , show . fst . last $ states'
        )

terminalType :: CoreType -> CoreType
terminalType (TArr _ b) = terminalType b
terminalType x = x