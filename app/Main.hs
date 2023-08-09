import Parsing.ParseEarley
import System.IO (stdout, hFlush)
import Parsing.Lex
import Data.Either (fromRight)
import Desugaring.DesugarOps (desugarOps)
import Desugaring.Corify (corify)
import Inference.Infer
import Data.Foldable
import Data.Map (assocs)

main = do
    putStr "> "
    hFlush stdout
    --x <- getLine
    let x = "let fix = \\f -> f (fix f) in fix"
    let lexed = doLex "<interactive>" x
    case lexed of
        Left e -> do
            putStrLn "Lexing Error:"
            print e
            main
        _ -> return ()
    let lexed' = fromRight [] lexed
    putStrLn "lexed:"
    print lexed'
    let parsed = doParse lexed'
    case parsed of
        ([], report) -> do
            putStrLn "Parsing Error: "
            print report
            main
        _ -> return ()
    let parsed' = head . fst $ parsed
    putStrLn "parsed:"
    print parsed'
    putStrLn "desugared:"
    let desugared = desugarOps parsed'
    print desugared
    let corified = corify desugared
    putStrLn "corified:"
    print corified
    let inferencer = constraintsExpr preludeEnv corified
    case inferencer of
        Left e -> do
            putStrLn "inference error:"
            print e
            main
        _ -> return ()
    let Right (cs, su, t, sch) = inferencer
    print $ assocs su
    print t
    print cs
    print sch
    main