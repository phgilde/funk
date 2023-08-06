import Parsing.Parse
import System.IO (stdout, hFlush)

main = do
    putStr "> "
    hFlush stdout
    x <- getLine
    print $ doParse x
    main