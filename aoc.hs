
import System.Environment
import Aoc

main = do
    args <- getArgs
    case args of 
      [file] -> do
        x <- readFile file
        putStr ((aoc (lines x)) ++ "\n")
      _ -> putStrLn "Wrong number of arguments"
