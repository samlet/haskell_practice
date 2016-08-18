import System.Environment
import Data.List

-- 在 System.Environment 模块当中有两个很酷的 I/O actions，一个是 getArgs，他的 type 是 getArgs :: IO [String]，他是一个拿取命令行引数的 I/O action，并把结果放在包含的一个串列中。getProgName 的型态是 getProgName :: IO String，他则是一个 I/O action 包含了程序的名称。

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName

-- $ ./arg-test first second w00t "multi word arg"
