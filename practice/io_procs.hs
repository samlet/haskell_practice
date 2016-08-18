import System.IO

main = do
    putStrLn "Hello, what's your name?"
    -- name <- getLine
    let name="tom"
    let fileName="../README.md"
    putStrLn ("Hey " ++ name ++ ", you rock!")

    contents <- readFile fileName
    putStr contents

    -- 
    withFile fileName ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)


