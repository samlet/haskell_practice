-- strings_encoding
module Main where
import Data.Char

main = putStrLn "Hello World"

-- Caesar ciphar 是加密的基础算法，它将消息中的每个字符都按照特定的字母表进行替换。它的实现非常简单，我们这里就先不管字母表了.
encode :: Int -> String -> String  
encode shift msg = 
  let ords = map ord msg  
      shifted = map (+ shift) ords  
  in map chr shifted
