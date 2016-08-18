-- modules
module Main where
import Data.List  

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub

main :: IO ()
main = do
    putStrLn "Hello World"


