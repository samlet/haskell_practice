-- modules
module Main where
import Data.List  

main = putStrLn "Hello World"

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub
