-- lib
module Main where

main = putStrLn "Hello World"

numbers = [1,2,3,4]
truths  = [True, False, False]
strings = ["here", "are", "some", "strings"]

area r = pi * r^2

mySignum x =
    if x < 0 then 
        -1
    else if x > 0 then 
        1
    else 
        0
f x =
    case x of
      0 -> 1
      1 -> 5
      2 -> 2
      _ -> (-1)


f2 x = case x of
        { 0 -> 1 ; 1 -> 5 ; 2 -> 2 ; _ -> -1 }


