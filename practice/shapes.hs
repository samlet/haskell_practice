-- lib

module Main where

main = putStrLn "Hello World"

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- 我们写个函数计算图形面积：
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- 它的型别声明表示了该函数取一个 Shape 值并返回一个 Float 值。写 Circle -> Float 是不可以的，因为 Circle 并非型别，真正的型别应该是 Shape。

