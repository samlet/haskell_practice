-- shapes_2.hs

module Main where

main = putStrLn "Hello World"

-- 我们的型别还可以更好。增加加一个表示二维空间中点的型别，可以让我们的 Shape 更加容易理解：
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
-- 注意下 Point 的定义，它的型别与值构造子用了相同的名字。没啥特殊含义，实际上，在一个型别含有唯一值构造子时这种重名是很常见的。好的，如今我们的 Circle 含有两个项，一个是 Point 型别，一个是 Float 型别，好作区分。Rectangle 也是同样，我们得修改 surface 函数以适应型别定义的变动。
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 表示移动一个图形的函数该怎么写？它应当取一个 Shape 和表示位移的两个数，返回一个位于新位置的图形。
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
-- 简洁明了。我们再给这一 Shape 的点加上位移的量。

-- 如果不想直接处理 Point，我们可以搞个辅助函数 (auxilliary function)，初始从原点创建图形，再移动它们。
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

