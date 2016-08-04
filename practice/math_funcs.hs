-- :l math_funcs
-- factorial 50  

-- Integer 表示...厄...也是整数，但它是无界的。这就意味着可以用它存放非常非常大的数，我是说非常大。它的效率不如 Int 高。
factorial :: Integer -> Integer  
factorial n = product [1..n]

circumference :: Float -> Float  
circumference r = 2 * pi * r

circumference' :: Double -> Double  
circumference' r = 2 * pi * r

main = putStrLn "Hello, Math!"

