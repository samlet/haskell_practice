-- lib
doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                      then x
                      else  x*2

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 编写函数时，给它一个明确的型别声明是个好习惯，比较短的函数就不用多此一举了。还记得前面那个过滤大写字母的 List Comprehension 吗？给它加上型别声明便是这个样子：     
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- 一个将三个整数相加的简单函数。
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z


main =  do putStrLn "hello world."
           putStrLn (show (doubleMe 5))




