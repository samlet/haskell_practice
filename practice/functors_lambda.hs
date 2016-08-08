-- lib: functors_lambda.hs
module Main where

main = putStrLn "Hello World"

-- 下个问题与 Collatz 串行有关，取一个自然数，若为偶数就除以 2。 若为奇数就乘以 3 再加 1。 再用相同的方式处理所得的结果，得到一组数字构成的的链。它有个性质，无论任何以任何数字开始，最终的结果都会归 1。所以若拿 13 当作起始数，就可以得到这样一个串行 13，40，20，10，5，16，8，4，2，1。13*3+1 得 40，40 除 2 得 20，如是继续，得到一个 10 个元素的链。

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)

-- lambda 就是匿名函数。有些时候我们需要传给高阶函数一个函数，而这函数我们只会用这一次，这就弄个特定功能的 lambda。编写 lambda，就写个 \ (因为它看起来像是希腊字母的 lambda -- 如果你斜视的厉害)，后面是用空格分隔的参数，-> 后面就是函数体。通常我们都是用括号将其括起，要不然它就会占据整个右边部分。

numLongChains :: Int  
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- 最易读的 flip 函数实现了：
-- 尽管这与 flip' f x y = f y x 等价，但它可以更明白地表示出它会产生一个新的函数。flip 常用来处理一个函数，再将回传的新函数传递给 map 或 filter。所以如此使用 lambda 可以更明确地表现出回传值是个函数，可以用来传递给其他函数作参数。

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f = \x y -> f y x


-- 一个 fold 取一个二元函数，一个初始值(我喜欢管它叫累加值)和一个需要折叠的 List。这个二元函数有两个参数，即累加值和 List 的首项(或尾项)，回传值是新的累加值。然后，以新的累加值和新的 List 首项调用该函数，如是继续。到 List 遍历完毕时，只剩下一个累加值，也就是最终的结果。
-- 首先看下 foldl 函数，也叫做左折叠。它从 List 的左端开始折叠，用初始值和 List 的头部调用这二元函数，得一新的累加值，并用新的累加值与 List 的下一个元素调用二元函数。如是继续。
-- 我们再实现下 sum，这次用 fold 替代那复杂的递归：
sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- 考虑到函数的柯里化，可以写出更简单的实现：
-- 这个 lambda 函数 (\acc x -> acc + x ) 与 (+) 等价。我们可以把 xs 等一应参数省略掉，反正调用 foldl (+) 0 会回传一个取 List 作参数的函数。通常，如果你的函数类似 foo a = bar b a， 大可改为 foo = bar b。有柯里化嘛。

sum'' :: (Num a) => [a] -> a  
sum'' = foldl (+) 0

-- elem 是检查某元素是否属于某 List 的函数吧，我就不再提了(唔，刚提了)。用左折叠实现它:
-- 这里我们有什么？起始值与累加值都是布尔值。在处理 fold 时，累加值与最终结果的型别总是相同的。如果你不知道怎样对待起始值，那我告诉你，我们先假设它不存在，以 False 开始。我们要是fold 一个空 List，结果就是 False。然后我们检查当前元素是否为我们寻找的，如果是，就令累加值为True，如果否，就保留原值不变。若 False，及表明当前元素不是。若 True，就表明已经找到了。
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys


-- 累加值可以是任何型别，可以是数值，布尔值，甚至一个新的 List。我们可以用右 fold 实现 map 函数，累加值就是个 List。将 map 处理过的元素一个一个连到一起。很容易想到，起始值就是空 List。
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs
-- 如果我们用 (+3) 来映射 [1,2,3]，它就会先到达 List 的右端，我们取最后那个元素，也就是 3 来调用 (+3)，得 6。追加 (:) 到累加值上，6:[] 得 [6] 并成为新的累加值。用 2 调用 (+3)，得5，追加到累加值，于是累加值成了 [5,6]。再对 1 调用 (+3)，并将结果 4 追加到累加值，最终得结果 [4,5,6]。
-- 当然，我们也完全可以用左折叠来实现它，map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs 就行了。不过问题是，使用 (++) 往 List 后面追加元素的效率要比使用 (:) 低得多。所以在生成新 List 的时候人们一般都是使用右折叠。

-- foldl1 与 foldr1 的行为与 foldl 和 foldr 相似，只是你无需明确提供初始值。他们假定 List 的首个(或末尾)元素作为起始值，并从旁边的元素开始折叠。这一来，sum 函数大可这样实现：sum = foldl1 (+)。这里待折叠的 List 中至少要有一个元素，若使用空 List 就会产生一个运行时错误。不过 foldl 和foldr 与空 List 相处的就很好。所以在使用 fold 前，应该先想下它会不会遇到空 List，如果不会遇到，大可放心使用 foldr1 和 foldl1。
-- 为了体会 fold 的威力，我们就用它实现几个库函数：
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  

reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  

product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  

filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  

head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  

last' :: [a] -> a  
last' = foldl1 (\_ x -> x)

-- 在 map 和 filter 那节中，我们求了小于 10000 的所有奇数的平方的和。如下就是将其置于一个函数中的样子：
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- 身为函数组合狂人，我可能会这么写：
oddSquareSum' :: Integer  
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
-- 不过若是给别人看，我可能就这么写了：
oddSquareSum'' :: Integer  
oddSquareSum'' =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit




