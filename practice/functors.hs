-- lib

main = do putStrLn (lucky 5)
          putStrLn (show (factorial 5))

-- 在定义函数时，你可以为不同的模式分别定义函数本身，这就让代码更加简洁易读。你可以匹配一切数据型别 --- 数字，字符，List，元组，等等。我们弄个简单函数，让它检查我们传给它的数字是不是 7。
-- 在调用 lucky 时，模式会从上至下进行检查，一旦有匹配，那对应的函数体就被应用了。这个模式中的唯一匹配是参数为 7，如果不是 7，就转到下一个模式，它匹配一切数值并将其绑定为 x 。这个函数完全可以使用 if 实现，不过我们若要个分辨 1 到 5 中的数字，而无视其它数的函数该怎么办？要是没有模式匹配的话，那可得好大一棵 if-else 树了！

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

-- 记得前面实现的那个阶乘函数么？当时是把 n 的阶乘定义成了 product [1..n]。也可以写出像数学那样的递归实现，先说明 0 的阶乘是 1 ，再说明每个正整数的阶乘都是这个数与它前驱 (predecessor) 对应的阶乘的积。如下便是翻译到 Haskell 的样子：
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

-- 将二维空间中的矢量相加该如何？
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- 我们再来试试用 guard 实现我们自己的 compare 函数：
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT
-- ghci> 3 `myCompare` 2  
-- GT
-- *Note*：通过反单引号，我们不仅可以以中缀形式调用函数，也可以在定义函数的时候使用它。有时这样会更易读。

-- 关键字 Let
-- let 绑定与 where 绑定很相似。where 绑定是在函数底部定义名字，对包括所有 guard 在内的整个函数可见。let 绑定则是个表达式，允许你在任何位置定义局部变量，而对不同的 guard 不可见。正如 Haskell 中所有赋值结构一样，let 绑定也可以使用模式匹配。看下它的实际应用！这是个依据半径和高度求圆柱体表面积的函数：
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

-- 你也可以把 let 绑定放到 List Comprehension 中。我们重写下那个计算 bmi 值的函数，用个 let 替换掉原先的 where。
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- List Comprehension 中 let 绑定的样子和限制条件差不多，只不过它做的不是过滤，而是绑定名字。let 中绑定的名字在输出函数及限制条件中都可见。这一来我们就可以让我们的函数只返回胖子的bmi 值：
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
-- 在 (w, h) <- xs 这里无法使用 bmi 这名字，因为它在 let 绑定的前面。

-- 你说既然 let 已经这么好了，还要 where 干嘛呢？嗯，let 是个表达式，定义域限制的相当小，因此不能在多个 guard 中使用。一些朋友更喜欢 where，因为它是跟在函数体后面，把主函数体距离型别声明近一些会更易读。

-- Curried functions

-- 我们若以不全的参数来调用某函数，就可以得到一个不全调用的函数。 如果你高兴，构造新函数就可以如此便捷，将其传给另一个函数也是同样方便。看下这个函数，简单至极:
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- 以不全的参数调用函数可以方便地创造新的函数。例如，搞个取一数与 100 比较大小的函数该如何? 大可这样:
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- 用 99 调用它，就可以得到一个 GT。 简单。 注意下在等号两边都有 x。 想想 compare 100 会回传什么？一个取一数与 100 比较的函数。 Wow，这不正是我们想要的? 这样重写:
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100
-- 型别声明依然相同，因为 compare 100 回传函数。compare 的型别为 (Ord a) => a -> (a -> Ordering)，用 100 调用它后回传的函数型别为 (Num a, Ord a) => a -> Ordering，同时由于 100 还是 Num 型别类的实例，所以还得另留一个类约束。

-- 中缀函数也可以不全调用，用括号把它和一边的参数括在一起就行了。 这回传一个取一参数并将其补到缺少的那一端的函数。 一个简单函数如下:
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
-- 调用 divideByTen 200 就是 (/10) 200，和 200 / 10 等价。

-- 一个检查字符是否为大写的函数:
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
-- 唯一的例外就是 - 运算符，按照前面提到的定义，(-4) 理应回传一个并将参数减 4 的函数，而实际上，处于计算上的方便，(-4) 表示负 4。 若你一定要弄个将参数减 4 的函数，就用 subtract 好了，像这样 (subtract 4).

-- 高阶函数

-- Haskell 中的函数可以取另一个函数做参数，也可以回传函数。 举个例子，我们弄个取一个函数并调用它两次的函数.
-- 首先注意这型别声明。 在此之前我们很少用到括号，因为 (->) 是自然的右结合，不过在这里括号是必须的。 它标明了首个参数是个参数与回传值型别都是a的函数，第二个参数与回传值的型别也都是a。 我们可以用 Curried functions 的思路来理解这一函数，不过免得自寻烦恼，我们姑且直接把它看作是取两个参数回传一个值，其首个参数是个型别为 (a->a) 的函数,第二个参数是个 a。 该函数的型别可以是 (Int->Int)，也可以是 (String->String)，但第二个参数必须与之一致。

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)


-- 接下来我们用高阶函数的编程思想来实现个标准库中的函数，它就是 zipWith。 它取一个函数和两个 List 做参数，并把两个 List 交到一起(使相应的元素去调用该函数)。 如下就是我们的实现:
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- 看下这个型别声明，它的首个参数是个函数，取两个参数处理交叉，其型别不必相同，不过相同也没关系。 第二三个参数都是 List，回传值也是个 List。 第一个 List中元素的型别必须是a，因为这个处理交叉的函数的第一个参数是a。 第二个 List 中元素的型别必为 b，因为这个处理交叉的函数第二个参数的型别是b。 回传的 List 中元素型别为 c。 如果一个函数说取一个型别为 a->b->c 的函数做参数，传给它个a->a->c 型别的也是可以的，但反过来就不行了。 可以记下，若在使用高阶函数的时候不清楚其型别为何，就先忽略掉它的型别声明，再到 ghci 下用 :t 命令来看下 Haskell 的型别推导.

-- 接下来实现标准库中的另一个函数 flip，flip简单地取一个函数作参数并回传一个相似的函数，只是它们的两个参数倒了个。
flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x

-- 可以改成更简单的写法: 在这里我们就利用了 Curried functions 的优势，只要调用 flip' f 而不带 y和x，它就会回传一个俩参数倒个的函数。 flip 处理的函数往往都是用来传给其他函数调用，于是我们可以发挥 Curried functions 的优势，预先想好发生完全调用的情景并处理好回传值.
flip'' :: (a -> b -> c) -> b -> a -> c  
flip'' f y x = f x y

-- quicksort 函数: 换做 filter 也可以实现，而且更加易读
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted

-- 我们如何解决给定周长寻找合适直角三角形的问题的? 在命令式编程中，我们可以套上三个循环逐个测试当前的组合是否满足条件，若满足，就打印到屏幕或其他类似的输出。 而在函数式编程中，这行就都交给 map 和 filter。 你弄个取一参数的函数，把它交给 map 过一遍 List，再 filter 之找到合适的结果。 感谢 Haskell 的惰性，即便是你多次 map 一个 `List 也只会遍历一遍该 List，要找出小于 100000 的数中最大的 3829 的倍数，只需过滤结果所在的 List 就行了.
-- 要找出小于 100000 的 3829 的所有倍数，我们应当过滤一个已知结果所在的 List.
-- 首先，取一个降序的小于 100000 所有数的 List，然后按照限制条件过滤它。 由于这个 List 是降序的，所以结果 List 中的首个元素就是最大的那个数。惰性再次行动! 由于我们只取这结果 List 的首个元素，所以它并不关心这 List 是有限还是无限的，在找到首个合适的结果处运算就停止了。

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0


-- 下个问题与 Collatz 串行有关，取一个自然数，若为偶数就除以 2。 若为奇数就乘以 3 再加 1。 再用相同的方式处理所得的结果，得到一组数字构成的的链。它有个性质，无论任何以任何数字开始，最终的结果都会归 1。所以若拿 13 当作起始数，就可以得到这样一个串行 13，40，20，10，5，16，8，4，2，1。13*3+1 得 40，40 除 2 得 20，如是继续，得到一个 10 个元素的链。
-- 好的，我们想知道的是: 以 1 到 100 之间的所有数作为起始数，会有多少个链的长度大于 15?
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)

-- *Note*: 这函数的型别为 ``numLongChains :: Int``。这是由于历史原因，``length`` 回传一个 ``Int`` 而非 ``Num`` 的成员型别，若要得到一个更通用的 ``Num a``，我们可以使用 ``fromIntegral`` 函数来处理所得结果.
numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15

