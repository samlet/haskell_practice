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









