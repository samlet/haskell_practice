-- lib

main = do putStrLn (lucky 5)
          putStrLn (show (factorial 5))
          putStrLn (bmiTell 25.0)

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

-- 三元组 (Tripple) 呢？嗯，没现成的函数，得自己动手：
first :: (a, b, c) -> a  
first (x, _, _) = x  

second :: (a, b, c) -> b  
second (_, y, _) = y  

third :: (a, b, c) -> c  
third (_, _, z) = z

-- 实现个我们自己的 head 函数。
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x

-- 注意下，你若要绑定多个变量(用 _ 也是如此)，我们必须用括号将其括起。同时注意下我们用的这个 error 函数，它可以生成一个运行时错误，用参数中的字串表示对错误的描述。它会直接导致进程崩溃，因此应谨慎使用。可是对一个空 List 取 head 真的不靠谱哇。
-- 弄个简单函数，让它用非标准的英语给我们展示 List 的前几项。
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
-- 这个函数顾及了空 List，单元素 List，双元素 List 以及较长的 List，所以这个函数很安全。(x:[]) 与(x:y:[]) 也可以写作 [x] 和 [x,y] (有了语法糖，我们不必多加括号)。不过 (x:y:_) 这样的模式就不行了，因为它匹配的 List 长度不固定。

-- 我们曾用 List Comprehension 实现过自己的 length 函数，现在用模式匹配和递归重新实现它：
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs

-- 实现 sum。我们知道空 List 的和是 0，就把它定义为一个模式。我们也知道一个 List 的和就是头部加上尾部的和的和。写下来就成了：
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs
-- 还有个东西叫做 as 模式，就是将一个名字和 @ 置于模式前，可以在按模式分割什么东西时仍保留对其整体的引用。如这个模式 xs@(x:y:ys)，它会匹配出与 x:y:ys 对应的东西，同时你也可以方便地通过xs 得到整个 List，而不必在函数体中重复 x:y:ys。看下这个 quick and dirty 的例子：
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- 模式用来检查一个值是否合适并从中取值，而 guard 则用来检查一个值的某项属性是否为真。咋一听有点像是 if 语句，实际上也正是如此。不过处理多个条件分支时 guard 的可读性要高些，并且与模式匹配契合的很好。

-- 我们先看一个用到 guard 的函数。它会依据你的 BMI 值 (body mass index，身体质量指数)来不同程度地侮辱你。BMI 值即为体重除以身高的平方。如果小于 18.5，就是太瘦；如果在 18.5 到 25 之间，就是正常；25 到 30 之间，超重；如果超过 30，肥胖。这就是那个函数(我们目前暂不为您计算 BMI，它只是直接取一个 BMI 值)。
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"


-- guard 可以在含有任意数量参数的函数中使用。省得用户在使用这函数之前每次都自己计算 bmi。我们修改下这个函数，让它取身高体重为我们计算。
bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"

-- 你可以测试自己胖不胖
-- ghci> bmiTell 85 1.90  
-- "You're supposedly normal. Pffft, I bet you're ugly!"

max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b

-- guard 也可以塞在一行里面。但这样会丧失可读性，因此是不被鼓励的。即使是较短的函数也是如此，不过出于展示，我们可以这样重写 max'：
max'' :: (Ord a) => a -> a -> a  
max'' a b | a > b = a | otherwise = b

-- 我们再来试试用 guard 实现我们自己的 compare 函数：
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT
-- ghci> 3 `myCompare` 2  
-- GT
-- *Note*：通过反单引号，我们不仅可以以中缀形式调用函数，也可以在定义函数的时候使用它。有时这样会更易读。

-- 关键字 Where
bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"

-- 注意，我们重复了 3 次。我们重复了 3 次。程序员的字典里不应该有"重复"这个词。既然发现有重复，那么给它一个名字来代替这三个表达式会更好些。嗯，我们可以这样修改：
bmiTell'' :: (RealFloat a) => a -> a -> String  
bmiTell'' weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
-- 我们的 where 关键字跟在 guard 后面(最好是与竖线缩进一致)，可以定义多个名字和函数。这些名字对每个 guard 都是可见的，这一来就避免了重复。如果我们打算换种方式计算 bmi，只需进行一次修改就行了。通过命名，我们提升了代码的可读性，并且由于 bmi 只计算了一次，函数的执行效率也有所提升。我们可以再做下修改：
bmiTell''' :: (RealFloat a) => a -> a -> String  
bmiTell''' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0
-- 函数在 where 绑定中定义的名字只对本函数可见，因此我们不必担心它会污染其他函数的命名空间。注意，其中的名字都是一列垂直排开，如果不这样规范，Haskell 就搞不清楚它们在哪个地方了。

-- where 绑定也可以使用模式匹配！前面那段代码可以改成：
bmiTell'''' :: (RealFloat a) => a -> a -> String  
bmiTell'''' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- 我们再搞个简单函数，让它告诉我们姓名的首字母：
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname


-- where 绑定可以定义名字，也可以定义函数。保持健康的编程语言风格，我们搞个计算一组 bmi 的函数：
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs] 
    where bmi weight height = weight / height ^ 2
-- 这就全了！在这里将 bmi 搞成一个函数，是因为我们不能依据参数直接进行计算，而必须先从传入函数的 List 中取出每个序对并计算对应的值。
-- where 绑定还可以一层套一层地来使用。 有个常见的写法是，在定义一个函数的时候也写几个辅助函数摆在 where 绑定中。 而每个辅助函数也可以透过 where 拥有各自的辅助函数。


-- Case expressions
-- 模式匹配本质上不过就是 case 语句的语法糖而已。这两段代码就是完全等价的：
head'' :: [a] -> a  
head'' [] = error "No head for empty lists!"  
head'' (x:_) = x

head''' :: [a] -> a  
head''' xs = case xs of [] -> error "No head for empty lists!"  
                        (x:_) -> x


-- expression 匹配合适的模式。 一如预期地，第一个模式若匹配，就执行第一个区块的代码；否则就接下去比对下一个模式。如果到最后依然没有匹配的模式，就会产生运行时错误。
-- 函数参数的模式匹配只能在定义函数时使用，而 case 表达式可以用在任何地方。例如：
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."
-- 这在表达式中作模式匹配很方便，由于模式匹配本质上就是 case 表达式的语法糖，那么写成这样也是等价的：
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

-- 递归
-- 现在看看递归的思路是如何：我们先定下一个边界条件，即处理单个元素的 List 时，回传该元素。如果该 List 的头部大于尾部的最大值，我们就可以假定较长的 List 的最大值就是它的头部。而尾部若存在比它更大的元素，它就是尾部的最大值。就这么简单！现在，我们在 Haskell 中实现它
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs


-- 改用 max 函数会使代码更加清晰。如果你还记得，max 函数取两个值做参数并回传其中较大的值。如下便是用 max 函数重写的 maximun'
-- 一个 List 的最大值就是它的首个元素与它尾部中最大值相比较所得的结果
maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum' xs)

-- 现在我们已经了解了递归的思路,接下来就使用递归来实现几个函数. 先实现下 replicate 函数, 它取一个Int 值和一个元素做参数, 回传一个包含多个重复元素的 List, 如 replicate 3 5 回传 [5,5,5]. 考虑一下, 我觉得它的边界条件应该是负数. 如果要 replicate 重复某元素零次, 那就是空 List. 负数也是同样, 不靠谱.
-- 在这里我们使用了 guard 而非模式匹配, 是因为这里做的是布林判断. 如果 n 小于 0 就回传一个空 List, 否则, 回传以 x 作首个元素并后接重复 n-1 次 x 的 List. 最后, (n-1) 的那部分就会令函数抵达边缘条件.
-- *Note*: Num 不是 Ord 的子集, 表示数字不一定得拘泥于排序, 这就是在做加减法比较时要将 Num 与 Ord 型别约束区别开来的原因.

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

-- 接下来实现 take 函数, 它可以从一个 List 取出一定数量的元素. 如 take 3 [5,4,3,2,1], 得 [5,4,3]. 若要取零或负数个的话就会得到一个空 List. 同样, 若是从一个空 List中取值, 它会得到一个空 List. 注意, 这儿有两个边界条件, 写出来:
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

-- reverse 函数简单地反转一个 List, 动脑筋想一下它的边界条件! 该怎样呢? 想想...是空 List! 空 List 的反转结果还是它自己. Okay, 接下来该怎么办? 好的, 你猜的出来. 若将一个 List 分割为头部与尾部, 那它反转的结果就是反转后的尾部与头部相连所得的 List.
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat 函数取一个元素作参数, 回传一个仅包含该元素的无限 List. 它的递归实现简单的很, 看:
-- 调用 repeat 3 会得到一个以 3 为头部并无限数量的 3 为尾部的 List, 可以说 repeat 3 运行起来就是3:repeat 3 , 然后 3:3:3:3 等等. 若执行 repeat 3, 那它的运算永远都不会停止。而 take 5 (repeat 3) 就可以得到 5 个 3, 与 replicate 5 3 差不多.

repeat' :: a -> [a]  
repeat' x = x:repeat' x

-- zip 取两个 List 作参数并将其捆在一起。zip [1,2,3] [2,3] 回传 [(1,2),(2,3)], 它会把较长的 List 从中间断开, 以匹配较短的 List. 用 zip 处理一个 List 与空 List 又会怎样? 嗯, 会得一个空 List, 这便是我们的限制条件, 由于 zip 取两个参数, 所以要有两个边缘条件
-- 第三个模式表示将两个 List 捆绑的行为, 即将其头部配对并后跟捆绑的尾部. 用 zip 处理 [1,2,3] 与 ['a','b'] 的话, 就会在 [3] 与 [] 时触及边界条件, 得到 (1,'a'):(2,'b'):[] 的结果,与 [(1,'a'),(2,'b')] 等价.

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- 再实现一个标准库函数 -- elem! 它取一个元素与一个 List 作参数, 并检测该元素是否包含于此 List. 而边缘条件就与大多数情况相同, 空 List. 大家都知道空 List 中不包含任何元素, 便不必再做任何判断
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs

-- "快速"排序
-- 它的型别声明应为 quicksort :: (Ord a) => [a] -> [a], 没啥奇怪的. 边界条件呢? 如料，空 List。排过序的空 List 还是空 List。接下来便是算法的定义：排过序的 List 就是令所有小于等于头部的元素在先(它们已经排过了序), 后跟大于头部的元素(它们同样已经拍过了序)。 注意定义中有两次排序，所以就得递归两次！同时也需要注意算法定义的动词为"是"什么而非"做"这个, "做"那个, 再"做"那个...这便是函数式编程之美！如何才能从 List 中取得比头部小的那些元素呢？List Comprehension。好，动手写出这个函数！
-- 若给 [5,1,9,4,6,7,3] 排序，这个算法就会取出它的头部，即 5。 将其置于分别比它大和比它小的两个 List 中间，得 [1,4,3] ++ [5] ++ [9,6,7], 我们便知道了当排序结束之时，5会在第四位，因为有3个数比它小每，也有三个数比它大。好的，接着排 [1,4,3] 与 [9,6,7], 结果就出来了！对它们的排序也是使用同样的函数，将它们分成许多小块，最终到达临界条件，即空 List 经排序依然为空

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =  
  let smallerSorted = quicksort [a | a <- xs, a <= x] 
      biggerSorted = quicksort [a | a <- xs, a > x]  
  in smallerSorted ++ [x] ++ biggerSorted














