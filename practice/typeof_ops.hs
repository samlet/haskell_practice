-- toplevel

-- 可以使用 ghci 来检测表达式的型别。使用 :t 命令后跟任何可用的表达式，即可得到该表达式的型别，先试一下：
:t 'a'  
'a' :: Char  
:t True  
True :: Bool  
:t "HELLO!"  
"HELLO!" :: [Char]  
:t (True, 'a')  
(True, 'a') :: (Bool, Char)  
:t 4 == 5  
4 == 5 :: Bool

-- 加上型别声明便是这个样子：
-- removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- 你觉得 head 函数的型别是啥？它可以取任意型别的 List 的首项，是怎么做到的呢？我们查一下！
:t head  
-- head :: [a] -> a

-- 还记得 fst？我们查一下它的型别：
:t fst  
-- fst :: (a, b) -> a
-- 可以看到fst取一个包含两个型别的 Tuple 作参数，并以第一个项的型别作为回传值。这便是 fst 可以处理一个含有两种型别项的 pair 的原因。注意，a 和 b 是不同的型别变量，但它们不一定非得是不同的型别，它只是标明了首项的型别与回传值的型别相同。

-- Typeclasses
-- 型别定义行为的接口，如果一个型别属于某 Typeclass，那它必实现了该 Typeclass 所描述的行为。很多从 OOP 走过来的人们往往会把 Typeclass 当成面向对象语言中的 class 而感到疑惑，厄，它们不是一回事。易于理解起见，你可以把它看做是 Java 的 interface。
-- 函数的型别声明是怎样的？
:t (==)  
-- (==) :: (Eq a) => a -> a -> Bool

-- Ord 包含可比较大小的型别。除了函数以外，我们目前所谈到的所有型别都属于 Ord 类。Ord 包中包含了<, >, <=, >= 之类用于比较大小的函数。compare 函数取两个 Ord 类中的相同型别的值作参数，回传比较的结果。这个结果是如下三种型别之一：GT, LT, EQ。
:t (>)  
-- (>) :: (Ord a) => a -> a -> Bool
-- 型别若要成为Ord的成员，必先加入Eq家族。
"Abrakadabra" < "Zebra"  
True  
"Abrakadabra" `compare` "Zebra"  
LT  
5 >= 2  
True  
5 `compare` 3  
GT

-- Show 的成员为可用字串表示的型别。目前为止，除函数以外的所有型别都是 Show 的成员。操作 Show Typeclass，最常用的函数表示 show。它可以取任一Show的成员型别并将其转为字串。
show 3  
"3"  
show 5.334  
"5.334"  
show True  
"True"
-- Read 是与 Show 相反的 Typeclass。read 函数可以将一个字串转为 Read 的某成员型别。
read "True" || False  
True  
read "8.2" + 3.8  
12.0  
read "5" - 2  
3  
read "[1,2,3,4]" ++ [3]  
[1,2,3,4,3]
-- 一切良好，如上的所有型别都属于这一 Typeclass。尝试 read "4" 又会怎样？
-- ghci 跟我们说它搞不清楚我们想要的是什么样的回传值。注意调用 read 后跟的那部分，ghci 通过它来辨认其型别。若要一个 boolean 值，他就知道必须得回传一个 Bool 型别的值。但在这里它只知道我们要的型别属于 Read Typeclass，而不能明确到底是哪个。看一下 read 函数的型别声明吧：
:t read  
-- read :: (Read a) => String -> a
-- 看，它的回传值属于 ReadTypeclass，但我们若用不到这个值，它就永远都不会得知该表达式的型别。所以我们需要在一个表达式后跟:: 的型别注释，以明确其型别。如下：
read "5" :: Int  
5  
read "5" :: Float  
5.0  
(read "5" :: Float) * 4  
20.0  
read "[1,2,3,4]" :: [Int]  
[1,2,3,4]  
read "(3, 'a')" :: (Int, Char)  
(3, 'a')
-- 编译器可以辨认出大部分表达式的型别，但遇到 read "5" 的时候它就搞不清楚究竟该是 Int 还是 Float 了。只有经过运算，Haskell 才会明确其型别；同时由于 Haskell 是静态的，它还必须得在 编译前搞清楚所有值的型别。所以我们就最好提前给它打声招呼："嘿，这个表达式应该是这个型别，省的你认不出来！"
-- Enum 的成员都是连续的型别 -- 也就是可枚举。Enum 类存在的主要好处就在于我们可以在 Range 中用到它的成员型别：每个值都有后继子 (successer) 和前置子 (predecesor)，分别可以通过 succ 函数和pred 函数得到。该 Typeclass 包含的型别有：(), Bool, Char, Ordering, Int, Integer,Float 和 Double。
['a'..'e']  
"abcde"  
[LT .. GT]  
[LT,EQ,GT]  
[3 .. 5]  
[3,4,5]  
succ 'B'  
'C'
-- Bounded 的成员都有一个上限和下限。
minBound :: Int  
-2147483648  
maxBound :: Char  
'\1114111'  
maxBound :: Bool  
True  
minBound :: Bool  
False
-- minBound 和 maxBound 函数很有趣，它们的型别都是 (Bounded a) => a。可以说，它们都是多态常量。
-- 如果其中的项都属于 Bounded Typeclass，那么该 Tuple 也属于 Bounded
maxBound :: (Bool, Int, Char)  
(True,2147483647,'\1114111')
-- Num 是表示数字的 Typeclass，它的成员型别都具有数字的特征。检查一个数字的型别：
:t 20  
-- 20 :: (Num t) => t
-- 看样子所有的数字都是多态常量，它可以作为所有 Num Typeclass中的成员型别。以上便是 NumTypeclass 中包含的所有型别，检测 * 运算子的型别，可以发现它可以处理一切的数字：
:t (*)  
-- (*) :: (Num a) => a -> a -> a
-- 它只取两个相同型别的参数。所以 (5 :: Int) * (6 :: Integer) 会引发一个型别错误，而 5 * (6 :: Integer) 就不会有问题。





