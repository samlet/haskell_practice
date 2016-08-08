-- toplevel
-- 函数可以接受一个以上的参数
let areaRect l w = l * w
areaRect 5 10

-- 计算一个直角三角形的面积
let areaTriangle b h = (b * h) / 2
areaTriangle 3 9

-- 函数中的函数
let areaRect l w = l * w
let areaSquare s = areaRect s s
areaSquare 5

-- if 语句也是个表达式。如果要给刚刚定义的函数的结果都加上 1，可以如此修改：
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
-- 注意函数名最后的那个单引号，它没有任何特殊含义，只是一个函数名的合法字符罢了。通常，我们使用单引号来区分一个稍经修改但差别不大的函数。定义这样的函数也是可以的：
conanO'Brien = "It's a-me, Conan O'Brien!"


-- let 的格式为 let [bindings] in [expressions]。在 let 中绑定的名字仅对 in 部分可见。let里面定义的名字也得对齐到一列。不难看出，这用 where 绑定也可以做到。那么它俩有什么区别呢？看起来无非就是，let 把绑定放在语句前面而 where 放在后面嘛。
-- 不同之处在于，let 绑定本身是个表达式，而 where 绑定则是个语法结构。还记得前面我们讲if语句时提到它是个表达式，因而可以随处安放？

[if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]  
["Woo", "Bar"]  
4 * (if 10 > 5 then 10 else 0) + 2  
42
-- 用 let 绑定也可以实现：
4 * (let a = 9 in a + 1) + 2  
42
-- let 也可以定义局部函数：
[let square x = x * x in (square 5, square 3, square 2)]  
[(25,9,4)]
-- 若要在一行中绑定多个名字，再将它们排成一列显然是不可以的。不过可以用分号将其分开。
(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
(6000000,"Hey there!")
-- 最后那个绑定后面的分号不是必须的，不过加上也没关系。如我们前面所说，你可以在 let 绑定中使用模式匹配。这在从 Tuple 取值之类的操作中很方便。
(let (a,b,c) = (1,2,3) in a+b+c) * 100  
600


-- 在 List Comprehension 中我们忽略了 let 绑定的 in 部分，因为名字的可见性已经预先定义好了。不过，把一个 let...in 放到限制条件中也是可以的，这样名字只对这个限制条件可见。在 ghci 中 in 部分也可以省略，名字的定义就在整个交互中可见。
let zoot x y z = x * y + z  
zoot 3 9 2  
29  
let boot x y z = x * y + z in boot 3 4 2  
14  

-- 所有多个参数的函数都是 Curried functions。 什么意思呢? 取一个例子最好理解，就拿我们的好朋友 max 函数说事吧。它看起来像是取两个参数，回传较大的那个数。 实际上，执行 max 4 5 时，它会首先回传一个取一个参数的函数，其回传值不是 4 就是该参数，取决于谁大。 然后，以 5 为参数调用它，并取得最终结果。 这听着挺绕口的，不过这一概念十分的酷! 如下的两个调用是等价的：
max 4 5
5
(max 4) 5
5

-- start here
:l functors
let multTwoWithNine = multThree 9
multTwoWithNine 2 3
54
let multWithEighteen = multTwoWithNine 2
multWithEighteen 10
180

-- 这个函数是相当的简单，就拿参数 f 当函数，用 x 调用它得到的结果再去调用它。也就可以这样玩:
applyTwice (+3) 10  
16  
applyTwice (++ " HAHA") "HEY"  
"HEY HAHA HAHA"  
applyTwice ("HAHA " ++) "HEY"  
"HAHA HAHA HEY"  
applyTwice (multThree 2 2) 9  
144  
applyTwice (3:) [1]  
[3,3,1]

:l functors
-- 一个简单的高阶函数可以在不同的场合反复使用。 如下便是我们zipWith' 函数本领的冰山一角:
zipWith' (+) [4,2,5,6] [2,6,2,3]  
[6,8,7,9]  
zipWith' max [6,3,2,1] [7,3,1,5]  
[7,3,2,5]  
-- [fail]
-- zipWith' (++) ["foo "，"bar "，"baz "] ["fighters"，"hoppers"，"aldrin"]  
-- ["foo fighters","bar hoppers","baz aldrin"]  
zipWith' (*) (replicate 5 2) [1..]  
[2,4,6,8,10]  
zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
[[3,4,6],[9,20,30],[10,12,12]]
-- 如你所见，一个简单的高阶函数就可以玩出很多花样。命令式语言使用 for、while、赋值、状态检测来实现功能，再包起来留个接口，使之像个函数一样调用。而函数式语言使用高阶函数来抽象出常见的模式，像成对遍历并处理两个 List 或从中筛掉自己不需要的结果。

flip' zip [1,2,3,4,5] "hello"  
[('h',1),('e',2),('l',3),('l',4),('o',5)]  
zipWith (flip' div) [2,2..] [10,8,6,4,2]  
[5,4,3,2,1]

-- map 取一个函数和 List 做参数，遍历该 List 的每个元素来调用该函数产生一个新的 List。 
map (+3) [1,5,3,1,6]  
[4,8,6,4,9]  
map (++ "!") ["BIFF"，"BANG"，"POW"]  
["BIFF!","BANG!","POW!"]  
map (replicate 3) [3..6]  
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]  
map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
[[1,4],[9,16,25,36],[49,64]]  
map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  
[1,3,6,2,2]
-- 你可能会发现，以上的所有代码都可以用 List Comprehension 来替代。map (+3) [1,5,3,1,6] 与 [x+3 | x <- [1,5,3,1,6] 完全等价。

-- filter 函数取一个限制条件和一个 List，回传该 List 中所有符合该条件的元素。
-- 只要 p x 所得的结果为真，就将这一元素加入新 List，否则就无视之。几个使用范例:
filter (>3) [1,5,3,2,1,6,4,3,2,1]  
[5,6,4]  
filter (==3) [1,2,3,4,5]  
[3]  
filter even [1..10]  
[2,4,6,8,10]  
let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]  
[[1,2,3],[3,4,5],[2,2]]  
filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  
"uagameasadifeent"  
filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"  
"GAYBALLS"
-- 同样，以上都可以用 List Comprehension 的限制条件来实现。并没有教条规定你必须在什么情况下用 map和 filter 还是 List Comprehension，选择权归你，看谁舒服用谁就是。 如果有多个限制条件，只能连着套好几个 filter 或用 && 等逻辑函数的组合之，这时就不如 List comprehension 来得爽了。

-- 我们就要找出所有小于 10000 且为奇的平方的和，得先提下 takeWhile 函数，它取一个限制条件和 List 作参数，然后从头开始遍历这一 List，并回传符合限制条件的元素。 而一旦遇到不符合条件的元素，它就停止了。 如果我们要取出字串 "elephants know how to party" 中的首个单词，可以 takeWhile (/=' ') "elephants know how to party"，回传 "elephants"。okay，要求所有小于 10000 的奇数的平方的和，首先就用 (^2) 函数 map 掉这个无限的 List [1..] 。然后过滤之，只取奇数就是了。 在大于 10000 处将它断开，最后前面的所有元素加到一起。 这一切连写函数都不用，在 ghci 下直接搞定.
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  
166650

-- 先从几个初始数据(表示所有自然数的无限 List)，再 map 它，filter 它，切它，直到它符合我们的要求，再将其加起来。 这用 List comprehension 也是可以的，而哪种方式就全看你的个人口味.
sum (takeWhile (<10000) [m | m <- [n^2 | n <- [1..]], odd m])  
166650

-- start here
:l functors
-- 该链止于 1，这便是边界条件。标准的递归函数:
chain 10  
[10,5,16,8,4,2,1]  
chain 1  
[1]  
chain 30  
[30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

numLongChains 

-- 用 map，我们可以写出类似 map (*) [0..] 之类的代码。 如果只是为了例证 Curried functions 和不全调用的函数是真正的值及其原理，那就是你可以把函数传递或把函数装在 List 中(只是你还不能将它们转换为字串)。 迄今为止，我们还只是 map 单参数的函数到 List，如 map (*2) [0..] 可得一组型别为 (Num a) => [a] 的 List，而 map (*) [0..] 也是完全没问题的。* 的型别为 (Num a) => a -> a -> a，用单个参数调用二元函数会回传一个一元函数。如果用 * 来 map 一个 [0..] 的 List，就会得到一组一元函数组成的 List，即 (Num a) => [a->a]。map (*) [0..] 所得的结果写起来大约就是 [(0*),(1*),(2*)..].
let listOfFuns = map (*) [0..]  
(listOfFuns !! 4) 5  
20
-- 取所得 List 的第五个元素可得一函数，与 (*4) 等价。 然后用 5 调用它，与 (* 4) 5 或 4*5 都是等价的.

-- lambda 是个表达式，因此我们可以任意传递。表达式 (\xs -> length xs > 15) 回传一个函数，它可以告诉我们一个 List 的长度是否大于 15。
-- 不熟悉 Curried functions 与不全调用的人们往往会写出很多 lambda，而实际上大部分都是没必要的。例如，表达式 map (+3) [1,6,3,2] 与 map (\x -> x+3) [1,6,3,2] 等价，(+3) 和 (\x -> x+3) 都是给一个数加上 3。不用说，在这种情况下不用 lambda 要清爽的多。和普通函数一样，lambda 也可以取多个参数。
zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
[153.0,61.5,31.0,15.75,6.6]
-- 同普通函数一样，你也可以在 lambda 中使用模式匹配，只是你无法为一个参数设置多个模式，如 [] 和(x:xs)。lambda 的模式匹配若失败，就会引发一个运行时错误，所以慎用！
map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  
[3,8,9,8,7]

:l functors_lambda
sum' [3,5,2,1]  

-- scanl 和 scanr 与 foldl 和 foldr 相似，只是它们会记录下累加值的所有状态到一个 List。也有scanl1 和 scanr1。
scanl (+) 0 [3,5,2,1]  
[0,3,8,10,11]  
scanr (+) 0 [3,5,2,1]  
[11,8,3,1,0]  
scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
[3,4,5,5,7,9,9,9]  
scanl (flip (:)) [] [3,2,1]  
[[],[3],[2,3],[1,2,3]]

-- 当使用 scanl 时，最终结果就是 List 的最后一个元素。而在 scanr 中则是第一个。
-- scan 可以用来跟踪 fold 函数的执行过程。想想这个问题，取所有自然数的平方根的和，寻找在何处超过 1000？ 先map sqrt [1..]，然后用个 fold 来求它们的和。但在这里我们想知道求和的过程，所以使用 scan，scan 完毕时就可以得到小于 1000 的所有和。所得结果 List 的第一个元素为 1，第二个就是 1+根2，第三个就是 1+根2+根3。若有 x 个和小于 1000，那结果就是 x+1。

sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
sqrtSums  
131  
sum (map sqrt [1..131])  
1005.0942035344083  
sum (map sqrt [1..130])  
993.6486803921487

--  $ 函数。它也叫作函数调用符。先看下它的定义：
-- ($) :: (a -> b) -> a -> b  
-- f $ x = f x
-- 试想有这个表达式： 
sum (map sqrt [1..130])
-- 由于低优先级的 $，我们可以将其改为 
sum $ map sqrt [1..130]

-- 可以将 
sum (filter (> 10) (map (*2) [2..10]) 
-- 重写为 
sum $ filter (> 10) $ map (*2) [2..10]

-- 除了减少括号外，$ 还可以将数据作为函数使用。例如映射一个函数调用符到一组函数组成的 List：
map ($ 3) [(4+),(10*),(^2),sqrt]  
[7.0,30.0,9.0,1.7320508075688772]

-- 函数组合的用处之一就是生成新函数，并传递给其它函数。当然我们可以用 lambda 实现，但大多数情况下，使用函数组合无疑更清楚。假设我们有一组由数字组成的 List，要将其全部转为负数，很容易就想到应先取其绝对值，再取负数，像这样：
map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24]

-- 注意下这个 lambda 与那函数组合是多么的相像。用函数组合，我们可以将代码改为：
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24]
-- 漂亮！函数组合是右结合的，我们同时组合多个函数。表达式 f (g (z x))与 (f . g . z) x 等价。按照这个思路，我们可以将
map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
[-14,-15,-27]
-- 改为：
map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
[-14,-15,-27]

-- 不过含多个参数的函数该怎么办？好，我们可以使用不全调用使每个函数都只剩下一个参数。
sum (replicate 5 (max 6.7 8.9)) 
-- 可以重写为 
(sum . replicate 5 . max 6.7) 8.9 
-- 或 
sum . replicate 5 . max 6.7 $ 8.9
-- 。在这里会产生一个函数，它取与 max 6.7 同样的参数，并使用结果调用 replicate 5 再用 sum 求和。最后用 8.9 调用该函数。不过一般你可以这么读，用 8.9 调用 max 6.7，然后使它 replicate 5，再 sum 之。如果你打算用函数组合来替掉那堆括号，可以先在最靠近参数的函数后面加一个 $，接着就用 . 组合其所有函数调用，而不用管最后那个参数。如果有这样一段代码：
replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- ，可以改为：
replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
-- 。如果表达式以 3 个括号结尾，就表示你可以将其修改为函数组合的形式。
-- 函数组合的另一用途就是定义 point free style (也称作 pointless style) 的函数。就拿我们之前写的函数作例子：
-- sum' :: (Num a) => [a] -> a     
sum' xs = foldl (+) 0 xs
-- 等号的两端都有个 xs。由于有柯里化 (Currying)，我们可以省掉两端的 xs。foldl (+) 0 回传的就是一个取一 List 作参数的函数，我们把它修改为 sum' = foldl (+) 0，这就是 point free style。下面这个函数又该如何改成 point free style 呢？
fn x = ceiling (negate (tan (cos (max 50 x))))
-- 像刚才那样简单去掉两端的 x 是不行的，函数定义中 x 的右边还有括号。cos (max 50) 是有错误的，你不能求一个函数的余弦。我们的解决方法就是，使用函数组合。
fn = ceiling . negate . tan . cos . max 50
-- 漂亮！point free style 会令你去思考函数的组合方式，而非数据的传递方式，更加简洁明了。你可以将一组简单的函数组合在一起，使之形成一个复杂的函数。不过函数若过于复杂，再使用 point free style 往往会适得其反，因此构造较长的函数组合链是不被鼓励的(虽然我本人热衷于函数组合)。更好的解决方法，就是使用 let 语句给中间的运算结果绑定一个名字，或者说把问题分解成几个小问题再组合到一起。这样一来我们代码的读者就可以轻松些，不必要纠结那巨长的函数组合链了。









