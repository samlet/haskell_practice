-- toplevel

-- 提示符经常是随着模块的加载而变化。因此经常会变得很长以至在单行中没有太多可视区域用来输入。
-- 可以用ghci的 :set prompt 来进行修改。
-- :set prompt "ghci>"
-- ghci>

-- prelude模块中的类型，值和函数是默认直接可用的，在使用之前我们不需要额外的操作。然而如果需要其他模块中的一些定义，则需要使用ghci的:module方法预先加载。
-- :module + Data.Ratio

2 ^ 5
3.1416 * 5^2

let pi = 3.14159265358979323846264338327950
let r = 25
2 * pi * r

let r = 25 :: Double

-- 单一同态限定（monomorphism restriction）的语言特性。事实上现在你不需要知道这个，所以如果只想快一点你可以跳过这条提示。除了指定Double类型，你也可以给它一个多态（polymorphic）类型，像Num a => a，意思是"属于Num类的任意类型a"。示例代码如下，它们运行起来像先前的代码一样：

let r = 25 :: Num a => a
2 * pi * r

let area = pi * 5^2
let r = 25.0
let area2 = pi * r ^ 2
area2


:{
let prRev = do
    inp <- getLine
    putStrLn $ reverse inp
:}
prRev

not (True && True)

-- 保存为 baby.hs 或任意名称，然后转至保存的位置，打开 ghci，执行 :l baby.hs。这样我们的函数就装载成功，可以调用了。
:l baby  
doubleMe 9
doubleMe 8.3
doubleUs 28 88 + doubleMe 123

----------------------------------------------
-- 用中缀表达式是为了书写方便：我们同样可以用前缀表达式，即操作符在操作数之前。在这种情况下，我们需要用括号将操作符括起来。
2 + 2
4
(+) 2 2
4
-- 上述的这些表达式暗示了一个概念，Haskell有整数和浮点数类型。整数的大小是随意的。下面例子中的(^)表示了整数的乘方。
313 ^ 15
27112218957718876716220410905036741257

-- Haskell中表示布尔逻辑的值有这么两个：True和False。名字中的大写很重要。作用于布尔值得操作符类似于C语言的情况：(&&)表示“逻辑与”，(||)表示“逻辑或”。
True && False
False
False || True
True
有些编程语言中会定义数字0和False同义，但是在Haskell中并没有这么定义，同样的，也Haskell也没有定义非0的值为True。
-- True && 1	-- error

-- 有一个操作符和C语言的相应的不一样，“不等于”。C语言中是用!=表示的，而Haskell是用/=表示的，它看上去很像数学中的≠。
-- 另外，类C的语言中通常用!表示逻辑非的操作，而Haskell中用函数not。
not True
False

-- Haskell给每个操作符一个数值型的优先级值，从1表示最低优先级，到9表示最高优先级。高优先级的操作符先于低优先级的操作符被应用(apply)。在ghci中我们可以用命令:info来查看某个操作符的优先级。
:info (+)
:info (^)






