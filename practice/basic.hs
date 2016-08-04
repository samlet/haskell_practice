-- toplevel
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



