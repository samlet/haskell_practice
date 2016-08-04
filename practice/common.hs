
{-|
  You can also use Haskell's nested-comment style for documentation annotations, which is sometimes more convenient when using multi-line comments:

  The 'square' function squares an integer.
  It takes one argument, of type 'Int'.
-}
square :: Int -> Int
square x = x * x

{-|
  fac
-}
let fac n = if n == 0 then 1 else n * fac (n-1)

{-|
https://zh.wikibooks.org/wiki/Haskell/%E5%8F%98%E9%87%8F%E5%92%8C%E5%87%BD%E6%95%B0
Haskell的这里“猜测”只会发生在当它缺乏足够的信息来推断出型别的时候。下面我们会看到，在大多数情况下，Haskell是能够根据上下文的信息来作出推断的。也就是说，是否把一个数字作为Integer，或是其它型别。
-}
let r = 25 :: Double
2 * pi * r

{-|
 除了指定Double类型，你也可以给它一个多态（polymorphic）类型，像Num a => a，意思是"属于Num类的任意类型a"。示例代码如下，它们运行起来像先前的代码一样： 
 -}
let r = 25 :: Num a => a
2 * pi * r

{-|
变量不仅可以保存像3.14这样的数值，还可以保存任何Haskell表达式。那么，为了方便的表达半径是5的圆的面积，我们可以写如下代码：
-}
let area = pi * 5^2

{-|
函数可以接受一个以上的参数。例如，你想要计算一个矩形的面积。这很容易表达：
Prelude> let areaRect l w = l * w
Prelude> areaRect 5 10
-}


