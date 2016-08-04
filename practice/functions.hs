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

:l functors
let multTwoWithNine = multThree 9
multTwoWithNine 2 3
54
let multWithEighteen = multTwoWithNine 2
multWithEighteen 10
180




