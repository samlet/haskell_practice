-- toplevel
-- monads_list.hs

-- List Monad

-- 我们已经了解了 Maybe 可以被看作具有失败可能性 context 的值，也见识到如何用 >>= 来把这些具有失败考量的值传给函数。在这一个章节中，我们要看一下如何利用 list 的 monadic 的性质来写 non-deterministic 的程序。
-- 我们已经讨论过在把 list 当作 applicatives 的时候他们具有 non-deterministic 的性质。像 5 这样一个值是 deterministic 的。他只有一种结果，而且我们清楚的知道他是什么结果。另一方面，像[3,8,9] 这样的值包含好几种结果，所以我们能把他看作是同时具有好几种结果的值。把 list 当作 applicative functors 展示了这种特性：
(*) <$> [1,2,3] <*> [10,100,1000]  
[10,100,1000,20,200,2000,30,300,3000]

-- 将左边 list 中的元素乘上右边 list 中的元素这样所有的组合全都被放进结果的 list 中。当处理 non-determinism 的时候，这代表我们有好几种选择可以选，我们也会每种选择都试试看，因此最终的结果也会是一个 non-deterministic 的值。只是包含更多不同可能罢了。
-- non-determinism 这样的 context 可以被漂亮地用 monad 来考虑。所以我们这就来看看 list 的Monad instance 的定义：
-- instance Monad [] where  
--     return x = [x]  
--     xs >>= f = concat (map f xs)  
--     fail _ = []
-- return 跟 pure 是做同样的事，所以我们应该算已经理解了 return 的部份。他接受一个值，并把他放进一个最小的一个 context 中。换种说法，就是他做了一个只包含一个元素的 list。这样对于我们想要操作普通值的时候很有用，可以直接把他包起来变成 non-deterministic value。
-- 要理解 >>= 在 list monad 的情形下是怎么运作的，让我们先来回归基本。>>= 基本上就是接受一个有 context 的值，把他喂进一个只接受普通值的函数，并回传一个具有 context 的值。如果操作的函数只会回传普通值而不是具有 context 的值，那 >>= 在操作一次后就会失效，因为 context 不见了。让我们来试着把一个 non-deterministic value 塞到一个函数中：
[3,4,5] >>= \x -> [x,-x]  
[3,-3,4,-4,5,-5]
-- 当我们对 Maybe 使用 >>=，是有考虑到可能失败的 context。在这边 >>= 则是有考虑到 non-determinism。[3,4,5] 是一个 non-deterministic value，我们把他喂给一个回传 non-deterministic value 的函数。那结果也会是 non-deterministic。而且他包含了所有从 [3,4,5] 取值，套用 \x -> [x,-x] 后的结果。这个函数他接受一个数值并产生两个数值，一个原来的数值与取过负号的数值。当我们用 >>= 来把一个 list 喂给这个函数，所有在 list 中的数值都保留了原有的跟取负号过的版本。x 会针对 list 中的每个元素走过一遍。
-- 要看看结果是如何算出来的，只要看看实作就好了。首先我们从 [3,4,5] 开始。然后我们用 lambda 映射过所有元素得到：
[[3,-3],[4,-4],[5,-5]]
-- lambda 会扫过每个元素，所以我们有一串包含一堆 list 的 list，最后我们在把这些 list 压扁，得到一层的 list。这就是我们得到 non-deterministic value 的过程。
-- non-determinism 也有考虑到失败的可能性。[] 其实等价于 Nothing，因为他什么结果也没有。所以失败等同于回传一个空的 list。所有的错误消息都不用。让我们来看看范例：
[] >>= \x -> ["bad","mad","rad"]  
[]  
[1,2,3] >>= \x -> []  
[]
-- 第一行里面，一个空的 list 被丢给 lambda。因为 list 没有任何元素，所以函数收不到任何东西而产生空的 list。这跟把 Nothing 喂给函数一样。第二行中，每一个元素都被喂给函数，但所有元素都被丢掉，而只回传一个空的 list。因为所有的元素都造成了失败，所以整个结果也代表失败。
-- 就像 Maybe 一样，我们可以用 >>= 把他们串起来：
[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- [1,2] 被绑定到 n 而 ['a','b'] 被绑定到 ch。最后我们用 return (n,ch) 来把他放到一个最小的 context 中。在这个案例中，就是把 (n,ch) 放到 list 中，这代表最低程度的 non-determinism。整套结构要表达的意思就是对于 [1,2] 的每个元素，以及 ['a','b'] 的每个元素，我们产生一个 tuple，每项分别取自不同的 list。
-- 一般来说，由于 return 接受一个值并放到最小的 context 中，他不会多做什么额外的东西仅仅是展示出结果而已。
-- 当你要处理 non-deterministic value 的时候，你可以把 list 中的每个元素想做计算路线的一个 branch。
-- 这边把先前的表达式用 do 重写：
:{
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)
:}
-- 这样写可以更清楚看到 n 走过 [1,2] 中的每一个值，而 ch 则取过 ['a','b'] 中的每个值。正如 Maybe 一般，我们从 monadic value 中取出普通值然后喂给函数。>>= 会帮我们处理好一切 context 相关的问题，只差在这边的 context 指的是 non-determinism。
-- 使用 do 来对 list 操作让我们回想起之前看过的一些东西。来看看下列的片段：
[ (n,ch) | n <- [1,2], ch <- ['a','b'] ]  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- 没错，就是 list comprehension。在先前的范例中，n 会走过 [1,2] 的每个元素，而 ch 会走过['a','b'] 的每个元素。同时我们又把 (n,ch) 放进一个 context 中。这跟 list comprehension 的目的一样，只是我们在 list comprehension 里面不用在最后写一个 return 来得到 (n,ch) 的结果。

-- 实际上，list comprehension 不过是一个语法糖。不论是 list comprehension 或是用 do 表示法来表示，他都会转换成用 >>= 来做计算。
-- List comprehension 允许我们 filter 我们的结果。举例来说，我们可以只要包含 7 在表示位数里面的数值。
[ x | x <- [1..50], '7' `elem` show x ]  
[7,17,27,37,47]
-- 我们用 show 跟 x 来把数值转成字串，然后检查 '7' 是否包含在字串里面。要看看 filtering 要如何转换成用 list monad 来表达，我们可以考虑使用 guard 函数，还有 MonadPlus 这个 type class。MonadPlus 这个 type class 是用来针对可以同时表现成 monoid 的 monad。下面是他的定义：
:{
class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a
:}
-- mzero 是其实是 Monoid 中 mempty 的同义词，而 mplus 则对应到 mappend。因为 list 同时是 monoid 跟 monad，他们可以是 MonadPlus 的 instance。
:{
instance MonadPlus [] where  
    mzero = []  
    mplus = (++)
:}

-- 对于 list 而言，mzero 代表的是不产生任何结果的 non-deterministic value，也就是失败的结果。而mplus 则把两个 non-deterministic value 结合成一个。guard 这个函数被定义成下列形式：
:{
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero
:}

-- 这函数接受一个布林值，如果他是 True 就回传一个包在缺省 context 中的 ()。如果他失败就产生 mzero。
-- guard (5 > 2) :: Maybe ()  
-- Just ()  
-- guard (1 > 2) :: Maybe ()  
-- Nothing  
guard (5 > 2) :: [()]  
[()]  
guard (1 > 2) :: [()]  
[]
-- 看起来蛮有趣的，但用起来如何呢？我们可以用他来过滤 non-deterministic 的计算。
[1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)  
[7,17,27,37,47]
-- 这边的结果跟我们之前 list comprehension 的结果一致。究竟 guard 是如何办到的？我们先看看guard 跟 >> 是如何交互：
guard (5 > 2) >> return "cool" :: [String]  
["cool"]  
guard (1 > 2) >> return "cool" :: [String]  
[]
-- 如果 guard 成功的话，结果就会是一个空的 tuple。接着我们用 >> 来忽略掉空的 tuple，而呈现不同的结果。另一方面，如果 guard 失败的话，后面的 return 也会失败。这是因为用 >>= 把空的 list 喂给函数总是会回传空的 list。基本上 guard 的意思就是：如果一个布林值是 False 那就产生一个失败状态，不然的话就回传一个基本的 ()。这样计算就可以继续进行。
-- 这边我们把先前的范例用 do 改写：
:{
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x
:}
-- 如果我们不写最后一行 return x，那整个 list 就会是包含一堆空 tuple 的 list。
-- 把上述范例写成 list comprehension 的话就会像这样：
[ x | x <- [1..50], '7' `elem` show x ]  
[7,17,27,37,47]
-- 所以 list comprehension 的 filtering 基本上跟 guard 是一致的。



