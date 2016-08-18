-- toplevel
-- monads.hs
-- Monad，基本上他是一种加强版的 Applicative Functor，正如 Applicative Functor 是 Functor 的加强版一样。

-- 介绍到 Functor 是因为我们观察到有许多态态都可以被 function 给 map over，了解到这个目的，便抽象化了 Functor 这个 typeclass 出来。但这让我们想问：如果给定一个 a -> b 的函数以及 f a 的型态，我们要如何将函数 map over 这个型态而得到 f b？我们知道要如何 map overMaybe a，[a] 以及 IO a。我们甚至还知道如何用 a -> b map over r -> a，并且会得到r -> b。要回答这个问题，我们只需要看 fmap 的型态就好了：
-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- 我们又看到一些可以针对 Functor 改进的地方，例如 a -> b 也被包在一个 Functor value 里面呢？像是 Just (*3)，我们要如何 apply Just 5 给他？如果我们不要 apply Just 5 而是Nothing 呢？甚至给定 [(*2),(+4)]，我们要如何 apply 他们到 [1,2,3] 呢？对于此，我们抽象出 Applicative typeclass，这就是我们想要问的问题：
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-- 我们也看到我们可以将一个正常的值包在一个数据型态中。例如说我们可以拿一个 1 然后把他包成Just 1。或是把他包成 [1]。也可以是一个 I/O action 会产生一个 1。这样包装的 function 我们叫他做 pure。

-- 一个 applicative value 可以被看作一个有附加 context 的值。例如说，'a' 只是一个普通的字符，但 Just 'a' 是一个附加了 context 的字符。他不是 Char 而是 Maybe Char，这型态告诉我们这个值可能是一个字符，也可能什么都没有。
-- 来看看 Applicative typeclass 怎样让我们用普通的 function 操作他们，同时还保有 context：

(*) <$> Just 2 <*> Just 8  
Just 16  
(++) <$> Just "klingon" <*> Nothing  
Nothing  
(-) <$> [3,4] <*> [1,2,3]  
[2,1,0,3,2,1]
-- 所以我们可以视他们为 applicative values，Maybe a 代表可能会失败的 computation，[a] 代表同时有好多结果的 computation (non-deterministic computation)，而 IO a 代表会有 side-effects 的 computation。

-- Monad 是一个从 Applicative functors 很自然的一个演进结果。对于他们我们主要考量的点是：如果你有一个具有 context 的值 m a，你能如何把他丢进一个只接受普通值 a 的函数中，并回传一个具有 context 的值？也就是说，你如何套用一个型态为 a -> m b 的函数至 m a？基本上，我们要求的函数是：
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

-- 如果我们有一个漂亮的值跟一个函数接受普通的值但回传漂亮的值，那我们要如何要把漂亮的值丢进函数中？这就是我们使用 Monad 时所要考量的事情。我们不写成 f a 而写成 m a 是因为 m 代表的是 Monad，但 monad 不过就是支持 >>= 操作的 applicative functors。>>= 我们称呼他为 bind。

-- Maybe 是一个 Monad
-- 一个 Maybe a 型态的值代表型态为 a 的值而且具备一个可能造成错误的 context。而 Just "dharma" 的值代表他不是一个 "dharma" 的字串就是字串不见时的 Nothing。如果你把字串当作计算的结果，Nothing 就代表计算失败了。
-- 当我们把 Maybe 视作 functor，我们其实要的是一个 fmap 来把一个函数针对其中的元素做套用。他会对 Just 中的元素进行套用，要不然就是保留 Nothing 的状态，其代表里面根本没有元素。
fmap (++"!") (Just "wisdom")  
Just "wisdom!"  
fmap (++"!") Nothing  
Nothing

-- Maybe作为一个 applicative functor，我们能用 <*> 来套用一个存在 Maybe 中的函数至包在另外一个Maybe 中的值。他们都必须是包在 Just 来代表值存在，要不然其实就是 Nothing。当你在想套用函数到值上面的时候，缺少了函数或是值都会造成错误，所以这样做是很合理的。
Just (+3) <*> Just 3  
Just 6  
Nothing <*> Just "greed"  
-- Nothing  
-- Just ord <*> Nothing  
-- Nothing

-- 当我们用 applicative 的方式套用函数至 Maybe 型态的值时，就跟上面描述的差不多。过程中所有值都必须是 Just，要不然结果一定会是 Nothing。
max <$> Just 3 <*> Just 6  
Just 6  
max <$> Just 3 <*> Nothing  
Nothing

-- 我们来思考一下要怎么为 Maybe 实作 >>=。正如我们之前提到的，>>= 接受一个 monadic value，以及一个接受普通值的函数，这函数会回传一个 monadic value。>>= 会帮我们套用这个函数到这个 monadic value。在函数只接受普通值的情况侠，函数是如何作到这件事的呢？要作到这件事，他必须要考虑到 monadic value 的 context。
-- 在这个案例中，>>= 会接受一个 Maybe a 以及一个型态为 a -> Maybe b 的函数。他会套用函数到 Maybe a。要厘清他怎么作到的，首先我们注意到 Maybe 的 applicative functor 特性。假设我们有一个函数 \x -> Just (x+1)。他接受一个数字，把他加 1 后再包回 Just。
(\x -> Just (x+1)) 1  
Just 2  
(\x -> Just (x+1)) 100  
Just 101

-- 我们调用 applyMaybe 而不调用 >>=。他接受 Maybe a 跟一个回传 Maybe b 的函数，并套用函数至 Maybe a。
:{
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x
    
:}

-- 我们套用一个 infix 函数，这样 Maybe 的值可以写在左边且函数是在右边：
Just 3 `applyMaybe` \x -> Just (x+1)  
-- Just 4  
Just "smile" `applyMaybe` \x -> Just (x ++ " :")
-- Just "smile :"
Nothing `applyMaybe` \x -> Just (x+1)  
Nothing  
Nothing `applyMaybe` \x -> Just (x ++ " :")
Nothing
-- 在上述的范例中，我们看到在套用 applyMaybe 的时候，函数是套用在 Just 里面的值。当我们试图套用到 Nothing，那整个结果便是 Nothing。假如函数回传 Nothing 呢？
Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Just 3  
Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Nothing
-- 这正是我们期待的结果。如果左边的 monadic value 是 Nothing，那整个结果就是 Nothing。如果右边的函数是 Nothing，那结果也会是 Nothing。这跟我们之前把 Maybe 当作 applicative 时，过程中有任何一个 Nothing 整个结果就会是 Nothing 一样。
-- 对于 Maybe 而言，我们已经找到一个方法处理漂亮值的方式。我们作到这件事的同时，也保留了Maybe 代表可能造成错误的计算的意义。
-- 你可能会问，这样的结果有用吗？由于 applicative functors 让我们可以拿一个接受普通值的函数，并让他可以操作具有 context 的值，这样看起来 applicative functors 好像比 monad 强。但我们会看到 monad 也能作到，因为他只是 applicative functors 的升级版。他们同时也能作到 applicative functors 不能作到的事情。

-- 感觉一下Maybe是怎样表现成Monad的。
-- return x = Just x  
return "WHAT" :: Maybe String  
Just "WHAT"  

-- >>=跟我们的applyMaybe是一样的。当我们将Maybe a塞给我们的函数，我们保留住context，并且在输入是Nothing的时候回传Nothing。毕竟当没有值的时候套用我们的函数是没有意义的。当输入是Just的时候则套用f并将他包在Just里面。
-- Just x >>= f  = f x  
-- 留意我们是如何把 Just 9 喂给 \x -> return (x*10)。在函数中 x 绑定到 9。他看起好像我们能不用 pattern matching 的方式就从 Maybe 中抽取出值。但我们并没有丧失掉 Maybe 的 context，当他是 Nothing 的时候，>>= 的结果也会是 Nothing。
Just 9 >>= \x -> return (x*10)  
Just 90  

-- Nothing >>= f = Nothing  
Nothing >>= \x -> return (x*10)  
Nothing

-- start here
-- 我们想看看先来了一只鸟停在左边，然后来了四只停在右边，然后左边那只飞走了。之后会是什么情形。我们用一对整数来代表我们的平衡竿状态。头一个位置代表左边的鸟的数量，第二个位置代表右边的鸟的数量。
type Birds = Int  
type Pole = (Birds,Birds)

-- 由于我们用整数来代表有多少只鸟，我们便先来定义 Int 的同义型态，叫做 Birds。然后我们把(Birds, Birds) 定义成 Pole。
-- 接下来，我们定义一个函数他接受一个数字，然后把他放在竿子的左边，还有另外一个函数放在右边。
:{
landLeft :: Birds -> Pole -> Pole  
landLeft n (left,right) = (left + n,right)  

landRight :: Birds -> Pole -> Pole  
landRight n (left,right) = (left,right + n)
:}

-- 我们来试着执行看看：
landLeft 2 (0,0)  
(2,0)  
landRight 1 (1,2)  
(1,3)  
landRight (-1) (1,2)  
(1,1)
-- 要仿真鸟飞走的话我们只要给定一个负数就好了。 由于这些操作是接受 Pole 并回传 Pole， 所以我们可以把函数串在一起。
landLeft 2 (landRight 1 (landLeft 1 (0,0)))  
(3,1)
-- 当我们喂 (0,0) 给 landLeft 1 时，我们会得到 (1,0)。接着我们仿真右边又停了一只鸟，状态就变成 (1,1)。最后又有两只鸟停在左边，状态变成 (3,1)。我们这边的写法是先写函数名称，然后再套用参数。但如果先写 pole 再写函数名称会比较清楚，所以我们会想定义一个函数
x -: f = f x
-- 我们能先套用参数然后再写函数名称：
100 -: (*3)  
300  
True -: not  
False  
(0,0) -: landLeft 2  
(2,0)
-- 有了这个函数，我们便能写得比较好读一些：
(0,0) -: landLeft 1 -: landRight 1 -: landLeft 2  
(3,1)
-- 这个范例跟先前的范例是等价的，只不过好读许多。很清楚的看出我们是从 (0,0) 开始，然后停了一只在左边，接着右边又有一只，最后左边多了两只。
-- 到目前为止没什么问题，但如果我们要停 10 只在左边呢？
landLeft 10 (0,3)  
(10,3)
-- 你说左边有 10 只右边却只有 3 只？那不是早就应该掉下去了？这个例子太明显了，如果换个比较不明显的例子。
(0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2)
-- 表面看起来没什么问题，但如果你仔细看的话，有一瞬间是右边有四只，但左边没有鸟。要修正这个错误，我们要重新查看 landLeft 跟 landRight。我们其实是希望这些函数产生失败的情况。那就是在维持平衡的时候回传新的 pole，但失败的时候告诉我们失败了。这时候 Maybe 就刚刚好是我们要的 context 了。我们用 Maybe 重新写一次：
:{
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  

landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing
:}    
-- 现在这些函数不回传 Pole 而回传 Maybe Pole 了。他们仍接受鸟的数量跟旧的的 pole，但他们现在会检查是否有太多鸟会造成皮尔斯失去平衡。我们用 guards 来检查是否有差异超过三的情况。如果没有，那就包一个在 Just 中的新的 pole，如果是，那就回传 Nothing。
-- 再来执行看看：
landLeft 2 (0,0)  
Just (2,0)  
landLeft 10 (0,3)  
Nothing
-- 一如预期，当皮尔斯不会掉下去的时候，我们就得到一个包在 Just 中的新 pole。当太多鸟停在同一边的时候，我们就会拿到 Nothing。这样很棒，但我们却不知道怎么把东西串在一起了。我们不能做 landLeft 1 (landRight 1 (0,0))，因为当我们对 (0,0) 使用 landRight 1 时，我们不是拿到 Pole 而是拿到 Maybe Pole。landLeft 1 会拿到 Pole 而不是拿到 Maybe Pole。
-- 我们需要一种方法可以把拿到的 Maybe Pole 塞到拿 Pole 的函数中，然后回传 Maybe Pole。而我们有 >>=，他对 Maybe 做的事就是我们要的
landRight 1 (0,0) >>= landLeft 2  
Just (2,1)
-- landLeft 2 的型态是 Pole -> Maybe Pole。我们不能喂给他 Maybe Pole 的东西。而landRight 1 (0,0) 的结果就是 Maybe Pole，所以我们用 >>= 来接受一个有 context 的值然后拿给 landLeft 2。>>= 的确让我们把 Maybe 当作有 context 的值，因为当我们丢 Nothing 给landLeft 2 的时候，结果会是 Nothing。
Nothing >>= landLeft 2  
Nothing
-- 这样我们可以把这些新写的用 >>= 串在一起。让 monadic value 可以喂进只吃普通值的函数。
-- 来看看些例子：
return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
Just (2,4)
-- 我们最开始用 return 回传一个 pole 并把他包在 Just 里面。我们可以像往常套用 landRight 2，不过我们不那么做，我们改用 >>=。Just (0,0) 被喂到 landRight 2，得到 Just (0,2)。接着被喂到 landLeft 2，得到 Just (2,2)。
-- 还记得我们之前引入失败情况的例子吗？
(0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2)
-- 之前的例子并不会反应失败的情况。但如果我们用 >>= 的话就可以得到失败的结果。
return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  
Nothing


-- 正如预期的，最后的情形代表了失败的情况。我们再进一步看看这是怎么产生的。首先 return 把(0,0) 放到一个最小的 context 中，得到 Just (0,0)。然后是 Just (0.0) >>= landLeft 1。由于 Just (0,0) 是一个 Just 的值。landLeft 1 被套用至 (0,0) 而得到 Just (1,0)。这反应了我们仍保持在平衡的状态。接着是 Just (1,0) >>= landright 4 而得到了 Just (1,4)。距离不平衡只有一步之遥了。他又被喂给 landLeft (-1)，这组合成了 landLeft (-1) (1,4)。由于失去了平衡，我们变得到了 Nothing。而我们把 Nothing 喂给 landRight (-2)，由于他是Nothing，也就自动得到了 Nothing。
-- 如果只把 Maybe 当作 applicative 用的话是没有办法达到我们要的效果的。你试着做一遍就会卡住。因为 applicative functor 并不允许 applicative value 之间有弹性的交互。他们最多就是让我们可以用 applicative style 来传递参数给函数。applicative operators 能拿到他们的结果并把他用 applicative 的方式喂给另一个函数，并把最终的 applicative 值放在一起。但在每一步之间并没有太多允许我们作手脚的机会。而我们的范例需要每一步都倚赖前一步的结果。当每一只鸟降落的时候，我们都会把前一步的结果拿出来看看。好知道结果到底应该成功或失败。

-- 我们也能写出一个函数，完全不管现在究竟有几只鸟停在竿子上，只是要害皮尔斯滑倒。我们可以称呼这个函数叫做 banana：
:{
banana :: Pole -> Maybe Pole  
banana _ = Nothing
:}

-- 现在我们能把香蕉皮串到我们的过程中。他绝对会让遇到的人滑倒。他完全不管前面的状态是什么都会产生失败。
return (0,0) >>= landLeft 1 >>= banana >>= landRight 1  
Nothing
-- Just (1,0) 被喂给 banana，而产生了 Nothing，之后所有的结果便都是 Nothing 了。
-- 要同样表示这种忽略前面的结果，只注重眼前的 monadic value 的情况，其实我们可以用 >> 来表达。
:{
(>>) :: (Monad m) => m a -> m b -> m b  
m >> n = m >>= \_ -> n
:}
-- 一般来讲，碰到一个完全忽略前面状态的函数，他就应该只会回传他想回传的值而已。但碰到 Monad，他们的 context 还是必须要被考虑到。来看一下 >> 串接 Maybe 的情况。
Nothing >> Just 3  
Nothing  
Just 3 >> Just 4  
Just 4  
Just 3 >> Nothing  
Nothing
-- 如果你把 >> 换成 >>= \_ ->，那就很容易看出他的意思。
-- 我们也可以把 banana 改用 >> 跟 Nothing 来表达：
return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1  
Nothing
-- 我们得到了保证的失败。
-- 我们也可以看看假如我们故意不用把 Maybe 视为有 context 的值的写法。他会长得像这样：
:{
routine :: Maybe Pole  
routine = case landLeft 1 (0,0) of  
    Nothing -> Nothing  
    Just pole1 -> case landRight 4 pole1 of   
            Nothing -> Nothing  
            Just pole2 -> case landLeft 2 pole2 of  
                    Nothing -> Nothing  
                    Just pole3 -> landLeft 1 pole3

:}

-- 把普通的函数套用换成了 >>=，让我们可以轻松地应付可能会失败的情况，并帮我们传递 context。这边的 context 就代表失败的可能性，当我们套用函数到 context 的时候，就代表考虑进了失败的情况。


-- do 表示法
-- Monad 在 Haskell 中是十分重要的，所以我们还特别为了操作他设置了特别的语法：do 表示法。我们在介绍 I/O 的时候已经用过 do 来把小的 I/O action 串在一起了。其实 do 并不只是可以用在IO，他可以用在任何 monad 上。他的原则是简单明了，把 monadic value 串成一串。我们这边来细看 do 是如何使用，以及为什么我们十分倚赖他。


Just 3 >>= (\x -> Just (show x ++ "!"))  
Just "3!"
-- 你说这没什么了不起，不过就是把 monadic value 喂给一个函数罢了。其中 x 就指定成 3。也从 monadic value 变成了普通值。那如果我们要在 lambda 中使用 >>= 呢？
Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Just "3!"
-- 我们嵌一个 >>= 在另外一个 >>= 中。在外层的 lambda，我们把 Just "!" 喂给 \y -> Just (show x ++ y)。在内层的 lambda，y 被指定成 "!"。x 仍被指定成 3，是因为我们是从外层的 lambda 取值的。这些行为让我们回想到下列式子：
let x = 3; y = "!" in show x ++ y  
"3!"
-- 差别在于前述的值是 monadic，具有失败可能性的 context。我们可以把其中任何一步代换成失败的状态：
Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Nothing  
Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))  
Nothing  
Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))  
Nothing
-- 第一行中，把 Nothing 喂给一个函数，很自然地会回传 Nothing。第二行里，我们把 Just 3 喂给一个函数，所以 x 就成了 3。但我们把 Nothing 喂给内层的 lambda 所有的结果就成了Nothing，这也进一步使得外层的 lambda 成了 Nothing。这就好比我们在 let expression 中来把值指定给变量一般。只差在我们这边的值是 monadic value。
-- 要再说得更清楚点，我们来把 script 改写成每行都处理一个 Maybe：
:{
foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))
:}
-- 为了摆脱这些烦人的 lambda，Haskell 允许我们使用 do 表示法。他让我们可以把先前的程序写成这样：
:{
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)
:}

-- 这看起来好像让我们不用在每一步都去检查 Maybe 的值究竟是 Just 或 Nothing。这蛮方便的，如果在任何一个步骤我们取出了 Nothing。那整个 do 的结果就会是 Nothing。我们把整个责任都交给 >>=，他会帮我们处理所有 context 的问题。这边的 do 表示法不过是另外一种语法的形式来串连所有的 monadic value 罢了。
-- 在 do expression 中，每一行都是一个 monadic value。要检查处理的结果的话，就要使用 <-。如果我们拿到一个 Maybe String，并用 <- 来绑定给一个变量，那个变量就会是一个 String，就像是使用 >>= 来将 monadic value 带给 lambda 一样。至于 do expression 中的最后一个值，好比说 Just (show x ++ y)，就不能用 <- 来绑定结果，因为那样的写法当转换成 >>= 的结果时并不合理。他必须要是所有 monadic value 黏起来后的总结果，要考虑到前面所有可能失败的情形。

Just 9 >>= (\x -> Just (x > 8))  
Just True
-- 由于 >>= 左边的参数是一个 Just 型态的值，当 lambda 被套用至 9 就会得到 Just True。如果我们重写整个式子，改用 do 表示法：我们会得到：
:{
marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8)
:}
-- 如果我们比较这两种写法，就很容易看出为什么整个 monadic value 的结果会是在 do 表示法中最后一个 monadic value 的值。他串连了全面所有的结果。

-- 我们走钢索的仿真程序也可以改用 do 表示法重写。landLeft 跟 landRight 接受一个鸟的数字跟一个竿子来产生一个包在 Just 中新的竿子。而在失败的情况会产生 Nothing。我们使用 >>=来串连所有的步骤，每一步都倚赖前一步的结果，而且都带有可能失败的 context。这边有一个范例，先是有两只鸟停在左边，接着有两只鸟停在右边，然后是一只鸟停在左边：
:{
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    second <- landRight 2 first  
    landLeft 1 second
:}
-- 我们来看看成功的结果：
routine  
Just (3,2)
-- 当我们要把这些 routine 用具体写出的 >>=，我们会这样写：return (0,0) >>= landLeft 2，而有了 do 表示法，每一行都必须是一个 monadic value。所以我们清楚地把前一个 Pole 传给landLeft 跟 landRight。如果我们查看我们绑定 Maybe 的变量，start 就是 (0,0)，而first 就会是 (2,0)。

-- 由于 do 表示法是一行一行写，他们会看起来很像是命令式的写法。但实际上他们只是代表串行而已，每一步的值都倚赖前一步的结果，并带着他们的 context 继续下去。
-- 我们再重新来看看如果我们没有善用 Maybe 的 monad 性质的程序：
:{
routine :: Maybe Pole  
    routine =   
        case Just (0,0) of   
            Nothing -> Nothing  
            Just start -> case landLeft 2 start of  
                Nothing -> Nothing  
                Just first -> case landRight 2 first of  
                    Nothing -> Nothing  
                    Just second -> landLeft 1 second
:}

-- 在成功的情形下，Just (0,0) 变成了 start， 而 landLeft 2 start 的结果成了 first。
-- 如果我们想在 do 表示法里面对皮尔斯丢出香蕉皮，我们可以这样做：
:{
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    landLeft 1 second
:}
-- 当我们在 do 表示法写了一行运算，但没有用到 <- 来绑定值的话，其实实际上就是用了 >>，他会忽略掉计算的结果。我们只是要让他们有序，而不是要他们的结果，而且他比写成 _ <- Nothing要来得漂亮的多。
-- 你会问究竟我们何时要使用 do 表示法或是 >>=，这完全取决于你的习惯。在这个例子由于有每一步都倚赖于前一步结果的特性，所以我们使用 >>=。如果用 do 表示法，我们就必须清楚写出鸟究竟是停在哪根竿子上，但其实每一次都是前一次的结果。不过他还是让我们了解到怎么使用 do。
-- 在 do 表示法中，我们其实可以用模式匹配来绑定 monadic value，就好像我们在 let 表达式，跟函数参数中使用模式匹配一样。这边来看一个在 do 表示法中使用模式匹配的范例：
:{
justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x
:}

-- 我们用模式匹配来取得 "hello" 的第一个字符，然后回传结果。所以 justH 计算会得到 Just 'h'。
-- 如果模式匹配失败怎么办？当定义一个函数的时候，一个模式不匹配就会跳到下一个模式。如果所有都不匹配，那就会造成错误，整个程序就当掉。另一方面，如果在 let 中进行模式匹配失败会直接造成错误。毕竟在 let 表达式的情况下并没有失败就跳下一个的设计。至于在 do 表示法中模式匹配失败的话，那就会调用 fail 函数。他定义在 Monad 的 type class 定义猪。他允许在现在的 monad context 底下，失败只会造成失败而不会让整个程序当掉。他缺省的实作如下：
-- fail :: (Monad m) => String -> m a  
-- fail msg = error msg

-- 可见缺省的实作的确是让程序挂掉，但在某些考虑到失败的可能性的 Monad（像是 Maybe）常常会有他们自己的实作。对于 Maybe，他的实作像是这样：
-- fail _ = Nothing

-- 他忽略错误消息，并直接回传 Nothing。所以当在 do 表示法中的 Maybe 模式匹配失败的时候，整个结果就会是 Nothing。这种方式比起让程序挂掉要好多了。这边来看一下 Maybe 模式匹配失败的范例：
:{
wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x
:}
-- 模式匹配的失败，所以那一行的效果相当于一个 Nothing。我们来看看执行结果：
wopwop  
Nothing
-- 这样模式匹配的失败只会限制在我们 monad 的 context 中，而不是整个程序的失败。这种处理方式要好多了。












