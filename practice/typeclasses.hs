-- typeclasses.hs

module Main where

main = putStrLn "Hello World"

-- 什么是 typeclass: typeclass 就像是 interface。一个 typeclass 定义了一些行为(像是比较相不相等，比较大小顺序，能否穷举)而我们会把希望满足这些性质的型别定义成这些 typeclass 的 instance。typeclass 的行为是由定义的函数来描述。并写出对应的实作。当我们把一个型别定义成某个 typeclass 的 instance，就表示我们可以对那个型别使用 typeclass 中定义的函数。

-- 例如说，Eq 这个 typeclass 是描述可以比较相等的事物。他定义了 == 跟 /= 两个函数。如果我们有一个型别 Car，而且对他们做相等比较是有意义的，那把 Car 作成是 Eq 的一个 instance 是非常合理的。

-- Eq的类定义: class Eq a where ....
-- 当我们有了 class 以后，可以用来做些什么呢？说实话，不多。不过一旦我们为它写一些 instance，就会有些好功能。来看看下面这个型别：
data TrafficLight = Red | Yellow | Green

-- 这里定义了红绿灯的状态。请注意这个型别并不是任何 class 的 instance，虽然可以透过 derive 让它成为Eq 或 Show 的 instance，但我们打算手工打造。下面展示了如何让一个型别成为 Eq 的 instance：
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- 我们使用了 instance 这个关键字。class 是用来定义新的 typeclass，而 instance 是用来说明我们要定义某个 typeclass 的 instance。当我们要定义 Eq，我们会写 class Eq a where，其中 a 代表任何型态。我们可以从 instance 的写法：instance Eq TrafficLight where 看出来。我们会把 a 换成实际的型别。
-- 由于 == 是用 /= 来定义的，同样的 /= 也是用 == 来定义。所以我们只需要在 instance 定义中复写其中一个就好了。我们这样叫做定义了一个 minimal complete definition。这是说能让型别符合 class 行为所最小需要实作的函数数量。而 Eq 的 minimal complete definition 需要 == 或 /= 其中一个。

-- 再来写 Show 的 instance。要满足 Show 的 minimal complete definition，我们必须实作 show 函数，他接受一个值并把他转成字串。
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- 你也可以把 typeclass 定义成其他 typeclass 的 subclass。像是 Num 的 class 宣告就有点冗长，但我们先看个雏型。
-- class (Eq a) => Num a where
--   ...
-- 如果你想看看一个 typeclass 有定义哪些 instance。可以在 ghci 中输入 :info YourTypeClass。

-- yes-no typeclass

-- 尽管使用 Bool 来表达布林的语意是比较好的作法。为了有趣起见，我们来试试看模仿 Javascript 的行为。我们先从 typeclass 宣告开始看：
class YesNo a where
    yesno :: a -> Bool


-- 接下来我们来定义一些 instance。对于数字，我们会假设任何非零的数字都会被当作 true，而 0 则当作false。
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
-- 空的 List (包含字串)代表 false，而非空的 List 则代表 true。
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
-- 留意到我们加了一个型别参数 a 来让整个 List 是一个具体型别，不过我们并没有对包涵在 List 中的元素的型别做任何额外假设。我们还剩下 Bool 可以被作为真假值，要定义他们也很容易：
instance YesNo Bool where
    yesno = id
-- 你说 id 是什么？他不过是标准函式库中的一个函数，他接受一个参数并回传相同的东西。
-- 我们也让 Maybe a 成为 YesNo 的 instance。
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

-- 由于我们不必对 Maybe 的内容做任何假设，因此并不需要 class constraint。我们只要定义遇到 Just 包装过的值就代表 true，而 Nothing 则代表 false。这里还是得写出 (Maybe a) 而不是只有 Maybe，毕竟 Maybe -> Bool 的函式并不存在（因为 Maybe 并不是具体型别），而 Maybe a -> Bool 看起来就合理多了。现在有了这个定义，Maybe something 型式的型别都属于 YesNo 了，不论 something 是什么。
-- 之前我们定义了 Tree a，那代表一个二元搜索树。我们可以说一棵空的树是 false，而非空的树则是true。
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

-- 而一个红绿灯可以代表 yes or no 吗？当然可以。如果他是红灯，那你就会停下来，如果他是绿灯，那你就能走。但如果是黄灯呢？只能说我通常会闯黄灯。
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True


-- 我们来写一个函数来模仿 if statement 的行为，但他是运作在 YesNo 的型别上。
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal then yesResult else noResult
-- 很直觉吧！他接受一个 yes or no 的值还有两个部份，如果值是代表 "yes"，那第一个部份就会被执行，而如果值是 "no"，那第二个部份就会执行。
-- ghci> yesnoIf [] "YEAH!" "NO!"


