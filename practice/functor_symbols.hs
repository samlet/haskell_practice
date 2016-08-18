-- functor_symbols.hs

-- 🐨 <*>
-- Control.Applicative 中，在其中定义了两个函数 pure 跟 <*>。他并没有提供缺省的实作，如果我们想使用他必须要为他们 applicative functor 的实作。typeclass 定义如下：
-- class (Functor f) => Applicative f where  
--     pure :: a -> f a  
--     (<*>) :: f (a -> b) -> f a -> f b
-- <*> 也非常有趣。他的型别是 f (a -> b) -> f a -> f b。这有让你联想到什么吗？没错！就是fmap :: (a -> b) -> f a -> f b。他有点像加强版的 fmap。然而 fmap 接受一个函数跟一个 functor，然后套用 functor 之中的函数。<*> 则是接受一个装有函数的 functor 跟另一个 functor，然后取出第一个 functor 中的函数将他对第二个 functor 中的值做 map。

-- 我们来看看 Maybe 的 Applicative 实作：
-- instance Applicative Maybe where  
--     pure = Just  
--     Nothing <*> _ = Nothing  
--     (Just f) <*> something = fmap f something


-- 对于 Maybe 而言，如果左边是 Just，那 <*> 会从其中抽出了一个函数来 map 右边的值。如果有任何一个参数是 Nothing。那结果便是 Nothing。
-- 来试试看吧！
Just (+3) <*> Just 9  
Just 12  
pure (+3) <*> Just 10  
Just 13  
pure (+3) <*> Just 9  
Just 12  
Just (++"hahah") <*> Nothing  
Nothing  
Nothing <*> Just "woot"  
Nothing

-- 🐨 <$>
-- Control.Applicative 会 export 一个函数 <$>，他基本上就是中缀版的 fmap。他是这么被定义的：
-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
-- f <$> x = fmap f x

-- 要记住型别变量跟参数的名字还有值绑定的名称不冲突。``f`` 在函数的型别宣告中是型别变量，说明 ``f`` 应该要满足 ``Functor`` typeclass 的条件。而在函数本体中的 ``f`` 则表示一个函数，我们将他 map over x。我们同样用 ``f`` 来表示他们并代表他们是相同的东西。
-- <$> 的使用显示了 applicative style 的好处。如果我们想要将 f 套用三个 applicative functor。我们可以写成 f <$> x <*> y <*> z。如果参数不是 applicative functor 而是普通值的话。我们则写成 f x y z。
-- 我们再仔细看看他是如何运作的。我们有一个 Just "johntra" 跟 Just "volta" 这样的值，我们希望将他们结合成一个 String，并且包含在 Maybe 中。我们会这样做：
(++) <$> Just "johntra" <*> Just "volta"  
Just "johntravolta"
-- 可以将上面的跟下面这行比较一下：
(++) "johntra" "volta"  
"johntravolta"
-- 可以将一个普通的函数套用在 applicative functor 上真不错。只要稍微写一些 <$> 跟 <*> 就可以把函数变成 applicative style，可以操作 applicatives 并回传 applicatives。
-- 总之当我们在做 (++) <$> Just "johntra" <*> Just "volta" 时，首先我们将 (++) map over 到 Just "johntra"，然后产生 Just ("johntra"++)，其中 (++) 的型别为 (++) :: [a] -> [a] -> [a]，Just ("johntra"++) 的型别为 Maybe ([Char] -> [Char])。注意到 (++) 是如何吃掉第一个参数，以及我们是怎么决定 a 是 Char 的。当我们做 Just ("johntra"++) <*> Just "volta"，他接受一个包在 Just 中的函数，然后 map over Just "volta"，产生了 Just "johntravolta"。如果两个值中有任意一个为 Nothing，那整个结果就会是 Nothing。





