-- toplevel: monads_knight_quest.hs

-- 这边来看一个可以用 non-determinism 解决的问题。假设你有一个西洋棋盘跟一只西洋棋中的骑士摆在上面。我们希望知道是否这只骑士可以在三步之内移到我们想要的位置。我们只要用一对数值来表示骑士在棋盘上的位置。第一个数值代表棋盘的行，而第二个数值代表棋盘的列。

-- 我们先帮骑士的位置定义一个 type synonym。
type KnightPos = (Int,Int)

-- 假设骑士现在是在 (6,2)。究竟他能不能够在三步内移动到 (6,1) 呢？你可能会先考虑究竟哪一步是最佳的一步。但不如全部一起考虑吧！要好好利用所谓的 non-determinism。所以我们不是只选择一步，而是选择全部。我们先写一个函数回传所有可能的下一步：
:{
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
                ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')
:}

-- 骑士有可能水平或垂直移动一步或二步，但问题是他们必须要同时水平跟垂直移动。(c',r') 走过 list 中的每一个元素，而 guard 会保证产生的结果会停留在棋盘上。如果没有，那就会产生一个空的 list，表示失败的结果，return (c',r') 也就不会被执行。
-- 这个函数也可以不用 list monad 来写，但我们这边只是写好玩的。下面是一个用 filter 实现的版本：
:{
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]
:}

-- 两个函数做的都是相同的事，所以选个你喜欢的吧。
moveKnight (6,2)  
[(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]  
moveKnight (8,1)  
[(6,2),(7,3)]
-- 我们接受一个位置然后产生所有可能的移动方式。所以我们有一个 non-deterministic 的下一个位置。我们用 >>= 来喂给 moveKnight。接下来我们就可以写一个三步内可以达到的所有位置：
:{
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second
:}

-- 如果你传 (6,2)，得到的 list 会很大，因为会有不同种方式来走到同样的一个位置。我们也可以不用do 来写：
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- 第一次 >>= 给我们移动一步的所有结果，第二次 >>= 给我们移动两步的所有结果，第三次则给我们移动三步的所有结果。
-- 用 return 来把一个值放进缺省的 context 然后用 >>= 喂给一个函数其实跟函数调用是同样的，只是用不同的写法而已。 接着我们写一个函数接受两个位置，然后可以测试是否可以在三步内从一个位置移到另一个位置：
:{
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start
:}
-- 我们产生所有三步的可能位置，然后看看其中一个位置是否在里面。所以我们可以看看是否可以在三步内从 (6,2) 走到 (6,1)：
(6,2) `canReachIn3` (6,1)  
True
-- 那从 (6,2) 到 (7,3) 呢？
(6,2) `canReachIn3` (7,3)  
False
-- 答案是不行。你可以修改函数改成当可以走到的时候，他还会告诉你实际的步骤。之后你也可以改成不只限定成三步，可以任意步。


