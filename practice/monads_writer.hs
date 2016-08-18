-- monads_writer.hs

-- 相对于 Maybe 是加入可能失败的 context，list 是加入 non-deterministic 的 context，Writer 则是加进一个附加值的 context，好比 log 一般。Writer 可以让我们在计算的同时搜集所有 log 纪录，并汇集成一个 log 并附加在结果上。
例如我们想要附加一个 String 好说明我们的值在干么（有可能是为了除错）。想像有一个函数接-- 受一个代表帮派人数的数字，然后会回传值告诉我们这是否算是一个庞大的帮派：
:{
isBigGang :: Int -> Bool  
isBigGang x = x > 9
:}
-- 现在我们希望他不只是回传 True 或 False，我们还希望他能够多回传一个字串代表 log。这很容易，只要多加一个 String 在 Bool 旁边就好了。
:{
isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")
:}

-- 我们现在回传了一个 Tuple，第一个元素是原来的布林值，第二个元素是一个 String。现在我们的值有了一个 context。
isBigGang 3  
(False,"Compared gang size to 9.")  
isBigGang 30  
(True,"Compared gang size to 9.")

-- 我们再来写一个接受附加 log 值的函数，也就是 (a, String) 型态的值跟 a -> (b, String) 型态的函数。我们称呼这个函数为 applyLog。这个函数有的 context 是附加 log 值，而不是一个可能会失败的 context，因此 applyLog 会确保原有的 log 被保留，并附上从函数产生出的新的 log。这边我们来看一下实作：
:{
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)
:}

-- 当我们想把一个具有 context 的值喂给一个函数的时候，我们会尝试把值跟他的 context 分开，然后把值喂给函数再重新接回 context。在 Maybe monad 的情况，我们检查值是否为 Just x，如果是，便将 x 喂给函数。而在 log 的情况，我们知道 pair 的其中一个 component 是 log 而另一个是值。所以我们先取出值 x，将 f apply 到 x，便获取 (y,newLog)，其中 y 是新的值而newLog 则是新的 log。但如果我们回传 newLog，旧的 log 便不会包含进去，所以我们要回传的是(y, log ++ newLog)。我们用 ++ 来把新的 log 接到旧的上面。
-- 来看看 applyLog 运作的情形：
(3, "Smallish gang.") `applyLog` isBigGang  
(False,"Smallish gang.Compared gang size to 9")  
(30, "A freaking platoon.") `applyLog` isBigGang  
(True,"A freaking platoon.Compared gang size to 9")
-- 跟之前的结果很像，只差在我们多了伴随产生的 log。再来多看几个例子：
("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))  
(5,"Got outlaw name.Applied length.")  
("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))  
(7,"Got outlaw name.Applied length")
-- 可以看到在 lambda 里面 x 只是个正常的字串而不是 tuple，且 applyLog 帮我们处理掉附加 log 的动作。





