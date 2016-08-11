-- typeclasses_ops.hs
:l typeclasses
-- 我们用模式匹配来完成我们的任务。我们来看看他是如何运作的。
Red == Red
True
Red == Yellow
False
Red `elem` [Red, Yellow, Green]
True
[Red, Yellow, Green]
-- [Red light,Yellow light,Green light]

-- 现在我们定义了许多 instance，来试着跑跑看！
:l typeclasses

yesno $ length []
False
yesno "haha"
True
yesno ""
False
yesno $ Just 0
True
yesno True
True
yesno EmptyTree
False
yesno []
False
yesno [0,0,0]
True
:t yesno
-- yesno :: (YesNo a) => a -> Bool

-- 接受一个 yes or no 的值还有两个部份，如果值是代表 "yes"，那第一个部份就会被执行，而如果值是 "no"，那第二个部份就会执行。
:l typeclasses
yesnoIf [] "YEAH!" "NO!"
"NO!"
yesnoIf [2,3,4] "YEAH!" "NO!"
"YEAH!"
yesnoIf True "YEAH!" "NO!"
"YEAH!"
yesnoIf (Just 500) "YEAH!" "NO!"
"YEAH!"
yesnoIf Nothing "YEAH!" "NO!"
"NO!"


