-- toplevel
-- 使用 Range
[1..20]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
['K'..'Z']  
"KLMNOPQRSTUVWXYZ"

-- Range 的特点是他还允许你指定每一步该跨多远。譬如说，今天的问题换成是要得到 1 到 20 间所有的偶数或者 3 的倍数该怎样？
-- 仅需用逗号将前两个元素隔开，再标上上限即可。
-- 在 Range 中使用浮点数要格外小心！出于定义的原因，浮点数并不精确。
[2,4..20]
[2,4,6,8,10,12,14,16,18,20]
[3,6..20]
[3,6,9,12,15,18]

-- 你也可以不标明 Range 的上限，从而得到一个无限长度的 List。在后面我们会讲解关于无限 List 的更多细节。取前 24 个 13 的倍数该怎样？恩，你完全可以 [13,26..24*13]，但有更好的方法： 
take 24 [13,26..]。
-- 由于 Haskell 是惰性的，它不会对无限长度的 List 求值，否则会没完没了的。它会等着，看你会从它那儿取多少。在这里它见你只要 24 个元素，便欣然交差。如下是几个生成无限 List 的函数 cycle 接受一个 List 做参数并返回一个无限 List 。如果你只是想看一下它的运算结果而已，它会运行个没完的。所以应该在某处划好范围。
take 10 (cycle [1,2,3])
[1,2,3,1,2,3,1,2,3,1]
take 12 (cycle "LOL ")
"LOL LOL LOL "
-- repeat 接受一个值作参数，并返回一个仅包含该值的无限 List。这与用 cycle 处理单元素 List 差不多。
take 10 (repeat 5)
[5,5,5,5,5,5,5,5,5,5]
-- 其实，你若只是想得到包含相同元素的 List ，使用 replicate 会更简单，如 replicate 3 10，得[10,10,10]。


-- 在 Haskell 下，我们可以通过类似 take 10 [2,4..] 的代码来实现。但若是把简单的乘 2 改成更复杂的函数操作该怎么办呢？用 list comprehension，它与 set comprehension 十分的相似，用它取前十个偶数轻而易举。这个 list comprehension 可以表示为：
[x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
-- 如你所见，结果正确。给这个 comprehension 再添个限制条件 (predicate)，它与前面的条件由一个逗号分隔。在这里，我们要求只取乘以 2 后大于等于 12 的元素。
[x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]
-- cool，灵了。若是取 50 到 100 间所有除7的余数为 3 的元素该怎么办？简单：
[ x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]

-- 假如我们想要一个 comprehension，它能够使 List 中所有大于 10 的奇数变为 "BANG"，小于 10 的奇数变为 "BOOM"，其他则统统扔掉。方便重用起见，我们将这个 comprehension 置于一个函数之中。
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- 这个 comprehension 的最后部分就是限制条件，使用 odd 函数判断是否为奇数：返回 True，就是奇数，该 List 中的元素才被包含。
boomBangs [7..13]
["BOOM!","BOOM!","BANG!","BANG!"]
-- 也可以加多个限制条件。若要达到 10 到 20 间所有不等于 13，15 或 19 的数，可以这样：
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]
-- 除了多个限制条件之外，从多个 List 中取元素也是可以的。这样的话 comprehension 会把所有的元素组合交付给我们的输出函数。在不过滤的前提 下，取自两个长度为 4 的集合的 comprehension 会产生一个长度为 16 的 List。假设有两个 List，[2,5,10] 和 [8,10,11]， 要取它们所有组合的积，可以这样：
[ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
-- 意料之中，得到的新 List 长度为 9。若只取乘积大于 50 的结果该如何？
[ x*y | x <-[2,5,10], y <- [8,10,11], x*y > 50]
[55,80,100,110]
-- 取个包含一组名词和形容词的 List comprehension 吧，写诗的话也许用得着。
let nouns = ["hobo","frog","pope"]
let adjectives = ["lazy","grouchy","scheming"]
[adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog", "grouchy pope","scheming hobo",
"scheming frog","scheming pope"]
-- 明白！让我们编写自己的 length 函数吧！就叫做 length'!
length' xs = sum [1 | _ <- xs]
-- _ 表示我们并不关心从 List 中取什么值，与其弄个永远不用的变量，不如直接一个 _。这个函数将一个 List 中所有元素置换为 1，并且使其相加求和。得到的结果便是我们的 List 长度。友情提示：字串也是 List，完全可以使用 list comprehension 来处理字串。如下是个除去字串中所有非大写字母的函数：
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
-- 测试一下：
removeNonUppercase "Hahaha! Ahahaha!"
"HA"
removeNonUppercase "IdontLIKEFROGS"
"ILIKEFROGS"
-- 在这里，限制条件做了所有的工作。它说：只有在 ['A'..'Z'] 之间的字符才可以被包含。
-- 若操作含有 List 的 List，使用嵌套的 List comprehension 也是可以的。假设有个包含许多数值的 List 的 List，让我们在不拆开它的前提下除去其中的所有奇数：
let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
[ [ x | x <- xs, even x ] | xs <- xxs]
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
-- 将 List Comprehension 分成多行也是可以的。若非在 ghci 之下，还是将 List Comprehension 分成多行好，尤其是需要嵌套的时候。


-- 接下来考虑一个同时应用到 List 和 Tuple 的问题：如何取得所有三边长度皆为整数且小于等于 10，周长为 24 的直角三角形？首先，把所有三遍长度小于等于 10 的三角形都列出来：
let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
-- 刚才我们是从三个 List 中取值，并且通过输出函数将其组合为一个三元组。只要在 ghci 下边调用 triangle，你就会得到所有三边都小于等于 10 的三角形。我们接下来给它添加一个限制条件，令其必须为直角三角形。同时也考虑上 b 边要短于斜边，a 边要短于 b 边情况：
let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
-- 已经差不多了。最后修改函数，告诉它只要周长为 24 的三角形。
let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
rightTriangles'
[(6,8,10)]


-- List Comprehension 中也能用模式匹配：一旦模式匹配失败，它就简单挪到下个元素。
let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
[a+b | (a,b) <- xs]  
[4,7,6,8,11,4]








