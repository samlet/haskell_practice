-- toplevel
-- collections

let numbers = [1,2,3,4]
let truths  = [True, False, False]
let strings = ["here", "are", "some", "strings"]

-- 列表元素必须是同一类型。因此,  [42, "life, universe and everything else"] 不是一个正确的列表, 因为它包含了两种不同类型的元素, 也就分别是整型和字符串型。而 [12, 80] 或者,  ["beer", "sandwiches"] 都是合法的列表, 因为他们都是单一类型的。

-- 附加(Cons)

1:0:numbers
-- [1,0,1,2,3,4]
2:1:0:numbers
-- [2,1,0,1,2,3,4]
5:4:3:2:1:0:numbers
-- [5,4,3,2,1,0,1,2,3,4]

-- 列表可以包含列表

let listOfLists = [[1,2],[3,4],[5,6]] 
listOfLists
-- [[1,2],[3,4],[5,6]]

-- 一些元组

(True, 1)
("Hello world", False)
(4, 5, "Six", True, 'b')

-- fst和snd, 它们分别“投影”出一个对中的第一和第二个元素（用数学语言来说, 从结构体中取出数据的函数叫做“投影”（Projection））。让我们来看一些例子：

fst (2, 5)	-- 2
fst (True, "boo") -- True
snd (5, "Hello") -- "Hello"

let first (x, y) = x
first (3, True) 

-- 我们可以像在列表中储存列表一样来操作元组。元组也是数据, 所以你可以在元组中储存元组（嵌套在元组中直到任意复杂的级别）。同样, 你也可以创建元组组成的列表, 列表组成的元组, 以下例子的每一行分别表达了一中不同的组合方法。
((2,3), True)
((2,3), [2,3])
[(1,2), (3,4), (5,6)]

-- 将两个 List 合并是很常见的操作, 这可以通过 ++ 运算子实现。
[1,2,3,4] ++ [9,10,11,12]  
-- 字串实际上就是一组字符的 List, "Hello" 只是 ['h','e','l','l','o'] 的语法糖而已。所以我们可以使用处理 List 的函数来对字串进行操作。 
"hello" ++ " " ++ "world"  
['w','o'] ++ ['o','t']  

-- 用 : 运算子往一个 List 前端插入元素会是更好的选择。
'A':" SMALL CAT"  
5:[1,2,3,4,5] 

-- 若是要按照索引取得 List 中的元素, 可以使用 !! 运算子, 索引的下标为 0。
"Steve Buscemi" !! 6  
-- 'B'  
[9.4,33.2,96.2,11.2,23.25] !! 1  
-- 33.2

-- List 同样也可以用来装 List, 甚至是 List 的 List 的 List
let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
b
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
b ++ [[1,1,1,1]]
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]
[6,6,6]:b
[[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
b !! 2
[1,2,2,3,4]

-- 当 List 内装有可比较的元素时, 使用 > 和 >= 可以比较 List 的大小。它会先比较第一个元素, 若它们的值相等, 则比较下一个, 以此类推。
[3,2,1] > [2,1,0]  
True  
[3,2,1] > [2,10,100]  
True  
[3,4,2] > [3,4]  
True  
[3,4,2] > [2,4]  
True  
[3,4,2] == [3,4,2]  
True

-- head 返回一个 List 的头部, 也就是 List 的首个元素。
head [5,4,3,2,1] 
5
-- tail 返回一个 List 的尾部, 也就是 List 除去头部之后的部分。
tail [5,4,3,2,1]  
[4,3,2,1]
-- last 返回一个 List 的最后一个元素。
last [5,4,3,2,1]  
1
-- init 返回一个 List 除去最后一个元素的部分。
init [5,4,3,2,1]
[5,4,3,2]

-- length 返回一个 List 的长度。
length [5,4,3,2,1]  
5
-- null 检查一个 List 是否为空。如果是, 则返回 True, 否则返回 False。应当避免使用 xs==[] 之类的语句来判断 List 是否为空, 使用 null 会更好。
null [1,2,3]  
False  
null []  
True
-- reverse 将一个 List 反转:
reverse [5,4,3,2,1]  
[1,2,3,4,5]
-- take 返回一个 List 的前几个元素, 看：
take 3 [5,4,3,2,1]  
[5,4,3]  
take 1 [3,9,3]  
[3]  
take 5 [1,2]  
[1,2]  
take 0 [6,6,6] 
[]
-- 如上, 若是图取超过 List 长度的元素个数, 只能得到原 List。若 take 0 个元素, 则会得到一个空 List！ drop 与 take 的用法大体相同, 它会删除一个 List 中的前几个元素。
drop 3 [8,4,2,1,5,6]  
[1,5,6]  
drop 0 [1,2,3,4]  
[1,2,3,4]  
drop 100 [1,2,3,4]  
[]
-- maximum 返回一个 List 中最大的那个元素。minimun 返回最小的。
minimum [8,4,2,1,5,6]  
1  
maximum [1,9,2,3,4]  
9
-- sum 返回一个 List 中所有元素的和。product 返回一个 List 中所有元素的积。
sum [5,2,1,6,3,2,5,7]  
31  
product [6,2,1,2]  
24  
product [1,2,5,6,7,9,2,0]  
0
-- elem 判断一个元素是否在包含于一个 List, 通常以中缀函数的形式调用它。
4 `elem` [3,4,5,6]  
True  
10 `elem` [3,4,5,6]  
False

-- Tuple ------------------
-- 我们要表示一个人的名字与年 龄, 可以使用这样的 Tuple:
("Christopher", "Walken", 55)

-- fst 返回一个序对的首项。
fst (8,11)
8
fst ("Wow", False)
"Wow"
-- snd 返回序对的尾项。
snd (8,11)
11
snd ("Wow", False)
False
-- *Note*：这两个函数仅对序对有效, 而不能应用于三元组, 四元组和五元组之上。

-- 有个函数很 cool, 它就是 zip。它可以用来生成一组序对 (Pair) 的 List。它取两个 List, 然后将它们交叉配对, 形成一组序对的 List。它很简单, 却很实用, 尤其是你需要组合或是遍历两个 List 时。如下是个例子：
zip [1,2,3,4,5] [5,5,5,5,5]
[(1,5),(2,5),(3,5),(4,5),(5,5)]
zip [1 .. 5] ["one", "two", "three", "four", "five"]
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
-- 它把元素配对并返回一个新的 List。第一个元素配第一个, 第二个元素配第二个..以此类推。注意, 由于序对中可以含有不同的型别, zip 函数可能会将不同型别的序对组合在一起。若是两个不同长度的 List 会怎么样？
zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
[(5,"im"),(3,"a"),(2,"turtle")]
-- 较长的那个会在中间断开, 去匹配较短的那个。由于 Haskell 是惰性的, 使用 zip 同时处理有限和无限的 List 也是可以的：
zip [1..] ["apple", "orange", "cherry", "mango"]
[(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]

-- Data.List 是关于 List 操作的模块, 它提供了一组非常有用的 List 处理函数。在前面我们已经见过了其中的几个函数(如 map 和 filter), 这是 Prelude 模块出于方便起见, 导出了几个Data.List 里的函数。因为这几个函数是直接引用自 Data.List, 所以就无需使用 qualified import。
import Data.List

-- intersperse 取一个元素与 List 作参数, 并将该元素置于 List 中每对元素的中间。如下是个例子:
intersperse '.' "MONKEY"  
"M.O.N.K.E.Y"  
intersperse 0 [1,2,3,4,5,6]  
[1,0,2,0,3,0,4,0,5,0,6]
-- intercalate 取两个 List 作参数。它会将第一个 List 交叉插入第二个 List 中间, 并返回一个 List.
intercalate " " ["hey","there","guys"]  
"hey there guys"  
intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]  
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
-- transpose 函数可以反转一组 List 的 List。你若把一组 List 的 List 看作是个 2D 的矩阵, 那 transpose的操作就是将其列为行。
transpose [[1,2,3],[4,5,6],[7,8,9]]  
[[1,4,7],[2,5,8],[3,6,9]]  
transpose ["hey","there","guys"]  
["htg","ehu","yey","rs","e"]
-- 假如有两个多项式 3x<sup>2</sup> + 5x + 9, 10x<sup>3</sup> + 9 和 8x<sup>3</sup> + 5x<sup>2</sup> + x - 1, 将其相加, 我们可以列三个 List: [0,3,5,9], [10,0,0,9] 和[8,5,1,-1] 来表示。再用如下的方法取得结果.
map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]  
[18,8,6,17]

-- concat 把一组 List 连接为一个 List。
concat ["foo","bar","car"]  
"foobarcar"  
concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]
-- 它相当于移除一级嵌套。若要彻底地连接其中的元素, 你得 concat 它两次才行.
-- concatMap 函数与 map 一个 List 之后再 concat 它等价.
concatMap (replicate 4) [1..3]  
[1,1,1,1,2,2,2,2,3,3,3,3]
-- and 取一组布林值 List 作参数。只有其中的值全为 True 的情况下才会返回 True。
and $ map (>4) [5,6,7,8]  
True  
and $ map (==4) [4,4,4,3,4]  
False
-- or 与 and 相似, 一组布林值 List 中若存在一个 True 它就返回 True.
or $ map (==4) [2,3,4,5,6,1]  
True  
or $ map (>4) [1,2,3]  
False
-- any 和 all 取一个限制条件和一组布林值 List 作参数, 检查是否该 List 的某个元素或每个元素都符合该条件。通常较 map 一个 List 到 and 或 or 而言, 使用 any 或 all 会更多些。
any (==4) [2,3,5,6,1,4]  
True  
all (>4) [6,9,10]  
True  
all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
False  
any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
True
-- iterate 取一个函数和一个值作参数。它会用该值去调用该函数并用所得的结果再次调用该函数, 产生一个无限的 List.
take 10 $ iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]  
take 3 $ iterate (++ "haha") "haha"  
["haha","hahahaha","hahahahahaha"]
-- splitAt 取一个 List 和数值作参数, 将该 List 在特定的位置断开。返回一个包含两个 List 的二元组.
splitAt 3 "heyman"  
("hey","man")  
splitAt 100 "heyman"  
("heyman","")  
splitAt (-3) "heyman"  
("","heyman")  
let (a,b) = splitAt 3 "foobar" in b ++ a  
"barfoo"
-- takeWhile 这一函数十分的实用。它从一个 List 中取元素, 一旦遇到不符合条件的某元素就停止.
takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
[6,5,4]  
takeWhile (/=' ') "This is a sentence"  
"This"
-- 如果要求所有三次方小于 1000 的数的和, 用 filter 来过滤 map (^3) [1..] 所得结果中所有小于 1000 的数是不行的。因为对无限 List 执行的 filter 永远都不会停止。你已经知道了这个 List 是单增的, 但 Haskell 不知道。所以应该这样：
sum $ takeWhile (<10000) $ map (^3) [1..]  
53361
-- 用 (^3) 处理一个无限 List, 而一旦出现了大于 10000 的元素这个 List 就被切断了, sum 到一起也就轻而易举.
-- dropWhile 与此相似, 不过它是扔掉符合条件的元素。一旦限制条件返回 False, 它就返回 List 的余下部分。方便实用!
dropWhile (/=' ') "This is a sentence"  
" is a sentence"  
dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
[3,4,5,4,3,2,1]
-- 给一 Tuple 组成的 List, 这 Tuple 的首项表示股票价格, 第二三四项分别表示年,月,日。我们想知道它是在哪天首次突破 $1000 的!
let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
head (dropWhile (\(val,y,m,d) -> val < 1000) stock)  
(1001.4,2008,9,4)
-- span 与 takeWhile 有点像, 只是它返回两个 List。第一个 List 与同参数调用 takeWhile 所得的结果相同, 第二个 List 就是原 List 中余下的部分。
let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest  
"First word: This, the rest: is a sentence"
-- span 是在条件首次为 False 时断开 List, 而 break 则是在条件首次为 True 时断开 List。break p 与 span (not . p) 是等价的.
break (==4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])  
span (/=4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])
-- break 返回的第二个 List 就会以第一个符合条件的元素开头。
-- sort 可以排序一个 List, 因为只有能够作比较的元素才可以被排序, 所以这一 List 的元素必须是 Ord 型别类的实例型别。
sort [8,5,3,2,1,6,4,2]  
[1,2,2,3,4,5,6,8]  
sort "This will be sorted soon"  
" Tbdeehiillnooorssstw"
-- group 取一个 List 作参数, 并将其中相邻并相等的元素各自归类, 组成一个个子 List.
group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
-- 若在 group 一个 List 之前给它排序就可以得到每个元素在该 List 中的出现次数。
map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
-- inits 和 tails 与 init 和 tail 相似, 只是它们会递归地调用自身直到什么都不剩, 看:
inits "w00t"  
["","w","w0","w00","w00t"]  
tails "w00t"  
["w00t","00t","0t","t",""]  
let w = "w00t" in zip (inits w) (tails w)  
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
-- 我们用 fold 实现一个搜索子 List 的函数:
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =  
  let nlen = length needle  
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
-- 首先, 对搜索的 List 调用 tails, 然后遍历每个 List 来检查它是不是我们想要的.
-- 由此我们便实现了一个类似 isInfixOf 的函数, isInfixOf 从一个 List 中搜索一个子 List, 若该 List 包含子 List, 则返回 True.
"cat" `isInfixOf` "im a cat burglar"  
True  
"Cat" `isInfixOf` "im a cat burglar"  
False  
"cats" `isInfixOf` "im a cat burglar"  
False
-- isPrefixOf 与 isSuffixOf 分别检查一个 List 是否以某子 List 开头或者结尾.
"hey" `isPrefixOf` "hey there!"  
True  
"hey" `isPrefixOf` "oh hey there!"  
False  
"there!" `isSuffixOf` "oh hey there!"  
True  
"there!" `isSuffixOf` "oh hey there"  
False
-- elem 与 notElem 检查一个 List 是否包含某元素.
-- partition 取一个限制条件和 List 作参数, 返回两个 List, 第一个 List 中包含所有符合条件的元素, 而第二个 List 中包含余下的.
partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOBMORGAN","sidneyeddy")  
partition (>3) [1,3,5,6,3,2,1,0,3,7]  
([5,6,7],[1,3,3,2,1,0,3])
-- 了解这个与 span 和 break 的差异是很重要的.
span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOB","sidneyMORGANeddy")
-- span 和 break 会在遇到第一个符合或不符合条件的元素处断开, 而 partition 则会遍历整个 List。
-- find 取一个 List 和限制条件作参数, 并返回首个符合该条件的元素, 而这个元素是个 Maybe 值。在下章, 我们将深入地探讨相关的算法和数据结构, 但在这里你只需了解 Maybe 值是 Just something 或Nothing 就够了。与一个 List 可以为空也可以包含多个元素相似, 一个 Maybe 可以为空, 也可以是单一元素。同样与 List 类似, 一个 Int 型的 List 可以写作 [Int], Maybe有个 Int 型可以写作 Maybe Int。先试一下 find 函数再说.
find (>4) [1,2,3,4,5,6]  
Just 5  
find (>9) [1,2,3,4,5,6]  
Nothing  
:t find  
-- find :: (a -> Bool) -> [a] -> Maybe a
-- 注意一下 find 的型别, 它的返回结果为 Maybe a, 这与 [a] 的写法有点像, 只是 Maybe 型的值只能为空或者单一元素, 而 List 可以为空,一个元素, 也可以是多个元素.
-- 想想前面那段找股票的代码, 
let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
head (dropWhile (\(val,y,m,d) -> val < 1000) stock) 
-- 。但 head 并不安全! 如果我们的股票没涨过 $1000 会怎样? dropWhile 会返回一个空 List, 而对空 List 取 head 就会引发一个错误。把它改成 
find (\(val,y,m,d) -> val > 1000) stock
-- 就安全多啦, 若存在合适的结果就得到它, 像 Just (1001.4,2008,9,4), 若不存在合适的元素(即我们的股票没有涨到过 $1000), 就会得到一个 Nothing.
-- elemIndex 与 elem 相似, 只是它返回的不是布林值, 它只是'可能' (Maybe)返回我们找的元素的索引, 若这一元素不存在, 就返回 Nothing。
:t elemIndex  
-- elemIndex :: (Eq a) => a -> [a] -> Maybe Int  
4 `elemIndex` [1,2,3,4,5,6]  
Just 3  
10 `elemIndex` [1,2,3,4,5,6]  
Nothing
-- elemIndices 与 elemIndex 相似, 只不过它返回的是 List, 就不需要 Maybe 了。因为不存在用空 List 就可以表示, 这就与 Nothing 相似了.
' ' `elemIndices` "Where are the spaces?"  
[5,9,13]
-- findIndex 与 find 相似, 但它返回的是可能存在的首个符合该条件元素的索引。findIndices 会返回所有符合条件的索引.
findIndex (==4) [5,3,2,1,6,4]  
Just 5  
findIndex (==7) [5,3,2,1,6,4]  
Nothing  
findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"  
[0,6,10,14]
-- 在前面, 我们讲过了 zip 和 zipWith, 它们只能将两个 List 组到一个二元组数或二参函数中, 但若要组三个 List 该怎么办? 好说~ 有 zip3,zip4...,和 zipWith3, zipWith4...直到 7。这看起来像是个 hack, 但工作良好。连着组 8 个 List 的情况很少遇到。还有个聪明办法可以组起无限多个 List, 但限于我们目前的水平, 就先不谈了.
zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]  
[7,9,8]  
zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]
-- 与普通的 zip 操作相似, 以返回的 List 中长度最短的那个为准.
-- 在处理来自文件或其它地方的输入时, lines 会非常有用。它取一个字串作参数。并返回由其中的每一行组成的 List.
lines "first line\nsecond line\nthird line"  
["first line","second line","third line"]
-- '\n' 表示unix下的换行符, 在 Haskell 的字符中, 反斜杠表示特殊字符.
-- unlines 是 lines 的反函数, 它取一组字串的 List, 并将其通过 '\n'合并到一块.
unlines ["first line", "second line", "third line"]  
"first line\nsecond line\nthird line\n"
-- words 和 unwords 可以把一个字串分为一组单词或执行相反的操作, 很有用.
words "hey these are the words in this sentence"  
["hey","these","are","the","words","in","this","sentence"]  
words "hey these are the words in this\nsentence"  
["hey","these","are","the","words","in","this","sentence"]  
unwords ["hey","there","mate"]  
"hey there mate"
-- 我们前面讲到了 nub, 它可以将一个 List 中的重复元素全部筛掉, 使该 List 的每个元素都如雪花般独一无二, 'nub' 的含义就是'一小块'或'一部分', 用在这里觉得很古怪。我觉得, 在函数的命名上应该用更确切的词语, 而避免使用老掉牙的过时词汇.
nub [1,2,3,4,3,2,1,2,3,4,3,2,1]  
[1,2,3,4]  
nub "Lots of words and stuff"  
"Lots fwrdanu"
-- delete 取一个元素和 List 作参数, 会删掉该 List 中首次出现的这一元素.
delete 'h' "hey there ghang!"  
"ey there ghang!"  
delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere ghang!"  
delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere gang!"
-- \ 表示 List 的差集操作, 这与集合的差集很相似, 它会从左边 List 中的元素扣除存在于右边 List 中的元素一次.
[1..10] \\ [2,5,9]  
[1,3,4,6,7,8,10]  
"Im a big baby" \\ "big"  
"Im a  baby"
-- union 与集合的并集也是很相似, 它返回两个 List 的并集, 即遍历第二个 List 若存在某元素不属于第一个 List, 则追加到第一个 List。看, 第二个 List 中的重复元素就都没了!
"hey man" `union` "man what's up"  
"hey manwt'sup"  
[1..7] `union` [5..10]  
[1,2,3,4,5,6,7,8,9,10]
-- intersection 相当于集合的交集。它返回两个 List 的相同部分.
[1..7] `intersect` [5..10]  
[5,6,7]
-- insert 可以将一个元素插入一个可排序的 List, 并将其置于首个大于等于它的元素之前, 如果使用 insert来给一个排过序的 List 插入元素, 返回的结果依然是排序的.
insert 4 [1,2,3,5,6,7]  
[1,2,3,4,5,6,7]  
insert 'g' $ ['a'..'f'] ++ ['h'..'z']  
"abcdefghijklmnopqrstuvwxyz"  
insert 3 [1,2,4,3,2,1]  
[1,2,3,4,3,2,1]
-- length, take, drop, splitAt, !! 和 replicate 之类的函数有个共同点。那就是它们的参数中都有个 Int 值（或者返回Int值）, 我觉得使用 Intergal 或 Num 型别类会更好, 但出于历史原因, 修改这些会破坏掉许多既有的代码。在 Data.List 中包含了更通用的替代版, 如: genericLength, genericTake, genericDrop, genericSplitAt, genericIndex 和 genericReplicate。length 的型别声明为 length :: [a] -> Int, 而我们若要像这样求它的平均值, let xs = [1..6] in sum xs / length xs , 就会得到一个型别错误, 因为 / 运算符不能对 Int 型使用! 而 genericLength 的型别声明则为 genericLength :: (Num a) => [b] -> a, Num 既可以是整数又可以是浮点数, let xs = [1..6] in sum xs / genericLength xs 这样再求平均数就不会有问题了.
-- nub, delete, union, intsect 和 group 函数也有各自的通用替代版nubBy, deleteBy, unionBy, intersectBy 和 groupBy, 它们的区别就是前一组函数使用(==) 来测试是否相等, 而带 By 的那组则取一个函数作参数来判定相等性, group 就与 groupBy (==) 等价.
-- 假如有个记录某函数在每秒的值的 List, 而我们要按照它小于零或者大于零的交界处将其分为一组子 List。如果用 group, 它只能将相邻并相等的元素组到一起, 而在这里我们的标准是它们是否互为相反数。groupBy 登场! 它取一个含两个参数的函数作为参数来判定相等性.
let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]  
groupBy (\x y -> (x > 0) == (y > 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
-- 这样一来我们就可以很清楚地看出哪部分是正数, 哪部分是负数, 这个判断相等性的函数会在两个元素同时大于零或同时小于零时返回 True。也可以写作 \x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)。但我觉得第一个写法的可读性更高。Data.Function 中还有个 on 函数可以让它的表达更清晰, 其定义如下:
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)
-- 执行 (==) `on` (> 0) 得到的函数就与 \x y -> (x > 0) == (y > 0) 基本等价。on 与带 By 的函数在一起会非常好用, 你可以这样写:
groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
-- 可读性很高! 你可以大声念出来: 按照元素是否大于零, 给它分类！
-- 同样, sort, insert, maximum 和 min 都有各自的通用版本。如 groupBy 类似, sortBy, insertBy, maximumBy 和 minimumBy 都取一个函数来比较两个元素的大小。像 sortBy的型别声明为: sortBy :: (a -> a -> Ordering) -> [a] -> [a]。前面提过, Ordering 型别可以有三个值,LT, EQ 和 GT。compare 取两个 Ord 型别类的元素作参数, 所以 sort 与 sortBy compare 等价.
-- List 是可以比较大小的, 且比较的依据就是其中元素的大小。如果按照其子 List 的长度为标准当如何? 很好, 你可能已经猜到了, sortBy 函数.
let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  
sortBy (compare `on` length) xs  
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
-- 太绝了! compare `on` length, 乖乖, 这简直就是英文! 如果你搞不清楚 on 在这里的原理, 就可以认为它与 \x y -> length x `compare` length y 等价。通常, 与带 By 的函数打交道时, 若要判断相等性, 则 (==) `on` something。若要判定大小, 则 compare `on` something.










