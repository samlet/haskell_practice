:m Data.Char

-- Data.Char 模块包含了一组用于处理字符的函数。由于字串的本质就是一组字符的 List，所以往往会在 filter 或是 map 字串时用到它.
-- Data.Char模块中含有一系列用于判定字符范围的函数，如下:

-- isControl 判断一个字符是否是控制字符。 isSpace 判断一个字符是否是空格字符，包括空格，tab，换行符等. isLower 判断一个字符是否为小写. isUper 判断一个字符是否为大写。 isAlpha 判断一个字符是否为字母. isAlphaNum 判断一个字符是否为字母或数字. isPrint 判断一个字符是否是可打印的. isDigit 判断一个字符是否为数字. isOctDigit 判断一个字符是否为八进制数字. isHexDigit 判断一个字符是否为十六进制数字. isLetter 判断一个字符是否为字母. isMark 判断是否为 unicode 注音字符，你如果是法国人就会经常用到的. isNumber 判断一个字符是否为数字. isPunctuation 判断一个字符是否为标点符号. isSymbol判断一个字符是否为货币符号. isSeperater 判断一个字符是否为 unicode 空格或分隔符. isAscii 判断一个字符是否在 unicode 字母表的前 128 位。 isLatin1 判断一个字符是否在 unicode 字母表的前 256 位.isAsciiUpper 判断一个字符是否为大写的 ascii 字符. isAsciiLower 判断一个字符是否为小写的 ascii 字符.
-- 以上所有判断函数的型别声明皆为 Char -> Bool，用到它们的绝大多数情况都无非就是过滤字串或类似操作。假设我们在写个进程，它需要一个由字符和数字组成的用户名。要实现对用户名的检验，我们可以结合使用 Data.List 模块的 all 函数与 Data.Char 的判断函数.

all isAlphaNum "bobby283"  
True  
all isAlphaNum "eddy the fish!"  
False
-- Kewl~ 免得你忘记，all 函数取一个判断函数和一个 List 做参数，若该 List 的所有元素都符合条件，就返回 True.
-- 也可以使用 isSpace 来实现 Data.List 的 words 函数.
words "hey guys its me"  
["hey","guys","its","me"]  
groupBy ((==) `on` isSpace) "hey guys its me"  
["hey"," ","guys"," ","its"," ","me"]  

-- Hmm，不错，有点 words 的样子了。只是还有空格在里面，恩，该怎么办? 我知道，用 filter 滤掉它们!
filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"  
["hey","guys","its","me"]
-- 啊哈.
-- Data.Char 中也含有与 Ordering 相似的型别。Ordering 可以有三个值，LT，GT 和 EQ。这就是个枚举，它表示了两个元素作比较可能的结果. GeneralCategory 型别也是个枚举，它表示了一个字符可能所在的分类。而得到一个字符所在分类的主要方法就是使用 generalCategory 函数.它的型别为:generalCategory :: Char -> GeneralCategory。那 31 个分类就不在此一一列出了，试下这个函数先:
generalCategory ' '  
Space  
generalCategory 'A'  
UppercaseLetter  
generalCategory 'a'  
LowercaseLetter  
generalCategory '.'  
OtherPunctuation  
generalCategory '9'  
DecimalNumber  
map generalCategory " \t\nA9?|"  
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
-- 由于 GeneralCategory 型别是 Eq 型别类的一部分，使用类似 generalCategory c == Space 的代码也是可以的.
-- toUpper 将一个字符转为大写字母，若该字符不是小写字母，就按原值返回. toLower 将一个字符转为小写字母，若该字符不是大写字母，就按原值返回. toTitle 将一个字符转为 title-case，对大多数字元而言，title-case 就是大写. digitToInt 将一个字符转为 Int 值，而这一字符必须得在'1'..'9','a'..'f'或'A'..'F' 的范围之内.
map digitToInt "34538"  
[3,4,5,3,8]  
map digitToInt "FF85AB"  
[15,15,8,5,10,11]
-- intToDigit 是 digitToInt 的反函数。它取一个 0 到 15 的 Int 值作参数，并返回一个小写的字符.
intToDigit 15  
'f'  
intToDigit 5  
'5'
-- ord 与 char 函数可以将字符与其对应的数字相互转换.
ord 'a'  
97  
chr 97  
'a'  
map ord "abcdefgh"  
[97,98,99,100,101,102,103,104]
-- 两个字符的 ord 值之差就是它们在 unicode 字符表上的距离.

:l strings_encoding
-- 先将一个字串转为一组数字，然后给它加上某数，再转回去。如果你是标准的组合牛仔，大可将函数写为:map (chr . (+ shift) . ord) msg。试一下它的效果:
encode 3 "Heeeeey"  
"Khhhhh|"  
encode 4 "Heeeeey"  
"Liiiii}"  
encode 1 "abcd"  
"bcde"  
encode 5 "Marry Christmas! Ho ho ho!"  
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
-- 不错。再简单地将它转成一组数字，减去某数后再转回来就是解密了.
-- decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg
encode 3 "Im a little teapot"  
"Lp#d#olwwoh#whdsrw"  
decode 3 "Lp#d#olwwoh#whdsrw"  
"Im a little teapot"  
decode 5 . encode 5 $ "This is a sentence"  
"This is a sentence"

