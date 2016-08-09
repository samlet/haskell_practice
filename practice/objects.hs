-- toplevel
-- 表示圆可以用一个 Tuple，如 (43.1,55.0,10.4)，前两项表示圆心的位置，末项表示半径。听着不错，不过三维矢量或其它什么东西也可能是这种形式！更好的方法就是自己构造一个表示图形的型别。假定图形可以是圆 (Circle) 或长方形 (Rectangle)：
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
:l shapes

-- 谈到「项」 (field)，其实应为「参数」 (parameters)。值构造子的本质是个函数，可以返回一个型别的值。我们看下这两个值构造子的型别声明：
:t Circle
-- Circle :: Float -> Float -> Float -> Shape
:t Rectangle
-- Rectangle :: Float -> Float -> Float -> Float -> Shape

-- 我们只关心圆的半径，因此不需理会表示坐标的前两项：
surface $ Circle 10 20 10
314.15927
surface $ Rectangle 0 0 100 100
10000.0
-- Yay，it works！不过我们若尝试输出 Circle 10 20 到控制台，就会得到一个错误。这是因为 Haskell 还不知道该型别的字符串表示方法。想想，当我们往控制台输出值的时候，Haskell 会先调用 show 函数得到这个值的字符串表示才会输出。因此要让我们的 Shape 型别成为 Show 型别类的成员。可以这样修改：
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- 先不去深究 deriving（派生），可以先这样理解：若在 data 声明的后面加上 deriving (Show)，那 Haskell 就会自动将该型别至于 Show 型别类之中。好了，由于值构造子是个函数，因此我们可以拿它交给map，拿它不全调用，以及普通函数能做的一切。
Circle 10 20 5
Circle 10.0 20.0 5.0
Rectangle 50 230 60 90
Rectangle 50.0 230.0 60.0 90.0
-- 我们若要取一组不同半径的同心圆，可以这样：
map (Circle 10 20) [4,5,6,6]
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]

:l shapes_2
-- 唯一需要修改的地方就是模式。在 Circle 的模式中，我们无视了整个 Point。而在 Rectangle 的模式中，我们用了一个嵌套的模式来取得 Point 中的项。若出于某原因而需要整个 Point，那么直接匹配就是了。
surface (Rectangle (Point 0 0) (Point 100 100))
10000.0
surface (Circle (Point 0 0) 24)
1809.5574


nudge (Circle (Point 34 34) 10) 5 10
Circle (Point 39.0 44.0) 10.0

nudge (baseRect 40 100) 60 23
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
-- 毫无疑问，你可以把你的数据型别导出到模块中。只要把你的型别与要导出的函数写到一起就是了。再在后面跟个括号，列出要导出的值构造子，用逗号隔开。如要导出所有的值构造子，那就写个..。
-- , Shape(..)
-- 一个 Shape (..)，我们就导出了 Shape 的所有值构造子。这一来无论谁导入我们的模块，都可以用Rectangle 和 Circle 值构造子来构造 Shape 了。这与写 Shape(Rectangle,Circle) 等价。

:l types
-- O~Kay，第一项是名，第二项是姓，第三项是年龄，等等。我们造一个人：
let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
guy
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

-- 与原先让那些项一个挨一个的空格隔开不同，这里用了花括号 {}。先写出项的名字，如 firstName，后跟两个冒号(也叫 Paamayim Nekudotayim，哈哈~(译者不知道什么意思~囧))，标明其型别，返回的数据型别仍与以前相同。这样的好处就是，可以用函数从中直接按项取值。通过 Record Syntax，Haskell 就自动生成了这些函数：firstName, lastName, age, height, phoneNumber 和 flavor。
:t flavor
-- flavor :: Person -> String
:t firstName
-- firstName :: Person -> String
-- 还有个好处，就是若派生 (deriving) 到 Show 型别类，它的显示是不同的。假如我们有个型别表示一辆车，要包含生产商、型号以及出场年份：
-- data Car = Car String String Int deriving (Show)
-- Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967
-- 若用 Record Syntax，就可以得到像这样的新车：
-- data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}
-- 这一来在造车时我们就不必关心各项的顺序了。

-- 值构造子可以取几个参数产生一个新值，如 Car 的构造子是取三个参数返回一个 Car。与之相似，型别构造子可以取型别作参数，产生新的型别。这咋一听貌似有点深奥，不过实际上并不复杂。如果你对 C++ 的模板有了解，就会看到很多相似的地方。我们看一个熟悉的型别，好对型别参数有个大致印象：
-- 这里的a就是个型别参数。也正因为有了它，Maybe 就成为了一个型别构造子。在它的值不是 Nothing时，它的型别构造子可以搞出 Maybe Int，Maybe String 等等诸多态别。但只一个 Maybe 是不行的，因为它不是型别，而是型别构造子。要成为真正的型别，必须得把它需要的型别参数全部填满。
-- 所以，如果拿 Char 作参数交给 Maybe，就可以得到一个 Maybe Char 的型别。如，Just 'a' 的型别就是 Maybe Char 。
-- 你可能并未察觉，在遇见 Maybe 之前我们早就接触到型别参数了。它便是 List 型别。这里面有点语法糖，List 型别实际上就是取一个参数来生成一个特定型别，这型别可以是 [Int]，[Char] 也可以是[String]，但不会跟在 [] 的后面。

data Maybe a = Nothing | Just a

-- Just "Haha"
-- Just 84
:t Just "Haha"
-- Just "Haha" :: Maybe [Char]
:t Just 84
-- Just 84 :: (Num t) => Maybe t
:t Nothing
-- Nothing :: Maybe a
-- Just 10 :: Maybe Double
-- Just 10.0

-- 把我们的Car型别改成
:{
data Car a b c = Car { company :: a
                       , model :: b
                       , year :: c
                        } deriving (Show)
:}

-- 还可以弄个简单函数来描述车的属性。
-- tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
let stang = Car {company="Ford", model="Mustang", year=1967}
tellCar stang
"This Ford Mustang was made in 1967"

tellCar (Car "Ford" "Mustang" 1967)
"This Ford Mustang was made in 1967"
tellCar (Car "Ford" "Mustang" "nineteen sixty seven")
"This Ford Mustang was made in \"nineteen sixty seven\""
:t Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t
:t Car "Ford" "Mustang" "nineteen sixty seven"
-- Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]

-- start here
:l types
Vector 3 5 8 `vplus` Vector 9 2 8
Vector 12 7 16
Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
Vector 12 9 19
Vector 3 9 7 `vectMult` 10
Vector 30 90 70
Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
74.0
Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
Vector 148 666 222


-- Int 型别是 Eq 型别类的一个 instance，Eq 类就定义了判定相等性的行为。Int 值可以判断相等性，所以 Int 就是 Eq 型别类的成员。它的真正威力体现在作为 Eq 接口的函数中，即 == 和 /=。只要一个型别是 Eq 型别类的成员，我们就可以使用 == 函数来处理这一型别。这便是为何 4==4 和 "foo"/="bar" 这样的表达式都需要作型别检查。
-- 型别类更像是接口，我们不是靠它构造数据，而是给既有的数据型别描述行为。什么东西若可以判定相等性，我们就可以让它成为 Eq 型别类的 instance。什么东西若可以比较大小，那就可以让它成为 Ord 型别类的 instance。

-- 可以判定姓名年龄的相等性，来判断它俩是否相等。这一来，让这个型别成为 Eq 的成员就很靠谱了。直接 derive 这个 instance：
:{
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)
:}

-- 在一个型别 derive 为 Eq 的 instance 后，就可以直接使用 == 或 /= 来判断它们的相等性了。Haskell 会先看下这两个值的值构造子是否一致(这里只是单值构造子)，再用 == 来检查其中的所有数据(必须都是Eq 的成员)是否一致。在这里只有 String 和 Int，所以是没有问题的。测试下我们的 Eqinstance：
let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}
mca == adRock
False
mikeD == adRock
False
mikeD == mikeD
True
mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}
True
-- 自然，Person 如今已经成为了 Eq 的成员，我们就可以将其应用于所有在型别声明中用到 Eq 类约束的函数了，如 elem。
let beastieBoys = [mca, adRock, mikeD]
mikeD `elem` beastieBoys
True
-- Show 和 Read 型别类处理可与字符串相互转换的东西。同 Eq 相似，如果一个型别的构造子含有参数，那所有参数的型别必须都得属于 Show 或 Read 才能让该型别成为其 instance。就让我们的Person 也成为 Read 和 Show 的一员吧。
:{
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
:}                     
-- 然后就可以输出一个 Person 到控制台了。
let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
mikeD
Person {firstName = "Michael", lastName = "Diamond", age = 43}
"mikeD is: " ++ show mikeD
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
-- 如果我们还没让 Person 型别作为 Show 的成员就尝试输出它，Haskell 就会向我们抱怨，说它不知道该怎么把它表示成一个字符串。不过现在既然已经 derive 成为了 Show 的一个 instance，它就知道了。
-- Read 几乎就是与 Show 相对的型别类，show 是将一个值转换成字符串，而 read 则是将一个字符串转成某型别的值。还记得，使用 read 函数时我们必须得用型别注释注明想要的型别，否则 Haskell 就不会知道如何转换。
read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
Person {firstName = "Michael", lastName = "Diamond", age = 43}
-- 如果我们 read 的结果会在后面用到参与计算，Haskell 就可以推导出是一个 Person 的行为，不加注释也是可以的。
read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
True
-- 也可以 read 带参数的型别，但必须填满所有的参数。因此 read "Just 't'" :: Maybe a 是不可以的，read "Just 't'" :: Maybe Char 才对。
-- 很容易想象 Ord 型别类 derive instance 的行为。首先，判断两个值构造子是否一致，如果是，再判断它们的参数，前提是它们的参数都得是 Ord 的 instance。Bool 型别可以有两种值，False 和 True。为了了解在比较中进程的行为，我们可以这样想象：
-- data Bool = False | True deriving (Ord)
-- 由于值构造子 False 安排在 True 的前面，我们可以认为 True 比 False 大。
True `compare` False
GT
True > False
True
True < False
False
-- 在 Maybe a 数据型别中，值构造子 Nothing 在 Just 值构造子前面，所以一个 Nothing 总要比Just something 的值小。即便这个 something 是 -100000000 也是如此。
Nothing < Just 100
True
Nothing > Just (-49999)
False
Just 3 `compare` Just 2
GT
Just 100 > Just 50
True
-- 不过类似 Just (*3) > Just (*2) 之类的代码是不可以的。因为 (*3) 和 (*2) 都是函数，而函数不是 Ord 类的成员。
-- 作枚举，使用数字型别就能轻易做到。不过使用 Enum 和 Bounded 型别类会更好，看下这个型别：
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
-- 所有的值构造子都是 nullary 的(也就是没有参数)，每个东西都有前置子和后继子，我们可以让它成为Enum 型别类的成员。同样，每个东西都有可能的最小值和最大值，我们也可以让它成为 Bounded 型别类的成员。在这里，我们就同时将它搞成其它可 derive型别类的 instance。再看看我们能拿它做啥：
:{
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
:}           
-- 由于它是 Show 和 Read 型别类的成员，我们可以将这个型别的值与字符串相互转换。
Wednesday
Wednesday
show Wednesday
"Wednesday"
read "Saturday" :: Day
Saturday
-- 由于它是 Eq 与 Ord 的成员，因此我们可以拿 Day 作比较。
Saturday == Sunday
False
Saturday == Saturday
True
Saturday > Friday
True
Monday `compare` Wednesday
LT
-- 它也是 Bounded 的成员，因此有最早和最晚的一天。
minBound :: Day
Monday
maxBound :: Day
Sunday
-- 它也是 Enum 的 instance，可以得到前一天和后一天，并且可以对此使用 List 的区间。
succ Monday
Tuesday
pred Saturday
Friday
[Thursday .. Sunday]
[Thursday,Friday,Saturday,Sunday]
[minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
-- 那是相当的棒。

-- 很酷的二参型别就是 Either a b 了，它大约是这样定义的：
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- 它有两个值构造子。如果用了 Left，那它内容的型别就是 a；用了 Right，那它内容的型别就是b。我们可以用它来将可能是两种型别的值封装起来，从里面取值时就同时提供 Left 和 Right 的模式匹配。
Right 20
Right 20
Left "w00t"
Left "w00t"
:t Right 'a'
Right 'a' :: Either a Char
:t Left True
Left True :: Either Bool b


-- start here
:l types
-- 现在从里面 lookup 某个橱子号..
lockerLookup 101 lockers
Right "JAH3I"
lockerLookup 100 lockers
Left "Locker 100 is already taken!"
lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"
lockerLookup 110 lockers
Left "Locker 110 is already taken!"
lockerLookup 105 lockers
Right "QOTSA"
-- 我们完全可以用 Maybe a 来表示它的结果，但这样一来我们就对得不到密码的原因不得而知了。而在这里，我们的新型别可以告诉我们失败的原因。

-- 我们可以说一个 List 的定义是要么是空的 List 或是一个元素，后面用 : 接了另一串 List。
-- 我们用 algebraic data type 来实作我们自己的 List！
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- 这读起来好像我们前一段提及的定义。他要么是空的 List，或是一个元素跟一串 List 的结合。如果你被搞混了，看看用 record syntax 定义的可能比较清楚。
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
-- 你可能也对这边的 Cons 构造子不太清楚。cons 其实就是指 :。对 List 而言，: 其实是一个构造子，他接受一个值跟另一串 List 来构造一个 List。现在我们可以使用我们新定义的 List 型态。换句话说，他有两个 field，其中一个 field 具有型态 a，另一个有型态 [a]。
Empty
--Empty
5 `Cons` Empty
--Cons 5 Empty
4 `Cons` (5 `Cons` Empty)
--Cons 4 (Cons 5 Empty)
3 `Cons` (4 `Cons` (5 `Cons` Empty))
--Cons 3 (Cons 4 (Cons 5 Empty))
-- 我们用中缀的方式调用 Cons 构造子，这样你可以很清楚地看到他就是 :。Empty 代表 []，而 4 `Cons` (5 `Cons` Empty) 就是 4:(5:[])。
-- 我们可以只用特殊字符来定义函数，这样他们就会自动具有中缀的性质。我们也能同样的手法套用在构造子上，毕竟他们不过是回传型态的函数而已。

-- start here
:l types

-- 这样我们就可以写成 a :-: (List a) 而不是 Cons a (List a)：
3 :-: 4 :-: 5 :-: Empty
-- (:-:) 3 ((:-:) 4 ((:-:) 5 Empty))
let a = 3 :-: 4 :-: 5 :-: Empty
100 :-: a
-- (:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))
-- Haskell 在宣告 deriving Show 的时候，他会仍视构造子为前缀函数，因此必须要用括号括起来。

let a = 3 :-: 4 :-: 5 :-: Empty
let b = 6 :-: 7 :-: Empty
a .++ b
-- (:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty))))
-- 如果我们想要的话，我们可以定义其他操作我们list的函数。






