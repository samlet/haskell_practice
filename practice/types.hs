-- types.hs
module Main where
import qualified Data.Map as Map

main = putStrLn "Hello World"

-- 我们需要一个数据型别来描述一个人，得包含他的姓、名、年龄、身高、电话号码以及最爱的冰淇淋。我不知你的想法，不过我觉得要了解一个人，这些数据就够了。就这样，实现出来！
-- data Person = Person String String Int Float String String deriving (Show)

-- Record Syntax。
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- 型别表示一辆车，要包含生产商、型号以及出场年份：
-- data Car = Car String String Int deriving (Show)
-- 若用 Record Syntax，就可以得到像这样的新车：
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- 我们实现个表示三维矢量的型别，再给它加几个处理函数。我么那就给它个型别参数，虽然大多数情况都是数值型，不过这一来它就支持了多种数值型别。
data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
-- vplus 用来相加两个矢量，即将其所有对应的项相加。scalarMult 用来求两个矢量的标量积，vectMult 求一个矢量和一个标量的积。这些函数可以处理 Vector Int，Vector Integer，Vector Float 等等型别，只要 Vector a 里的这个 a 在 Num 型别类中就行。同样，如果你看下这些函数的型别声明就会发现，它们只能处理相同型别的矢量，其中包含的数字型别必须与另一个矢量一致。注意，我们并没有在 data 声明中添加 Num 的类约束。反正无论怎么着都是给函数加约束。

-- phoneBook 的型别就是 [(String,String)]，这表示一个关联 List 仅是 String 到 String 的映射关系。我们就弄个型别别名，好让它型别声明中能够表达更多信息。
-- 如果不用型别别名，我们函数的型别声明就只能是 String -> String -> [(String ,String)] -> Bool了。在这里使用型别别名是为了让型别声明更加易读，但你也不必拘泥于它。
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- 一个函数，它可以取一名字和号码检查它是否存在于电话本。现在可以给它加一个相当好看明了的型别声明：
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- 如果你想搞个型别来表示关联 List，但依然要它保持通用，好让它可以使用任意型别作 key 和 value，我们可以这样：
type AssocList k v = [(k,v)]

-- 如果我们要一个表示从整数到某东西间映射关系的型别，我们可以这样：
-- type IntMap v = Map Int v
-- 也可以这样：
type IntMap = Map.Map Int

-- 另一个很酷的二参型别就是 Either a b 了，它大约是这样定义的：
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- 它有两个值构造子。如果用了 Left，那它内容的型别就是 a；用了 Right，那它内容的型别就是b。我们可以用它来将可能是两种型别的值封装起来，从里面取值时就同时提供 Left 和 Right 的模式匹配。

-- 一个例子：有个学校提供了不少壁橱，好给学生们地方放他们的 Gun'N'Rose 海报。每个壁橱都有个密码，哪个学生想用个壁橱，就告诉管理员壁橱的号码，管理员就会告诉他壁橱的密码。但如果这个壁橱已经让别人用了，管理员就不能告诉他密码了，得换一个壁橱。我们就用 Data.Map 的一个 Map 来表示这些壁橱，把一个号码映射到一个表示壁橱占用情况及密码的 Tuple 里。

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)
-- 很简单，我们引入了一个新的型别来表示壁橱的占用情况。并为壁橱密码及按号码找壁橱的 Map 分别设置了一个别名。好，现在我们实现这个按号码找壁橱的函数，就用 Either String Code 型别表示我们的结果，因为 lookup 可能会以两种原因失败。橱子已经让别人用了或者压根就没有这个橱子。如果 lookup失败，就用字符串表明失败的原因。
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
-- 我们在这里个 Map 中执行一次普通的 lookup，如果得到一个 Nothing，就返回一个 Left String 的值，告诉他压根就没这个号码的橱子。如果找到了，就再检查下，看这橱子是不是已经让别人用了，如果是，就返回个 Left String 说它已经让别人用了。否则就返回个 Right Code 的值，通过它来告诉学生壁橱的密码。它实际上就是个 Right String，我们引入了个型别别名让它这型别声明更好看。
-- 如下是个 Map 的例子：
lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

-- 我们在来写个函数来把两个 List 连起来。一般 ++ 在操作普通 List 的时候是这样的：
-- infixr 5  ++
-- (++) :: [a] -> [a] -> [a]
-- []     ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

-- 我们把他偷过来用在我们的 List 上，把函数命名成 .++：
infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)










