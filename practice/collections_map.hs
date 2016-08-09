-- exec: collections_map
module Main where
import Data.Map hiding (filter, foldr)

main = putStrLn "Hello World"

-- 关联列表(也叫做字典)是按照键值对排列而没有特定顺序的一种 List。例如，我们用关联列表保存电话号码，号码就是值，人名就是键。我们并不关心它们的存储顺序，只要能按人名得到正确的号码就好.在 Haskell 中表示关联列表的最简单方法就是弄一个二元组的 List，而这二元组就首项为键，后项为值。如下便是个表示电话号码的关联列表:
phoneBook = [("betty","555-2938") ,
             ("bonnie","452-2928") ,
             ("patsy","493-2928") ,
             ("lucille","205-2928") ,
             ("wendy","939-8282") ,
             ("penny","853-2492") ]

findKey' :: (Eq k) => k -> [(k,v)] -> v 
findKey' key xs = snd . head . filter (\(k,v) -> key == k) $ xs

-- 但若该关联列表中不存在这个键那会怎样? 哼，那就会在试图从空 List 中取 head 时引发一个运行时错误。无论如何也不能让进程就这么轻易地崩溃吧，所以就应该用 Maybe 型别。如果没找到相应的键，就返回Nothing。而找到了就返回 Just something。而这 something 就是键对应的值。
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v 
findKey key [] = Nothing
findKey key ((k,v):xs) = 
     if key == k then 
         Just v 
     else 
         findKey key xs

-- 用 fold 怎样实现
-- 通常，使用 ``fold`` 来替代类似的递归函数会更好些。用 ``fold`` 的代码让人一目了然，而看明白递归则得多花点脑子。
findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v 
findKey'' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing








