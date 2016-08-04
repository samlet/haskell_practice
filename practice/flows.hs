-- toplevel
-- Haskell支持标准的条件表达式
mySignum x =
    if x < 0 then 
        -1
    else if x > 0 then 
        1
    else 
        0

mySignum 5
mySignum (5-10)

--  case 结构用于组合多个条件语句
-- 如果x不符合之前任何列出的值那么f的值为{\displaystyle -1}（"_"为“通配符”表示任何值都可以符合这个条件）
:{
f x =
    case x of
      0 -> 1
      1 -> 5
      2 -> 2
      _ -> (-1)
:}
-- 使用分号跟花括号的方法      
f x = case x of { 0 -> 1 ; 1 -> 5 ; 2 -> 2 ; _ -> -1 }
f x =
    case x of { 0 -> 1 ;
      1 -> 5 ; 2 -> 2
   ; _ -> -1 }

:{
classify age = case age of 0 -> "newborn"
                           1 -> "infant"
                           2 -> "toddler"
                           _ -> "senior citizen"
:}
classify 5




