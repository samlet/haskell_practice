-- toplevel
succ 8
min 9 10
min 3.4 3.2

-- 函数调用拥有最高的优先级，如下两句是等效的
succ 9 + max 5 4 + 1
(succ 9) + (max 5 4) + 1

:l math_funcs
factorial 50  

circumference 4.0  
25.132742
circumference' 4.0  
25.132741228718345
