-- collections_tree_ops.hs

:l collections_tree

-- 首先我们不想手动一个个来创造一棵树。我们想用一个fold 来从一个 List 创造一棵树。要知道走遍一个 List 并回传某种值的操作都可以用 fold 来实现。我们先从一棵空的树开始，然后从右边走过 List的 每一个元素，一个一个丢到树里面。
let nums = [8,6,4,1,7,3,5]
let numsTree = foldr treeInsert EmptyTree nums
numsTree
-- Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))
-- 在 foldr 中，treeInsert 是做 folding 操作的函数，而 EmptyTree 是起始的 accumulator，nums则是要被走遍的 List。
-- 当我们想把我们的树印出来的时候，印出来的形式会不太容易读。但如果我们能有结构地印出来呢？我们知道 root 是 5，他有两棵子树，其中一个的 root 是 3 另一个则是 7。
8 `treeElem` numsTree
True
100 `treeElem` numsTree
False
1 `treeElem` numsTree
True
10 `treeElem` numsTree
False
-- 检查元素是否属于某棵树的函数现在能正常运作了！
-- 你可以看到 algebraic data structures 是非常有力的概念。我们可以使用这个结构来构造出布林值，周一到周五的概念，甚至还有二元树。

