-- collections_tree
module Main where

main = putStrLn "Hello World"

-- 这边我们来定义一棵树的结构：他不是一棵空的树就是带有值并含有两棵子树。听起来非常符合 algebraic data type 的结构！
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- 来看下列两个函数。第一个做了一个单节点的树，而第二个插入一个元素到一棵树中。
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
      | x == a = Node x left right
      | x < a  = Node a (treeInsert x left) right
      | x > a  = Node a left (treeInsert x right)

-- singleton 函数只是一个做一个含有两棵空子树的节点的函数的别名。在插入的操作中，我们先为终端条件定义了一个模式匹配。如果我们走到了一棵空的子树，这表示我们到达了我们想要的地方，我们便建造一棵空的单元素的树来放在那个位置。如果我们还没走到一棵空的树来插入我们的元素。那就必须要做一些检查来往下走。如果我们要安插的元素跟 root 所含有的元素相等，那就直接回传这棵树。如果安插的元素比较小，就回传一棵新的树。这棵树的 root 跟原来的相同，右子树也相同，只差在我们要安插新的元素到左子树中。如果安插的元素反而比较大，那整个过程就相反。
-- 接下来，我们要写一个函数来检查某个元素是否已经在这棵树中。首先我们定义终端条件。如果我们已经走到一棵空的树，那这个元素一定不在这棵树中。这跟我们搜索 List 的情形是一致的。如果我们要在空的 List 中搜索某一元素，那就代表他不在这个 List 里面。假设我们现在搜索一棵非空的树，而且 root 中的元素刚好就是我们要的，那就找到了。那如果不是呢？我们就要利用在 root 节点左边的元素都比 root 小的这个性质。如果我们的元素比 root 小，那就往左子树中找。如果比较大，那就往右子树中找。
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right









