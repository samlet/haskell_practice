-- modules_ops.hs
-- 你也可以在 ghci 中装载模块，若要调用 Data.List 中的函数，就这样:
:m Data.List
-- 若要在 ghci 中装载多个模块，不必多次 :m 命令，一下就可以全部搞定:
:m Data.List Data.Map Data.Set

-- 如果你只用得到某模块的两个函数，大可仅包含它俩。若仅装载 Data.List 模块 nub 和 sort，就这样:
import Data.List (nub，sort)
-- 也可以只包含除去某函数之外的其它函数，这在避免多个模块中函数的命名冲突很有用。假设我们的代码中已经有了一个叫做 nub 的函数，而装入 Data.List 模块时就要把它里面的 nub 除掉.
import Data.List hiding (nub)

-- 避免命名冲突还有个方法，便是 qualified import，Data.Map 模块提供一了一个按键索值的数据结构，它里面有几个和 Prelude 模块重名的函数。如 filter 和 null，装入 Data.Map 模块之后再调用 filter，Haskell 就不知道它究竟是哪个函数。如下便是解决的方法:
import qualified Data.Map

-- 这样一来，再调用 Data.Map 中的 filter 函数，就必须得 Data.Map.filter，而 filter 依然是为我们熟悉喜爱的样子。但是要在每个函数前面都加 个Data.Map 实在是太烦人了! 那就给它起个别名，让它短些:
import qualified Data.Map as M
-- 好，再调用 Data.Map 模块的 filter 函数的话仅需 M.filter 就行了

