-- toplevel

-- http://www.vaikan.com/10-haskell-one-liners-to-impress-your-friends/
-- cabal install parallel
-- 下面的例子需要使用 parallel 代码包。

import Control.Parallel
import Control.Parallel.Strategies

parMap rseq (*2) [1..100]

