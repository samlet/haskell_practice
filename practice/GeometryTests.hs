-- 将 Geometry.hs 文件至于用到它的进程文件的同一目录之下.
-- 模块也可以按照分层的结构来组织，每个模块都可以含有多个子模块。而子模块还可以有自己的子模块。我们可以把 Geometry 分成三个子模块，而一个模块对应各自的图形对象.
-- 首先，建立一个 Geometry 文件夹，注意首字母要大写，在里面新建三个文件
module Main where

import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube

main = do putStrLn "Hello World"
          putStrLn (show (Sphere.volume 5.5) )
