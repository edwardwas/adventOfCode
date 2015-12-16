import Day06
import Day11

import Control.Monad


main = helper "hxbxwxba"
    where helper i = do
            let x = partA i
            print x
            helper $ increment x
