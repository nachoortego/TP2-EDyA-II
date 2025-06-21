module ListSeq where

import Par ((|||))
import qualified Arr as A
import Arr (!)

instance Seq [] where
    emptyS = A.empty

    singletonS x = A.fromList [x]

    