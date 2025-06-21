module ListSeq where

import Par ((|||))
import qualified Arr as A
import Arr (!)

instance Seq [] where
    emptyS = A.empty

    singletonS x = A.fromList [x]

    lengthS = length

    nthS s i = s ! i

    tabulateS = A.tabulate

    mapS f s = A.tabulate (\i -> f (nth i s)) (lengthS s)

    filterS p s = let x = nthS 1 s
                      (px, fxs) = p x ||| filterS p xs
                  in case px of
                      True -> x : fxs -- ver como appendear
                      False -> fxs

    appendS s t = let (ls, lt) = lengthS s ||| lengthS t
                  in  A.tabulate (\i -> if i < ls then (nthS i s) else (nthS (i - ls) t)) (ls + lt)
