module ListSeq where

import Par ((|||))
import qualified Arr as A
import Arr (!)

instance Seq [] where
    emptyS = A.empty

    singletonS x = A.fromList [x]

    lengthS = A.length

    nthS s i = s ! i

    tabulateS = A.tabulate

    mapS f s = A.tabulate (\i -> f (nthS i s)) (lengthS s)

    filterS p s | length s == 1 = let x = nthS s 0
                                  in case p x of
                                       True -> x
                                       False -> empty
                | otherwise = let h = div (length s) 2
                                  (fl, fr) = filterS (take h s) ||| filterS (drop h s)
                                  in append fl fr 

    appendS s t = let (ls, lt) = lengthS s ||| lengthS t
                  in  A.tabulate (\i -> if i < ls then (nthS i s) else (nthS (i - ls) t)) (ls + lt)

    takeS s n = A.subArray 0 n s

    dropS s n = A.subArray (lengthS s - n) (lengthS s) s
