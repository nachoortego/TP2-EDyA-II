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

    showtS s | length s == 0 = EMPTY
             | length s == 1 = ELT (nthS 0 s)
             | otherwise     = let hl = div (lengthS s) 2
                                   (l, r) = takeS s hl ||| dropS s hl
                               in NODE l r

    showlS s | length s == 0 = NIL
             | otherwise     = CONS (nthS 0 s) (drop 1 s)

    joinS ss = A.flatten ss

    reduceS f e xs = f e (reduceS' f (lengthS xs) xs)
    where
        reduceS' f l xs = case l of 
                                1 -> (nthS xs 0)
                                2 -> let (xs0, xs1) = (nthS xs 0) ||| (nthS xs 1)  
                                    in f xs0 xs1
                                n -> let n2 = 2 ^ (floor (logBase 2 (fromIntegral (n - 1))))
                                        l' = l - n2 
                                        l'' = n - l'
                                        (rl, rr) = (reduceS' f l' (takeS xs n2)) ||| (reduceS' f l'' (dropS xs l'))
                                    in f rl rr

    scanS f e xs | lengthS xs == 1 = (singletonS e, f e (nthS xs 0))
                 | otherwise = let  h = div (lengthS xs) 2
                                    sc = contract xs h f
                                    (s', st) = scanS f e sc
                                    r = tabulateS (expand f xs s') (lengthS xs)
                                in
                                    (r, st)

    fromList s = A.fromList


contract :: Seq s => s a -> Int -> (a -> a -> a) -> s a
contract xs h f = let 
                    l = lengthS xs
                    f' i = f (nthS xs (i*2)) (nthS xs (i*2+1))
                in 
                    if mod l 2 /= 0
                    then appendS (tabulateS f' h) (singletonS (nthS xs (l - 1))) 
                    else tabulateS f' h

expand :: Seq s => (a -> a ->a) -> s a -> s a -> Int -> a
expand f s s' i =  let i' = div i 2
                   in
                    if mod i 2 == 0
                    then nthS s' i'
                    else f (nthS s' i') (nthS s (i-1))

