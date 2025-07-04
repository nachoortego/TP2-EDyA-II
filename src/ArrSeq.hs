module ArrSeq where

import Par ((|||))
import qualified Arr as A
import Seq

instance Seq A.Arr where
    emptyS = A.empty

    singletonS x = A.fromList [x]

    lengthS = A.length

    nthS s i = s A.! i

    tabulateS = A.tabulate

    mapS f s = A.tabulate (\i -> f (nthS s i)) (lengthS s)

    filterS p s | lengthS s == 0 = emptyS
                | lengthS s == 1 = let x = nthS s 0
                                   in case p x of
                                       True -> singletonS x
                                       False -> emptyS
                | otherwise = let h = div (lengthS s) 2
                                  (fl, fr) = filterS p (takeS s h) ||| filterS p (dropS s h)
                                  in appendS fl fr 

    appendS s t = let (ls, lt) = lengthS s ||| lengthS t
                  in  A.tabulate (\i -> if i < ls then (nthS s i) else (nthS t (i - ls))) (ls + lt)

    takeS s n = if n > (lengthS s)
                    then s
                    else A.subArray 0 n s

    dropS s n = if n > (lengthS s)
                    then emptyS
                    else A.subArray n (lengthS s - n) s

    showtS s | lengthS s == 0 = EMPTY
             | lengthS s == 1 = ELT (nthS s 0)
             | otherwise      = let hl = div (lengthS s) 2
                                   (l, r) = takeS s hl ||| dropS s hl
                                in NODE l r

    showlS s | lengthS s == 0 = NIL
             | otherwise      = CONS (nthS s 0) (dropS s 1)

    joinS = A.flatten

    reduceS f e xs = let l = lengthS xs
                     in if l == 0 
                        then e
                        else f e (reduceS' f l xs)
                     where reduceS' f l xs = case l of 
                                1 -> (nthS xs 0)
                                2 -> let (xs0, xs1) = (nthS xs 0) ||| (nthS xs 1)  
                                     in f xs0 xs1
                                n -> let n' = 2 ^ (floor (logBase 2 (fromIntegral (n - 1))))
                                         l' = l - n' 
                                         (rl, rr) = (reduceS' f n' (takeS xs n')) ||| (reduceS' f l' (dropS xs n'))
                                     in f rl rr

    scanS f e xs | lengthS xs == 0 = (emptyS, e)
                 | lengthS xs == 1 = (singletonS e, f e (nthS xs 0))
                 | otherwise = let  h = div (lengthS xs) 2
                                    sc = contract xs h f
                                    (s', st) = scanS f e sc
                                    r = tabulateS (expand f xs s') (lengthS xs)
                                in
                                    (r, st)

    fromList = A.fromList

contract :: Seq s => s a -> Int -> (a -> a -> a) -> s a
contract xs h f = let 
                    l = lengthS xs
                    f' i = f (nthS xs (i*2)) (nthS xs (i*2+1))
                in 
                    if mod l 2 /= 0
                    then
                        let (tab, sing) = (tabulateS f' h) ||| (singletonS (nthS xs (l - 1)))  
                        in appendS tab sing
                    else tabulateS f' h

expand :: Seq s => (a -> a ->a) -> s a -> s a -> Int -> a
expand f s s' i =  let i' = div i 2
                   in
                    if mod i 2 == 0
                    then nthS s' i'
                    else f (nthS s' i') (nthS s (i-1))


-- l = A.fromList ["1", "2", "3", "4", "5", "6", "7"]
l = A.fromList ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14" , "15"]
-- ln = fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 , 15]

asd = reduceS op "b" l
    where op x y = concat ["(", x, "+", y,")"]