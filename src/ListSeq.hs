module ListSeq where

import Par ((|||))
import Seq

instance Seq [] where
    emptyS = []
    
    singletonS x = [x]
    
    lengthS = length

    nthS [] _ = error "Fuera de rango"
    nthS (x:xs) 0 = x 
    nthS (x:xs) n = nthS xs (n - 1)

    tabulateS f n = 
        tabulateS' f 0
        where 
            tabulateS' f i | i == n = [f i]
                           | otherwise = 
                                let (fi, tab) = f i ||| tabulateS' f (i+1)
                                in fi : tab

    mapS f (x:xs) = 
        let (fx, fxs) = f x ||| mapS f xs
        in fx : fxs

    filterS p [] = []
    filterS p (x:xs) = let (px, fxs) = p x ||| filterS p xs
                       in case px of
                            True -> x : fxs
                            False -> fxs
    
    appendS xs ys = xs ++ ys
    -- appendS = (++)

    takeS xs n = take n xs

    dropS xs n = drop n xs

    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS xs = let hl = div (length xs) 2
                    (l, r) = take hl xs ||| drop hl xs
                in NODE l r

    showlS [] = NIL
    showlS (x:xs) = CONS x xs
    
    joinS [[]] = []
    joinS [xs] = xs
    joinS (xs:xss) = xs ++ (joinS xss)

    data Tree a = Leaf a | Node (Tree a) (Tree a)

    toTree : Seq a -> Tree a
    toTree xs = let ls = length xs
                in toTree' xs
                where
                toTree' xs = case ls of
                            1 -> (Leaf s0)
                            n -> Node (toTree (take xs pp))
                            (toTree (drop xs pp))
                pp = 2 ** logBase 2 (n - 1)


    reduce f e xs = f e (reduceS' (length xs) f xs)

    reduceS' f l xs = case l of 
                            1 -> xs 
                            2 -> f (nth 0 xs) (nth 1 xs)
                            n -> let n2 = 2 ** logBase 2 (n - 1)
                                    l' = l - n2 
                                    l'' = n - l'
                                    (rl, rr) = (reduceS' f l' (take n2 xs)) ||| (reduceS' f l'' (drop l' xs))
                                    in f rl rr
                                

    reduceT f (Leaf x) = x
    reduceT f (Node l r) = let (rl, rr) = (reduceT f l) ||| (reduceT f r)
                           in f rl rr

    fromList xs = xs

    scanS f b s = (tabulate (λi → reduce f b (take s i)) length s, reduce f b s)

    -- reduceS f e xs = case showtS xs of
    --                 EMPTY -> e
    --                 ELT x -> f e x
    --                 NODE l r -> 
    --                     let (ll, rr) = reduceS f e l ||| reduceS f e r
    --                     in f ll rr  


    -- reduceS' f xs = case showtS xs of
    --                     EMPTY -> []
    --                     ELT x -> x
    --                     NODE l r -> 
    --                         let (ll, rr) = reduceS' f l ||| reduceS' f r
    --                         in f ll rr 




asd = reduceS (+) 0 [1, 2, 3, 4]