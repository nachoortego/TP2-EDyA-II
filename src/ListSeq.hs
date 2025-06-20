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

    -- reduceS f e xs = case showtS xs of
    --                 EMPTY -> e
    --                 ELT x -> f e x
    --                 NODE l r -> 
    --                     let (ll, rr) = reduceS f e l ||| reduceS f e r
    --                     in f ll rr  

    -- reduceS f e xs = f e (reduceS' f xs)

    -- reduceS' f xs = case showtS xs of
    --                     EMPTY -> []
    --                     ELT x -> x
    --                     NODE l r -> 
    --                         let (ll, rr) = reduceS' f l ||| reduceS' f r
    --                         in f ll rr

    fromList xs = xs

asd = reduceS (+) 0 [1, 2, 3, 4]