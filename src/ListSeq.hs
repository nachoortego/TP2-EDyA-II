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
    showtS xs = let hl = div (lengthS xs) 2
                    (l, r) = takeS xs hl ||| dropS xs hl
                in NODE l r

    showlS [] = NIL
    showlS (x:xs) = CONS x xs
    
    joinS [[]] = []
    joinS [xs] = xs
    joinS (xs:xss) = xs ++ (joinS xss)
    
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

    scanS f e xs = 

    fromList = id



    -- scanS f b s = (tabulate (λi → reduce f b (take s i)) length s, reduce f b s)




asd = reduceS (+) 2 [1, 2, 3, 4]