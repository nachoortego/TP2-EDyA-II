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

    tabulateS f 0 = emptyS
    tabulateS f n = 
        tabulateS' f 0
        where 
            tabulateS' f i | i == (n-1) = [f i]
                           | otherwise = 
                                let (fi, tab) = f i ||| tabulateS' f (i+1)
                                in fi : tab
    
    -- W_scan: O(c1_let + sum(i = 0, n-1, W(f_i) + W_map(n-1)) 
    -- S_scan: S(c1_let + max(f_0, f_1, ..., f_n-1)) 
    mapS f [] = []
    mapS f (x:xs) = 
        let (fx, fxs) = f x ||| mapS f xs
        in fx : fxs

    filterS p [] = []
    filterS p (x:xs) = let (px, fxs) = p x ||| filterS p xs
                       in case px of
                            True -> x : fxs
                            False -> fxs
    
    appendS xs ys = xs ++ ys

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
    

    -- W_scan: O(c1_let + c2_div + W_lengthS + W_contract + W_scan(n/2) + W_tabulate(n))
    -- S_scan: O(c1_let + c2_div + S_lengthS + S_contract + S_scan(n/2) + S_tabulate(n))
    scanS f e []  = (emptyS, e)
    scanS f e [x] = (singletonS e, f e x)
    scanS f e xs  = let h = div (lengthS xs) 2
                        sc = contract xs h f
                        (s', st) = scanS f e sc
                        r = tabulateS (expand f xs s') (lengthS xs)
                    in
                        (r, st)

    fromList = id
 
    -- W_reduce: O(c1_let + W_contract(n) + W_reduce(⌊n/2⌋))
    -- S_reduce: O(c1_let + S_contract(n) + S_reduce(⌊n/2⌋))
    reduceS f e [] = e
    reduceS f e [x] = f e x 
    reduceS f e (xs) = let xs' = contractR f xs 
                           in case xs' of 
                            [a] -> f e a 
                            _    -> reduceS f e xs'

-- W_contract: O(c1_let + sum(i = 0, ⌊n/2⌋, W(f_i) + W_contract(n-2) + c2_(:))
-- S_contract: O(c1_let + max(f_0, f_1, ..., f_⌈n/2⌉) + S_contract(n-2) + c2_(:))
contractR f [] = []
contractR f [x] = [x]
contractR f (x:y:xs) = let (fxy, contr) = (f x y) ||| (contractR f xs)
                       in fxy:contr

-- W_f'(c1_let + 2*W_nthS(n) + W_f)
-- W_contract(c1_let + W_lengthS(n) + c2_mod + c3_/= + W_tabulateS(f') + c4_singleton + W_nthS(n) + W_append(n/2))
-- S_f'(c1_let + S_nthS(n))
-- S_contract(c1_let + S_lengthS(n) + c2_mod + c3_/= + max(S_tabulateS(f'), c4_singleton + S_nthS(n)) + S_append(n/2))
contract :: Seq s => s a -> Int -> (a -> a -> a) -> s a
contract xs h f = let 
                    l = lengthS xs
                    f' i = let (nth0, nth1) = (nthS xs (i*2)) ||| (nthS xs (i*2+1))  
                           in f nth0 nth1 
                  in 
                    if mod l 2 /= 0
                    then
                        let (tab, sing) = (tabulateS f' h) ||| (singletonS (nthS xs (l - 1)))  
                        in appendS tab sing
                    else tabulateS f' h

-- W_expand (c1_let + c2_div + c3_mod + 2*W_nthS(n) + W_f)
-- S_expand (c1_let + c2_div + c3_mod + S_nthS(n) + S_f)
expand :: Seq s => (a -> a ->a) -> s a -> s a -> Int -> a
expand f s s' i =  let i' = div i 2
                   in
                    if mod i 2 == 0
                    then nthS s' i'
                    else let (nth0, nth1) = (nthS s' i') ||| (nthS s (i-1))  
                         in f nth0 nth1


l = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14" , "15"]
ln = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 , 15]

asd = reduceS op "b" l
  where op x y = concat ["(", x, "+", y,")"]

zxc = scanS op "b" ["1", "2", "3", "4", "5"]
  where op x y = concat ["(", x, "+", y,")"]


qwe = contract [1] 0 (+)
