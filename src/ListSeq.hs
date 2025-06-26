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
    
    scanS f e []  = (emptyS, e)
    scanS f e [x] = (singletonS e, f e x)
    scanS f e xs  = let h = div (lengthS xs) 2
                        sc = contract xs h f
                        (s', st) = scanS f e sc
                        r = tabulateS (expand f xs s') (lengthS xs)
                    in
                        (r, st)

    fromList = id
 
    reduceS f e [] = e
    reduceS f e [x] = f e x 
    reduceS f e (xs) = let xs' = contractR f xs 
                           in case xs' of 
                            [a] -> f e a 
                            _    -> reduceS f e xs'

contractR f [] = []
contractR f [x] = [x]
contractR f (x:y:xs) = (f x y) : (contractR f xs)


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


l = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14" , "15"]
ln = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 , 15]

asd = reduceS op "b" l
  where op x y = concat ["(", x, "+", y,")"]

zxc = scanS op "b" ["1", "2", "3", "4", "5"]
  where op x y = concat ["(", x, "+", y,")"]


qwe = contract [1] 0 (+)
