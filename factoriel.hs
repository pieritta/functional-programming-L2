factoriel::(Integral a) => a -> a
factoriel 0 = 1
factoriel x 
             | x==1 = 1
             | otherwise = x*factoriel(x-1)
           
