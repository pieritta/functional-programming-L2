factoriel::(Integral a) => a -> a
factoriel 0 = 1
factoriel x 
             | x==1 = 1
             | otherwise = x*factoriel(x-1)
           

combinaison::(Integral a) => a -> a -> a
combinaison n p = a`div`b
             where 
              a = factoriel p
              b = factoriel(n)*factoriel(p-n) 