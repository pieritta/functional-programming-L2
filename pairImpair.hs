pairImpair :: [Int] -> ([Int],[Int])
pairImpair [] = ([],[])
pairImpair xs = (a,b)
               where
               	a = filter even xs
               	b = filter odd xs