somme::(Ord a) => [a] -> ([a],[a])
somme [] = ([],[])
somme (x:y:xs) = 
     
     	 x + head (somme xs)
     	 y + head tail(somme xs)