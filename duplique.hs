duplique::(Eq a)=> [a] -> [a]
duplique [] = []
duplique (x:y:xs) 
	          | x/=y = x:x:duplique xs
	          | otherwise = duplique xs