maxListe::(Ord a)=>[a]->Maybe a
maxListe [] =  Nothing
maxListe [x] = Just x
maxListe (x:y:xs) 
                | x > y = maxListe (x:xs)
                | otherwise = maxListe (y:xs)
