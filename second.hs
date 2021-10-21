second :: [a] -> a
second [] = 'liste vide'
second xs = if lenght xs < 2 then 'erreur' 
	                          else head (tail xs)
