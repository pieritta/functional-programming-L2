data List a = Nil | Cons a (List a)
 instance (Eq a)=>Eq [a] where
 	[] == [] = True
 	[] == (y:ys) = False
 	(x:xs) == [] = False
 	(x:xs) == (y:ys) = (x==y) && (xs==ys)
 	null :: [a]->Bool
 	null [] = True
 	null (x:xs) = False
 	(++)::[a]->[a]->[a]
 	[] ++ ys = ys
 	(x:xs) ++ ys = x:(xs ++ ys)
 	flattern :: [[a]]->[a]
 	flattern [[]] = []
 	flattern xs:xss = xs ++ flattern xss
 	reverse :: [a]->[a]
 	reverse [] = []
 	reverse (x:xs) = reverse xs ++ [x]
 	revacc:: [a]->[a]->[a]
 	revacc [] as = as
 	revacc (x:xs) as = revacc xs (x:as)
 	length :: [a]->Int
 	length [] = 0
 	length (x:xs) = 1 + length xs
 	head::[a]->a
 	head x:xs = x
 	tail :: [a]->[a]
 	tail (x:xs) = xs
 	last :: [a]->a
 	last  = head . reverse 
 	init:: [a]->[a]
    init = reverse . tail . reverse
    init:: [a]->[a]
    init (x:xs) = if null xs then [] else x:init xs 
    take :: Int->[a]->[a]
    take 0 xs = []
    take (n+1) [] = []
    take (n+1) (x:xs) = x:take n xs
    drop::Int->[a]->[a]
    drop 0 xs = []
    drop (n+1) [] = []
    drop (n+1) (x:xs) = drop n xs
    (!!):: [a]->Int->a
    (x:xs) !! 0 = x
    (x:xs) !! (n + 1) = xs !! n

