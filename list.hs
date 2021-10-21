data List a = Nil | Cons {head_::a,tail_::List a} deriving(Eq,Show)
--Cons 1(Cons 8(Cons 9(Cons 3 Nil)))
instance Show List where
	show Nil = "("++show Nil++")"
	show (Cons a Nil) = "cons"++show a ++ "Nil"
	show (Cons a as) = "cons"++show a ++ "(" ++ show ys ++")" 

toList::[a]->List a
toList [] = Nil
toList (x:xs) = Cons x(toList xs)
fromList::List a->[a]
fromList Nil = []
fromList (Cons x xs) = x:fromList xs
--alist = let l = toList [1,2.3,4]
       --      t = Cons 8 (Cons 2 Nil) 
       --     in toList [t,l]
applyMap xs = [map (/2) elt | elt<-xs]