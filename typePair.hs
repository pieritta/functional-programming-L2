data Pair a b = MKPair a b deriving(Eq,Ord)
MKPair :: a -> b -> Pair a b
fst :: Pair a b -> a
fst MKPair x y = x
snd MKPair x y = y
instance (Eq a, Eq b) => Eq(a,b) where
	(x,y) == (u,v) = (x==u) && (y==v)
	instance (Ord a, Ord b) => Ord(a,b) where
	 		(x,y) < (u,v) = (x<u) || (x==u && y<v)