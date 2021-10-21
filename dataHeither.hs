data Either = left Bool | right Char deriving(Eq,Ord)
left::Bool->Either
right::Char->Either
data Either a b = left a | right b deriving(Eq,Ord)
left::a->Either a b
right::b->Either a b
case :: (a->c, b->c) -> Either -> c
case (f,g) (left x) = f x
case (f,g) (right y) = g y
instance (Eq a, Eq b)=> Eq(Either a b) where
	left x == left y = (x==y)
	left x == right y = False
	right x == left y = False
	right x == left y = (x==y)
instance (Ord a, Ord b)=> Ord(Either a b) where
	left x < left y = (x<y)
	left x < right y = True
	right x < left y = False
	right x == left y =(x<y)
