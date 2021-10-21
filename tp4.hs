   type Triplet =(Int,[Char],Bool)
      first :: Triplet ->Int
      first (a,_,_) = a
      second :: Triplet -> [Char]
      second (_,xs,_) = xs
      third :: Triplet -> Bool
      third (_,_,True) = True

type TripletPoly a b c = (a,b,c)
first'::TripletPoly a b c->a
first' (a,_,_) = a     
second' :: TripletPoly a b c -> b
second' (_,a,_) = a
third' :: TripletPoly a b c -> c
third' (_,_,a) = a
data MyNum = MyInt Int | MyFloat Float deriving (show)
square:: MyNum->MyNum
square (MyInt a) = (MyInt (a*a))
square (MyFloat b) = (MyFloat (b*b))

data List a = Nil | Cons {head::a, tail::List} deriving(Eq)
fromList::[a] -> List a
fromList [] = Nil
fromList (x:xs) =x`Cons`fromList xs
class MyEq a where
test::a->a->Bool
instance MyEq (List a) where
         test _ _ = True

instance (show a)=> show (List a) where
  	show Nil  = "Nil"
  	show (Cons x Nil) ="Cons"++ show x ++"Nil"
  	show (Cons x xs) = "Cons"++ show x"("++ show ys ++")"  