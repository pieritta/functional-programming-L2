data List a = Nil | Cons {head::a,tail::List a}
            deriving(Show,Eq)

instance  (Show a) =>Show (List a) where
  show Nil = "Nil"
  show (Cons x Nil) = "Cons " ++ show x ++ " Nil"
  show (Cons y ys) = "Cons "++show y ++"(" ++ show ys ++")" 
data NBC = NBC {partReel::Float , partIm::Float} deriving(Eq,Show)
instance Show NBC => Show(NBC) where
	show (NBC a b) =  "(" ++ show a ++ "," ++ show b ++ "i" ++ ")"
instance Ord NBC where
	(NBC a b) > (NBC c d) = (a*a + b*b)>(c*c + d*d)
	(NBC a b) < (NBC c d) = (a*a + b*b)<(c*c + d*d)

	--(NBC a b) /= (NBC c d) = (a*a + b*b)\=(c*c + d*d)
	--(NBC a b) == (NBC c d) = (a*a + b*b)==(c*c + d*d)
somme:: NBC ->NBC ->NBC
somme (NBC a b) (NBC c d) = (NBC (a+c)  (b+d))
produit:: NBC ->NBC ->NBC
produit (NBC a b) (NBC c d) = (NBC (a*c - b*d)  (a*d + b*c))
sommee:: List NBC->NBC
sommee Nil = (NBC 0 0)
sommee (Cons x xs) = somme x(sommee xs)
filter::(a->Bool)=>List a->List a
imgPur::List NBC->List NBC
imgPur Nil = Nil
imgPur = filter im 
      where 
      	 im (NBC a _) = False
      	 	im (NBC 0 _) = True
carree::List NBC -> List NBC
carree Nil = Nil
carree (Cons s ss) = (Cons carre s(carree ss))
           where 
           	carre (NBC a b) = if(a<0) then (NBC (a*a - b*b)((-2)*a*b))
           		               else (NBC (a*a - b*b)(2*a*b))     
