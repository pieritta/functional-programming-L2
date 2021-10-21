
data List a = Nil | Cons {head_::a,tail_::List a} deriving(Eq,Show)
--Cons 1(Cons 8(Cons 9(Cons 3 Nil)))

data Cercle = Cercle{abs::Float,ord::Float,rayonn::Float}
             deriving(Show)
data Rectangle = Rectangle{long::Float,larg::Float}
               deriving(Show)
data Carre = Carre{cote::Float}
            deriving(Show)

class FigureGeometrique a where
 surface::a->Float
 perimetre::a->Float

instance FigureGeometrique Cercle where
 surface (Cercle _ _ b) = b*b*3.14 
 perimetre (Cercle _ _ c) = 2*3.14*c

instance FigureGeometrique Rectangle where
 surface (Rectangle a b) = a*b 
 perimetre (Rectangle c d) = (c+d)*2

instance FigureGeometrique Carre where
 surface (Carre a) = a*a
 perimetre (Carre c) = 4*c


map'::(a->a)->List a->List a
map' f Nil = Nil
map' f (Cons a as) = f a `Cons` (map' f as)


evaluerSurface::(FigureGeometrique a) =>List a ->List Float
evaluerSurface (Cons a as)= map' surface a `Cons` evaluerSurface as

evaluer::(a->b->c)->a->b->c
evaluer f x y = x`f`y

