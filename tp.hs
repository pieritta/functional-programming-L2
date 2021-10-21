-- TP2
--quetion 1
module OperationsBinaires where

--quetion 2
add::(Num a)=>a->a->a
add x y = x + y

inc = add 1
--quetion 6
evaluerOperationBinaire::(a->b->c)->a->b->c
evaluerOperationBinaire f x y = x`f`y

--quetion 9
intDecc::(Num a)=>Bool->a->a
intDecc x y = if x then (y+1) else (y-1)

intDecc'::(Num a)=>Bool->a->a
intDecc' x y = if x then (add y 1) else (add y (-1))

dec::(Integral a)=>a->a
dec y = if even y then (add y 1) else (add y (-1))

dec'::(Integral a)=>a->a
dec' y = if even y then (y+1) else (y-1)
--quetion 7
--  evaluerOperationBinaire(||)::Bool->Bool->Bool
-- evaluerOperationBinaire add::(Num a)=>a->a->a
-- evaluerOperationBinaire(!!)::[a]->Int->a

----TP4--
--quetion 1
-- let liste1=[0..9]
-- let liste3 = [x | x<-[0..9]]
 --QUETION 2--
 -- let liste4=[x | x<-[1..10000],even x]
 -- let lsite5=[x | x<-[1..10000],x`mod`5==0]
 -- let liste6=[1..]

 --QUETION3--
 -- let liste7=[x*x | x<-[1..10000],even x]
 --QUETION 5--
 
unzip'::[(a,b)]->([a],[b])
unzip' [] = ([],[])
unzip' xs = (map fst xs,map snd xs)

--TP 4--
type Triplet = (Int,[Char],Bool) 
frst::Triplet->Int
frst(a,_,_)=a

second::Triplet->[Char]
second(_,a,_)= a

third::Triplet->Bool
third (_,_,a) = a

--QUETION 2--

type TripletPoly a b c = (a,b,c)

frst'::TripletPoly a b c ->a
frst' (a,_,_) = a

second'::TripletPoly a b c->b
second' (_,b,_) = b

third'::TripletPoly a b c ->c
third' (_,_,a) = a

--QUETION 3--

data MyNum = MyInt Int|MyFloat Float
           deriving(Show)

square::MyNum->MyNum
square (MyInt a) = (MyInt (a*a))
square (MyFloat b) = (MyFloat (b*b))

--QUETION 4--

data List a = Nil | Cons{head::a,tail::List a}
            deriving(Eq)

fromlist::[a]->List a
fromlist [] = Nil
fromlist (x:xs) = x`Cons`fromlist xs

--QUETION 5--
class MyEq a where
 test::a->a->Bool

instance MyEq (List a) where
 test _ _ = True

 --QUETION 6--

instance (Show a)=>Show (List a) where
  show Nil = "Nil"
  show (Cons x Nil) = "Cons "++ show x ++ "Nil"  
  show (Cons x ys) = "Cons "++show x ++"("++show ys ++ ")"


data Carre a= Carre a
             deriving(Show)

func::(Num a,Fractional b)=>(a,b)->a
func (x,y)=x