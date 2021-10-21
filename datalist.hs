data List a = Nil | Cons {head::a, tail::List a} deriving(Eq)
fromList::[a] -> List a
fromList [] = Nil
fromList (x:xs) =x`Cons`fromList xs
class MyEq a where
--test::a->a->Bool
--instance MyEq (List a) where
  

instance (Show a)=> Show (List a) where
  	show Nil  = "Nil"
  	show (Cons x Nil) ="Cons" ++ show x ++ "Nil"
  	show (Cons x xs) = "Cons"++ show x ++"("++ show xs ++")"  
  