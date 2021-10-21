(a,b).(c,d)=a*c+b*d
produit::(Int,Int)->(Int,Int)->Int
produit (a,b) (c,d)= a*c+b*d
affiche::(Int,Int)->(Int,Int)->[Char]
affiche (a,b) (c,d)= "("++show a ++","++show b++")"++"("++show c ++","++show d++")"
scalair1::(Int,Int)->(Int,Int)->[Char]
scalair1 (a,b) (c,d) = affiche (a,b) (c,d) ++ "=" ++show (produit (a,b) (c,d))