--Tp2


module OperationsBinaires where
add::(Num a)=>a->a->a
add x y = x + y
incrementer :: Int -> Int
incrementer = add 1
incDecc::(Num a)=>Bool->a->a
incDecc x y = if x then (add y 1) else(add y(-1))
--incPairDecImpair::(Integral a)=>Bool->a
--incPairDecImpair y = if even y then (y+1) else (y-1)
--incPairDeccImpair::(Integral a )=>Bool->a
--incPairDeccImpair x = if even x then (incrementer x 1) else (incrementer x (-1))
 --Tp4
 let liste = [0..9]
 
 --map :: (a->b)->[a]->[b]
 --map f [] = []
 --map f (x:xs) = f x:map f xs


--unzipp::[(a,b)]->([a],[b])
--unzipp [] = ([],[])
--unzipp xs = (a,b)
  --       where
    --     	a= map (fst xs)
      --   	b= map (snd xs)
      --tp4
      type Triplet =(Int,[Char],Bool)
      first::Triplet->Int
      first (a,_,_) = a
      second::Triplet->[Char]
      second (_,xs,_) = xs
      third :: Triplet -> Bool
      third (_,_,True) = True
      

