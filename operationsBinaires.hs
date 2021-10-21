module OperationsBinaires where
add::(Num a)=>a->a->a
add x y = x + y
incrementer :: Int -> Int
incrementer = add 1
evaluerOpertionBinaire :: (a->b->c)->a->b->c
evaluerOpertionBinaire f x y = x `f` y
-- evaluerOperationBinaire(||)::Bool->Bool->Bool
--evaluerOpertionBinaire add::(Num a)=>a->a->a
--evaluerOperationBinaire(!!)::[a]->Int->a
incDec :: (Num a)=>Bool->a->a
incDec x y = if x then (y+1) else (y-1)
incDecc::(Num a)=>Bool->a->a
incDecc x y = if x then (add y 1) else(add y(-1))
incPairDecImpair::(Integral a)=>Bool->a
incPairDecImpair y = if even y then (y+1) else (y-1)
incPairDeccImpair::(Integral a )=>Bool->a
incPairDeccImpair x = if even x then (add x 1) else (add x (-1))
