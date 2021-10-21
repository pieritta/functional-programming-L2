estImpair::(Integral a) => a -> Bool
estImpair x
         | x `mod` 2 /= 0 = True
         | otherwise = False