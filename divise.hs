divise :: (Integral a) => [a] -> ([a],[a])
divise [] = ([],[])
divise xs 
        | lenght xs `mod` 2 == 0 = take(lenght xs`div`2 xs) , drop(lenght xs`div`2 xs)
        | otherwise = take(lenght xs+1 `div`2 xs) , drop(lenght xs+1 `div`2 xs)