meliMelo :: (Integral a) => [a] -> ([a],[a])
meliMelo [] = ([],[])
meliMelo xs 
        | lenght xs `mod` 2 == 0 = take(lenght xs`div`2 xs) , reverse (drop(lenght xs`div`2 xs))
        | Otherwise = take(lenght xs+1 `div`2 xs) , reverse (drop(lenght xs+1 `div`2 xs))