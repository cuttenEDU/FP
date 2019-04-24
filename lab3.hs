f :: Eq a => [a] -> Int -> Int -> [a]
f lst n i 
    | lst == [] = error "empty list!"
    | (n+i) < length lst = take (n+i-1) lst ++ drop (n+i) lst
    | (n+i) > length lst = error "no such element!"
    
