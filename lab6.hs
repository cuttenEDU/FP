count:: (Integer,Integer) -> Integer
count pair = let
              a = fst pair
              b = snd pair
              maxNumber = max a b
              minNumber = min a b
              diff = maxNumber - minNumber
              n = ceiling(fromInteger(diff)/fromInteger(minNumber))
            in
              if a == b then 1 else n + count (minNumber,(maxNumber - n*minNumber))
    
findN:: [(Integer,Integer)] -> [Integer]
findN lst = map count lst