import Data.List
import Data.Char
import System.Random

data State = State [Integer] [Integer]

getFrees :: State -> [Integer]
getFrees (State x y) = [1,2,3,4,5,6,7,8,9] \\ (x ++ y)

validStep :: Integer -> State -> Bool
validStep x (State a b) = notElem x (a ++ b)

play :: Integer -> State -> State
play x (State a b) = if length a == length b then State (a ++ [x]) b else State a (b ++ [x])

isWin :: State -> Bool
isWin (State a b) = let
                        p1 x = elem 1 x
                        p2 x = elem 2 x
                        p3 x = elem 3 x
                        p4 x = elem 4 x
                        p5 x = elem 5 x
                        p6 x = elem 6 x
                        p7 x = elem 7 x
                        p8 x = elem 8 x
                        p9 x = elem 9 x
                        check1 x = (p2 x && p3 x) || (p4 x && p7 x) || (p5 x && p9 x)
                        check2 x = p5 x && p8 x
                        check3 x = (p5 x && p7 x) || (p6 x && p9 x)
                        check4 x = p5 x && p6 x
                        check7 x = p8 x && p9 x
                        check x = if length x < 3 then False else
                            (p1 x && check1 x)
                                || (p2 x && check2 x)
                                    || (p3 x && check3 x)
                                        || (p4 x && check4 x)
                                            || (p7 x && check7 x)
                    in
                        if length a /= length b then check a else check b

score :: State -> Integer -> Integer
score (State a b) x = let
                        alttest _ _ [] acc = (-acc)
                        alttest x1 x2 (x:xs) acc = if isWin $ play x (State x1 x2) then alttest x1 x2 xs (acc + 1) else alttest x1 x2 xs acc 
                        test x1 x2 = if isWin (State x1 x2) then 1 else alttest x1 x2 (getFrees (State x1 x2)) 0
                      in  
                        if length a == length b then test (a ++ [x]) b else test a (b ++ [x])

bestSteps :: State -> [Integer]
bestSteps s = let
                test _ [] _ step = step
                test s (x:xs) max step = let
                                            scr = score s x
                                         in 
                                            if scr > max then test s xs scr [x] else if scr == max then test s xs max (step ++ [x]) else test s xs max step
             in
                test s (getFrees s) (-10) []

main = do
    putStr "\ESC[2J\ESC[H"
    putStrLn "Choose your symbol:\n1) X\n2) O"
    ch <- getLine
    game (State [] []) (digitToInt (ch!!0) == 2)
    
    

game :: State -> Bool -> IO()
game s ai = do
    if isWin s then do
        putStrLn (if ai then "You win!!!" else "AI win!!!")
        return ()
     else do
        if length (getFrees s) == 0 then do
            putStrLn "Game over! :("
            return ()
         else do
            if ai then do
                let steps = bestSteps s
                index <- randomRIO (0, length steps - 1)
                let s2 = play (steps!!index) s
                printField s2
                game s2 False
             else do
                printField s
                putStrLn "Choose position:"
                numIO <- getLine
                let num = toInteger $ digitToInt (numIO!!0)
                if validStep num s then do
                    let s2 = play num s
                    printField s2
                    game s2 True
                 else do
                    printField s
                    game s False

printField :: State -> IO()
printField (State a b) = do
    putStr "\ESC[2J\ESC[H"
    putStr (if elem 1 a then "X  " else if elem 1 b then "O  " else "1  ")
    putStr (if elem 2 a then "X  " else if elem 2 b then "O  " else "2  ")
    putStrLn (if elem 3 a then "X" else if elem 3 b then "O" else "3")
    putStr (if elem 4 a then "X  " else if elem 4 b then "O  " else "4  ")
    putStr (if elem 5 a then "X  " else if elem 5 b then "O  " else "5  ")
    putStrLn (if elem 6 a then "X" else if elem 6 b then "O" else "6")
    putStr (if elem 7 a then "X  " else if elem 7 b then "O  " else "7  ")
    putStr (if elem 8 a then "X  " else if elem 8 b then "O  " else "8  ")
    putStrLn (if elem 9 a then "X" else if elem 9 b then "O" else "9")