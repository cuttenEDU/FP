import System.IO

main = do
    handle <- openFile "file.txt" ReadMode
    contents <- hGetContents handle
    writeHandle <- openFile "fileOut.txt" WriteMode
    hPutStr writeHandle (sortFile contents)
    putStr $ sortFile contents
    hClose handle
    hClose writeHandle
    return()
    
sortFile :: String -> String
sortFile = unlines . quicksort . lines

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted