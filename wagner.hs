module WagnerFischer where

main :: IO ()
main = do
        putStrLn $ show $ lev "kitten" 6 "sitting" 7
        putStrLn $ show $ wf "hello" 6 "hi" 2

-- Levenshtein distance
lev :: [Char] ->Int ->[Char] ->Int ->Int
lev s lens t lent | lens == 0 = lens
                  | lent == 0 = lent
                  | otherwise = min3 lev1 lev2 lev3
                      where lev1 = 1 + lev s (lens-1) t lent            --deletion
                            lev2 = 1 + lev s lens t (lent-1)            --insertion
                            lev3 = cost ss tt + lev s (lens-1) t (lent-1) --substitution
                            ss = s !! (lens-1)
                            tt = t !! (lent -1)

-- Werner-Fischer algorithm
wf :: [Char] ->Int ->[Char] ->Int ->[[Int]]
wf s lens t lent = matrix lens lent 0

matrix :: Int ->Int ->a ->[[a]]
matrix n m = replicate m . replicate n


cost :: Char ->Char ->Int
cost s t | s == t = 0
         | otherwise = 1

min3 :: Int -> Int -> Int -> Int
min3 a b c | a <= b = min a c
           | a <= c = min a b
           | otherwise = min b c

