module StringDist where

main :: IO ()
main = do
        putStrLn $ show $ stringdist "haplo" "hello" "ham"

stringdist :: [Char] ->[Char] ->[Char]->Int
stringdist s t "lev" = lev s (length s) t (length t)
stringdist s t "ham" | (length s) == (length t) = ham s t
                     | otherwise = error "Can't compute Hamming dist for strings of unequal lengths"

-- Hamming distance
ham :: [Char] ->[Char] ->Int
ham s t = length $ filter not (zipWith (==) s t)

-- Damerau-Levenshtein distance

--

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


cost :: Char ->Char ->Int
cost s t | s == t = 0
         | otherwise = 1

min3 :: Int -> Int -> Int -> Int
min3 a b c | a <= b = min a c
           | a <= c = min a b
           | otherwise = min b c

