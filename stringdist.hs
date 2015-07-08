module StringDist where

main :: IO ()
main = do
        putStrLn $ show $ fstringdist "abcde" "abdcde" "jac"

stringdist :: [Char] ->[Char] ->[Char]->Int
stringdist s t "lev" = lev s (length s) t (length t)
stringdist s t "ham" | (length s) == (length t) = ham s t
                     | otherwise = error "Can't compute Hamming dist for strings of unequal lengths"
stringdist s t "dlev" = dlev s (length s) t (length t)
stringdist s t "ndist" = ndist 2 s t

fstringdist :: [Char] ->[Char] ->[Char]->Float
fstringdist s t "jac" = jac 2 s t

--N-grams
ndist :: Int ->[Char] ->[Char] ->Int
ndist n s t = length $ filter (`elem` (ngram n s)) (ngram n t)

ngram :: Int ->[Char] ->[[Char]]
ngram n s | (length s >= n) = take n s : ngram n (tail s)
          | otherwise = []

-- Jaccard distance
jac :: Int ->[Char] ->[Char] ->Float
jac n [] t = 1.0
jac n s t = 1.0 - ((fromIntegral (ndist n s t))/(fromIntegral (length (ncomb n s t))))

ncomb :: Int ->[Char] ->[Char] ->[[Char]]
ncomb n s t = zipWith (not . elem) (ngram n s) (ngram n t)


-- Hamming distance
ham :: [Char] ->[Char] ->Int
ham s t = length $ filter not (zipWith (==) s t)

-- Damerau-Levenshtein distance
dlev :: [Char] ->Int ->[Char] ->Int ->Int
dlev s lens t lent | min lens lent == 0 = max lens lent
                   | lenc && si && sj = min4 dlev1 dlev2 dlev3 dlev4
                   | otherwise = min3 dlev1 dlev2 dlev3
                      where lenc = lens>1 && lent>1
                            si = s!!(lens-1)==t!!(lens-2)
                            sj = s!!(lens-2)==t!!(lent-1)
                            dlev1 = 1 + dlev s (lens-1) t lent
                            dlev2 = 1 + dlev s (lens-1) t (lent-1)
                            dlev3 = cost ss tt + dlev s (lens-1) t (lent-1)
                            dlev4 = 1 + dlev s (lens-2) t (lent-2)
                            ss = s !! (lens-1)
                            tt = t !! (lent-1)

-- Levenshtein distance
lev :: [Char] ->Int ->[Char] ->Int ->Int
lev _ 0 _ _ = 0
lev _ _ _ 0 = 0
lev s lens t lent = min3 lev1 lev2 lev3
                      where lev1 = 1 + lev s (lens-1) t lent            --deletion
                            lev2 = 1 + lev s lens t (lent-1)            --insertion
                            lev3 = cost ss tt + lev s (lens-1) t (lent-1) --substitution
                            ss = s !! (lens-1)
                            tt = t !! (lent-1)


cost :: Char ->Char ->Int
cost s t | s == t = 0
         | otherwise = 1

min3 :: Int -> Int -> Int -> Int
min3 a b c | a <= b = min a c
           | a <= c = min a b
           | otherwise = min b c

min4 :: Int ->Int ->Int ->Int ->Int
min4 a b c d | a <= b = min3 a c d
             | a <= c = min3 a b d
             | a <= d = min3 a c b
             | otherwise = min3 b c d

