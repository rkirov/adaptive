
filt f [] = []
filt f (x:xs) = if (f x)
                then x : (filt f xs)
                else filt f xs

qsort :: (Num a, Ord a) => [a] -> [a]
qsort x = qsort' x [] 

qsort' :: (Num a, Ord a) => [a] -> [a] -> [a]
qsort' [] t = t
qsort' (x : xs) tail = let
  l = filt (< x) xs
  h = filt (>= x) xs
  in 
  qsort' l (x : (qsort' h tail))

main = do
  putStrLn $ show $ qsort [2,4,5,1,6]
