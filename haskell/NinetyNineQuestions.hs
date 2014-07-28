-- http://www.haskell.org/haskellwiki/99_questions/1_to_10
-- Just mucking around here learning a bit of Haskell and mostly ignoring error handling

import Data.List

-- 1.) Find the last element in a list
last'' :: (Show x) => [x] -> x
last'' [x] = x
last'' (_:xs) = last'' xs

last''' = head . reverse
last'''' = foldr1 (flip const) 
last''''' = foldl1 (curry snd)
last'''''' = foldr1 (flip $ curry fst)
last''''''' = head . foldl1 (>>) . map (:[])
last'''''''' x = x !! (length x - 1)


-- 2.) Find penultimate item in list
penultimate :: [x] -> x
penultimate [x, _] = x
penultimate (_:xs) = penultimate xs

penultimate' = last . init
penultimate'' x = reverse x !! 1
penultimate''' = head . tail . reverse
-- Clever hack taken from solutions
penultimate'''' = snd . (foldl (\ (a,b) c -> (c,a)) (e1, e2))
    where e1 = error "List too small!"
          e2 = error "List is null!"
penultimate''''' = (!! 1) . head . bunch 2 . reverse
	where bunch n l = (take n l) : (bunch n (drop n l))

	
-- 3.) Find the K'th element of a list. The first element in the list is number 1.	
kth :: Int -> [x] -> x
kth 1 (x:_) = x
kth k (_:xs) = kth (k-1) xs

kth' k = last . take (k - 1)
kth'' k = head . drop (k - 1)
kth''' k = last . head . dropWhile (\ l -> (length l) < k) . inits

			
-- 4.) Find the number of elements of a list
length' :: [x] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' = sum . map (const 1)
length''' = foldl (\n _ -> n + 1) 0
length'''' = fst . last . zip [1..]
			
		  
		  
main = do
  
  putStrLn ("oh hai")
