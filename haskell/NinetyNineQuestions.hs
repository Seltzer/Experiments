-- http://www.haskell.org/haskellwiki/99_questions/1_to_10
-- Just mucking around here learning a bit of Haskell and mostly ignoring error handling

import Data.List
import Control.Applicative
import Control.Monad
import Data.Maybe

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


-- 5.) Reverse a list
reverse' :: [x] -> [x]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' = foldl (flip (:)) []

		
-- 6.) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq x) => [x] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

isPalindrome' :: (Eq x) => [x] -> Bool
isPalindrome' xs = xs == (reverse xs)

isPalindrome'' :: (Eq x) => [x] -> Bool
isPalindrome'' xs = all (\ (a, b) -> a == b) $ zip xs (reverse xs)

isPalindrome''' :: (Eq x) => [x] -> Bool
isPalindrome''' xs = and $ zipWith (==) xs (reverse xs)

-- Applicative version where applicative functor type f = (->) [Int]
isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' = (==) <*> reverse 

-- Monad version where monad type m = (->) [Int]
-- liftM2 :: (a -> b -> c) -> m a -> m b -> m c
-- liftM2 specialised:: (Int -> Int -> Bool) -> ([Int] -> [Int]) -> ([Int] -> [Int]) -> ([Int] -> Bool)
isPalindrome''''' :: (Eq a) => [a] -> Bool
isPalindrome''''' = Control.Monad.liftM2 (==) id reverse


-- 7.) Flatten a nested list structure (e.g. concat)
-- flatten [[1, 2, 3, 4], [5, 6]]
flatten' :: [[a]] -> [a]
flatten' [[]] = []
flatten' [x] = x
flatten' (x:xs) = x ++ flatten'' xs

flatten'' = foldl (\r n -> r ++ n) [] 

-- 8.) Eliminate consecutive duplicates of list elements
-- compress "aaaabccaadeeee" returns "abcade"
compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' [x] = [x]
compress' (x:xs)  
	| x == second = rest
	| otherwise = x : rest
	where 
		second = head xs
		rest = compress' xs
		
compress'' [] = []
compress'' (x:xs) = x : (compress'' $ dropWhile (== x) xs)

-- These two are variants of each other... zip x with its tail to produce a binary mask which indicates whether we should discard values
compress''' x = map fst $ filter snd $ zip x $ ((:) True) $ zipWith (/=) x (tail x)
compress'''' xs = (++ [last xs]) $ catMaybes $ zipWith (\a b -> if (a == b) then Nothing else Just a) xs (tail xs)

compress''''' x = map head $ group x
compress'''''' x = foldr (\a b -> if a == (head b) then b else a:b) [last x] x


-- 8.) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- 		["aaaa","b","cc","aa","d","eeee"]
pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x : a) : pack' b
	where (a, b) = span (==x) xs

pack'' :: (Eq a) => [a] -> [[a]]
pack'' [] = []
pack'' [x] = [[x]]
pack'' (x:xs) = if x `elem` (head (pack'' xs)) then (x:(head (pack'' xs))):(tail (pack'' xs)) else [x]:(pack'' xs)


		  
main = do
  
  putStrLn ("oh hai")
