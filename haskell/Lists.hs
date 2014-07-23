module Lists
(
  bisect
) where
    
bisect :: [a] -> ([a], a, [a])
bisect x = 
  let middle = div (length x) 2 
  in (take middle x, x !! middle, drop (middle + 1) x)





