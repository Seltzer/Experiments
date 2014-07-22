module lists
(
  bisectr
) where
    
bisectr :: [a] -> ([a], a, [a])
bisectr x = 
  let middle = div (length x) 2 
  in (take middle x, x !! middle, drop (middle + 1) x)





