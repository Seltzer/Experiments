blah :: [Int]
blah = do
	x1 <- [1, 2]
	x2 <- [10, 20, 30]

	return (x1 ++ x2)
	  

main = do
	let k = blah
  
	putStrLn . show $ head k
	putStrLn "hello"
