type Birds = Int  
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  

landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

banana :: Pole -> Maybe Pole
banana _ = Nothing

printPole :: Pole -> String
printPole (left, right) = show left ++ " on the left and " ++ show right ++ " on the right."


-- Various ways of printing Maybe Poles
imbalanced = "Imbalanced!"

printMaybePole1 :: Maybe Pole -> String
printMaybePole1 (Just pole) = printPole pole
printMaybePole1 Nothing = imbalanced

printMaybePole2 :: Maybe Pole -> String
printMaybePole2 pole = maybe imbalanced printPole pole

printMaybePole3 :: Maybe Pole -> String
printMaybePole3 pole = 
  case fmap printPole pole of
    Just poleString -> poleString
    Nothing         -> imbalanced
   

main = do
  let balanced = return (0,0) >>= landLeft 1 >>= landRight 4
  let imbalanced1 = return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2) 

  let imbalanced2 = return (0, 1) >>= banana

  putStrLn $ printMaybePole1 balanced
  putStrLn $ printMaybePole2 imbalanced1
  putStrLn $ printMaybePole2 imbalanced2
  putStrLn $ printMaybePole3 balanced





