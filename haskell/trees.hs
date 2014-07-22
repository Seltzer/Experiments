module Trees
(
  Tree
) where

import qualified Data.List as DList
import qualified Data.Tree as DTree
import qualified Data.Sequence as DSeq

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a l r)
  | x == a = Node a l r
  | x < a =  Node a (treeInsert x l) r
  | x > a =  Node a l (treeInsert x r)

treeElement :: (Ord a) => a -> Tree a -> Bool
treeElement _ EmptyTree = False
treeElement x (Node a l r) 
  | x == a = True
  | x < a  = treeElement x l
  | x > a  = treeElement x r

treeDepth :: Tree a -> Int
treeDepth EmptyTree = 0
treeDepth (Node a l r) = 1 + (maximum $ map treeDepth [l, r])

toDTree :: (a -> b) -> Tree a -> DTree.Tree b
toDTree mapFunc (Node a EmptyTree EmptyTree) = DTree.Node (mapFunc a) []
toDTree mapFunc (Node a EmptyTree r) = DTree.Node (mapFunc a) [toDTree mapFunc r]
toDTree mapFunc (Node a l EmptyTree) = DTree.Node (mapFunc a) [toDTree mapFunc l]
toDTree mapFunc (Node a l r)  = DTree.Node (mapFunc a) (map (toDTree mapFunc) [l, r])
  
prettyPrint :: (Show a) => Tree a -> String
prettyPrint = DTree.drawTree . toDTree show

buildTree :: (Ord a) => [a] -> Tree a
buildTree = foldr treeInsert EmptyTree

getTreeValuesPreOrder :: (Ord a) => Tree a -> [a]
getTreeValuesPreOrder EmptyTree = []
getTreeValuesPreOrder (Node a l r) = 
  getTreeValuesPreOrder l ++ [a] ++ getTreeValuesPreOrder r

treeBalance :: (Ord a) => Tree a -> Tree a
treeBalance EmptyTree = EmptyTree
treeBalance x = x
  where middle x = let x' = DSeq.fromList x in DSeq.index x' $ (div (DSeq.length x') 2)

main = do
  let coolTree = buildTree [1, 7, 4, 9, 10, 13, 22, 2] 
  
--  putStrLn $ show $ middle [1, 2, 3]
--  putStrLn $ show $ middle [1, 2, 3, 4]

  -- Ugly
  putStrLn ("Depth: " ++ (show $ treeDepth coolTree))
  putStrLn (DList.intercalate " " $ map show $ getTreeValuesPreOrder coolTree)

  putStrLn (show coolTree)
  putStrLn $ prettyPrint coolTree

  putStrLn $ show $ treeElement 999 coolTree
  putStrLn $ show $ treeElement 7 coolTree









