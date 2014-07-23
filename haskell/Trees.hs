module Trees
(
  Tree
) where

import Core
import Lists
import qualified Data.List as DList
import qualified Data.Tree as DTree
import qualified Data.Sequence as DSeq


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r) 

-- FIXME
instance Monad Tree where
  return x = singleton x
  EmptyTree >>= f = EmptyTree
  (Node a _ _) >>= f = f a  
  fail _ = EmptyTree


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a l r)
  | x == a = Node a l r
  | x < a =  Node a (treeInsert x l) r
  | x > a =  Node a l (treeInsert x r)

treeRemove :: (Ord a) => a -> Tree a -> Tree a
treeRemove x EmptyTree = EmptyTree
treeRemove x tree = tree
  --treeRemove x (Node a l r)
  -- | x == a = Node 

treeElement :: (Ord a) => a -> Tree a -> Bool
treeElement _ EmptyTree = False
treeElement x (Node a l r) 
  | x == a = True
  | x < a  = treeElement x l
  | x > a  = treeElement x r

treeDepth :: Tree a -> Int
treeDepth EmptyTree = 0
treeDepth (Node a l r) = 1 + (maximum $ map treeDepth [l, r])

toDTree :: Tree a -> DTree.Tree a
toDTree (Node a EmptyTree EmptyTree) = DTree.Node a []
toDTree (Node a EmptyTree r) = DTree.Node a [toDTree r]
toDTree (Node a l EmptyTree) = DTree.Node a [toDTree l]
toDTree (Node a l r)  = DTree.Node a (map toDTree [l, r])
  
prettyPrint :: (Show a) => Tree a -> String
prettyPrint = DTree.drawTree . toDTree . fmap show

buildTree :: (Ord a) => [a] -> Tree a
buildTree = foldr treeInsert EmptyTree

getTreeValuesPreOrder :: (Ord a) => Tree a -> [a]
getTreeValuesPreOrder EmptyTree = []
getTreeValuesPreOrder (Node a l r) = 
  getTreeValuesPreOrder l ++ [a] ++ getTreeValuesPreOrder r

treeBalance :: (Ord a) => Tree a -> Tree a
treeBalance EmptyTree = EmptyTree
treeBalance x = (buildTree . reverse. reorder . getTreeValuesPreOrder) x
  where 
    reorder [] = []
    reorder x = let (leading, middle, trailing) = bisect x in [middle] ++ (reorder leading) ++ (reorder trailing)

nodeCount :: Tree a -> Int
nodeCount EmptyTree    = 0
nodeCount (Node a l r) = 1 + nodeCount l + nodeCount r


data Direction = L | R deriving (Show)
type Directions = [Direction]

navigate :: Tree a -> Directions -> Maybe (Tree a)
navigate EmptyTree _ = Nothing
navigate tree [] = Just tree
navigate (Node _ l _) (L:remaining) = navigate l remaining
navigate (Node _ _ r) (R:remaining) = navigate r remaining


type Breadcrumbs a = [Crumb a]
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show

type TreeZipper a = (Tree a, Breadcrumbs a)

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node a l r, br) = (l, LeftCrumb a r:br)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node a l r, br) = (r, RightCrumb a l:br)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (tree, LeftCrumb a r:br) = (Node a tree r, br)
goUp (tree, RightCrumb a l:br) = (Node a l tree, br)






main = do
  let coolTree = buildTree [1, 7, 4, 9, 15, 10, 13, 22, 2] 

  putStrLn ("Depth: " ++ (show $ treeDepth coolTree))
  putStrLn (DList.intercalate " " $ map show $ getTreeValuesPreOrder coolTree)

  putStrLn (show coolTree)
  putStrLn $ prettyPrint coolTree

  putStrLn "Balancing"
  (putStrLn . prettyPrint . treeBalance) coolTree

  putStrLn $ show $ treeElement 999 coolTree
  putStrLn $ show $ treeElement 7 coolTree

  putStrLn $ (maybe "Not found" prettyPrint (navigate coolTree [L, L, L]))
  putStrLn $ (maybe "Not found" prettyPrint (navigate coolTree [R, L, L]))

  putStrLn . prettyPrint $ fst $ (treeBalance coolTree, []) -: goRight -: goLeft





