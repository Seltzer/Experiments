module Trees
(
  Tree(..),
  treeInsert,
  singleton,
  nodeCount

) where

import Core
import Lists
import qualified Data.List as DList
import qualified Data.Tree as DTree
import qualified Data.Sequence as DSeq


-- Tree type
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


-- Crumbs keep track of the previous node's values plus the sub-tree not taken.
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show
type Breadcrumbs a = [Crumb a]
type TreeZipper a = (Tree a, Breadcrumbs a)


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


modify :: (a -> a) -> TreeZipper a -> TreeZipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs) 
modify f (EmptyTree, bs) = (EmptyTree, bs)


-- Tree navigation (old)
data Direction = L | R deriving (Show)
type Directions = [Direction]

navigate :: Tree a -> Directions -> Maybe (Tree a)
navigate EmptyTree _ = Nothing
navigate tree [] = Just tree
navigate (Node _ l _) (L:remaining) = navigate l remaining
navigate (Node _ _ r) (R:remaining) = navigate r remaining



goLeft :: TreeZipper a -> TreeZipper a
goLeft (Node a l r, br) = (l, LeftCrumb a r:br)

goRight :: TreeZipper a -> TreeZipper a
goRight (Node a l r, br) = (r, RightCrumb a l:br)

goUp :: TreeZipper a -> TreeZipper a
goUp (tree, LeftCrumb a r:br) = (Node a tree r, br)
goUp (tree, RightCrumb a l:br) = (Node a l tree, br)


-- Private
prt :: (Show a) => Tree a -> IO()
prt = putStrLn . prettyPrint


main = do
  let coolTree = buildTree [1, 7, 4, 9, 15, 10, 13, 22, 2] 

  putStrLn ("Depth: " ++ (show $ treeDepth coolTree))
  putStrLn (DList.intercalate " " $ map show $ getTreeValuesPreOrder coolTree)

  putStrLn (show coolTree)
  putStrLn $ prettyPrint coolTree

  putStrLn "Balancing"
  prt $ treeBalance coolTree

  putStrLn $ (maybe "Not found" prettyPrint (navigate coolTree [L, L, L]))
  putStrLn $ (maybe "Not found" prettyPrint (navigate coolTree [R, L, L]))

  putStrLn . prettyPrint $ fst $ (treeBalance coolTree, []) -: goRight -: goLeft






