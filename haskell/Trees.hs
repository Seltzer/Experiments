module Trees
(
  Tree(..),
  Crumb(..),
  Breadcrumbs,
  TreeZipper,
  Direction(..),
  Directions,
  buildTree,
  treeBalance,
  treeInsert,
  treeRemove,
  treeFind,
  singleton,
  nodeCount,
  modify,
  navigate,
  goLeft,
  goRight

) where

import Core
import Lists
import Control.Applicative
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
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show, Eq)
type Breadcrumbs a = [Crumb a]
type TreeZipper a = (Tree a, Breadcrumbs a)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Returns passed in tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a l r)
  | x == a = Node a l r
  | x < a =  Node a (treeInsert x l) r
  | x > a =  Node a l (treeInsert x r)

treeRemove :: (Ord a) => a -> Tree a -> Tree a
treeRemove _ EmptyTree = EmptyTree
treeRemove x (Node a l r)
  | x == a = EmptyTree
  | x < a = Node a (treeRemove x l) r
  | x > a = Node a l (treeRemove x r)

treeFind :: (Ord a) => a -> TreeZipper a -> Maybe(TreeZipper a)
treeFind _ (EmptyTree, _) = Nothing
treeFind x z@(Node a l r, bs) 
  | x == a = Just z
  | x < a = (goLeft z) >>= treeFind x
  | x > a = (goRight z) >>= treeFind x

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



goLeft :: TreeZipper a -> Maybe(TreeZipper a)
goLeft (EmptyTree, _) = Nothing
goLeft (Node _ EmptyTree _, _) = Nothing
goLeft (Node a l r, br) = Just (l, LeftCrumb a r:br)


goRight :: TreeZipper a -> Maybe(TreeZipper a)
goRight (EmptyTree, _) = Nothing
goRight (Node _ _ EmptyTree, _) = Nothing
goRight (Node a l r, br) = Just (r, RightCrumb a l:br)

goUp :: TreeZipper a -> Maybe(TreeZipper a)
goUp (EmptyTree, _) = Nothing
goUp (tree, LeftCrumb a r:br) = Just (Node a tree r, br)
goUp (tree, RightCrumb a l:br) = Just (Node a l tree, br)


-- Private
prt :: (Show a) => Tree a -> IO()
prt = putStrLn . prettyPrint

prt2 :: Maybe String -> IO()
prt2 Nothing = putStrLn "Went off the edge of the tree/universe"
prt2 (Just x) = putStrLn x

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

  prt2 $ prettyPrint <$> fst <$> (Just (treeBalance coolTree, []) >>= goRight >>= goLeft)

  prt2 $ prettyPrint <$> fst <$> (Just (treeBalance coolTree, []) >>= goRight >>= goLeft >>= goLeft >>= goLeft)







 
