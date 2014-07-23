import Test.HUnit
import Core
import Trees

value :: Tree a -> Maybe a
value EmptyTree = Nothing
value (Node x l r) = Just x


toTree :: TreeZipper a -> Tree a
toTree (t, _) = t


treeInsertTest = TestCase $
  do
    tree <- return (singleton 10 -: treeInsert 9 -: treeInsert 11)

    assertEqual "Check node count" 3 (nodeCount tree) 
    assertEqual "Check values" (Just 10) (value tree)
    assertEqual "Check values" (Node 10 (singleton 9) (singleton 11)) tree

treeRemoveTest = TestCase $
  do
    tree <- return $ treeBalance $ buildTree [1, 2, 3, 4, 5, 6, 7]

    assertEqual "remove non-existent" 7 (nodeCount $ treeRemove 300 tree) 
    assertEqual "remove whole tree" EmptyTree (treeRemove 4 tree) 
    assertEqual "remove leaf" 6 (nodeCount $ treeRemove 1 tree) 
    assertEqual "remove interior node" 4 (nodeCount $ treeRemove 2 tree) 
    
treeFindTest = TestCase $
  do
    tree <- return (singleton 10 -: treeInsert 9 -: treeInsert 11)

    assertEqual "Expect Nothing" Nothing (treeFind 100 (tree, []))
    assertEqual "Expect root" (Just (tree, [])) (treeFind 10 (tree, []))
    assertEqual "Expect leaf" (goRight (tree, [])) (treeFind 11 (tree, []))
  
modifyTest = TestCase $
  do
    tree <- return (singleton 10 -: treeInsert 9 -: treeInsert 11)
    subtree <- return $ Just (navigate tree [L])

    assertEqual "Modify root" 
      (Just 11)
      (value . toTree . modify (\t -> t + 1) $ (tree, []))

--    assertEqual "Modify something other than the root"
  --    (Node 10 (singleton 11) (singleton 13))
    --  (toTree . modify (\_ -> 13) $ (subtree, []))



main = runTestTT $ TestList [
  treeInsertTest, treeRemoveTest, treeFindTest, modifyTest]
