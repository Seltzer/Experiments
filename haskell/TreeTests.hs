import Test.HUnit
import Core
import Trees

value :: Tree a -> Maybe a
value EmptyTree = Nothing
value (Node x l r) = Just x


treeInsertTest = TestCase (
  do
    tree <- return (singleton 10 -: treeInsert 9 -: treeInsert 11)

    assertEqual "Check node count" 3 (nodeCount tree) 
    assertEqual "Check values" (Just 10) (value tree)
  )



main = runTestTT treeInsertTest
