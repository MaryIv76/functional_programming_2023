-- Binary Tree
data BinaryTree t 
 = EmptyTree 
 | Node t (BinaryTree t) (BinaryTree t)
 deriving(Show)


singleNodeTree x = Node x EmptyTree EmptyTree


textTree = Node "Eins" 
             (Node "Zwei" (singleNodeTree "Vier") (singleNodeTree "Fuenf"))
             (Node "Drei" (singleNodeTree "Sechs") (singleNodeTree "Sieben"))
numTree = Node 0
            (Node 1 (singleNodeTree 2) (singleNodeTree 3))
            (singleNodeTree 4)


-- treeMap
treeMap _ EmptyTree = EmptyTree
treeMap f (Node x left right) = Node (f x) (treeMap f left) (treeMap f right)


-- treeSize
treeSize EmptyTree = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right


-- treeTraverseD
treeTraverseD _ res EmptyTree = res
treeTraverseD f res (Node x left right) =
 treeTraverseD f (f (treeTraverseD f res left) x) right


-- treeTraverseW
treeTraverseWHelper _ [] res = res
treeTraverseWHelper f (EmptyTree:xs) res = treeTraverseWHelper f xs res
treeTraverseWHelper f ((Node x left right):xs) res = treeTraverseWHelper f (xs ++ [left, right]) (f res x)

treeTraverseW _ res EmptyTree = res
treeTraverseW f res (Node x left right) = 
 treeTraverseWHelper f [(Node x left right)] res