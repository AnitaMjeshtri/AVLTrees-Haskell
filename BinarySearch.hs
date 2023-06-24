data BST a = Empty | Node a (BST a) (BST a) deriving(Show,Read,Eq,Ord)

singleton:: a -> BST a
singleton x = Node x Empty Empty

insert::(Ord a)=> a -> BST a-> BST a
insert x Empty = singleton x
insert x (Node a l r)
  | x < a = Node a (insert x l) r
  | x > a = Node a l (insert x r)
  | otherwise = Node a l r

search::(Ord a)=>a->BST a->Bool
search _ Empty = False
search el (Node a l r)
  | el == a = True
  | el < a = search el l
  | otherwise = search el r

preorder:: BST a->[a]
preorder Empty = []
preorder (Node x l r) = x:(preorder l ++ preorder r)

postorder:: BST a -> [a]
postorder Empty = []
postorder (Node x l r) = postorder l ++ postorder r ++ [x]

inorder::BST a ->[a]
inorder Empty = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

preorderToBST::(Ord a)=>[a]->BST a
preorderToBST [] = Empty
preorderToBST (x:xs) = insert x (preorderToBST xs)

countNodes::BST a ->Int
countNodes Empty = 0
countNodes (Node a l r) = 1 + countNodes l + countNodes r 

findHeight:: BST a -> Int
findHeight Empty = 0
findHeight (Node a l r) = 1 + max (findHeight l) (findHeight r)

countInternalNodes::BST a->Int
countInternalNodes Empty = 0
countInternalNodes (Node _ Empty Empty) = 0
countInternalNodes (Node a l r) = 1 + countInternalNodes l + countInternalNodes r

countLeaves:: BST a -> Int
countLeaves Empty = 0
countLeaves (Node _ Empty Empty) = 1
countLeaves (Node _ l r) = countLeaves l + countLeaves r

collectLeaves::BST a->[a]
collectLeaves Empty = []
collectLeaves (Node a Empty Empty) = [a]
collectLeaves (Node a l r) = collectLeaves l ++ collectLeaves r

collectInternalNodes::BST a->[a]
collectInternalNodes Empty = []
collectInternalNodes (Node _ Empty Empty) = []
collectInternalNodes (Node a l r) = a: collectInternalNodes l ++ collectInternalNodes r

findNodesAtThisLevel::Int-> BST a->Int
findNodesAtThisLevel _ Empty = 0
findNodesAtThisLevel 0 _ = 0
findNodesAtThisLevel lev (Node a l r) = 1 + (findNodesAtThisLevel (lev-1) l) + (findNodesAtThisLevel (lev-1) r)

collectNodesAtThisLevel :: Int -> BST a -> [a]
collectNodesAtThisLevel _ Empty = []
collectNodesAtThisLevel 1 (Node a _ _) = [a] 
collectNodesAtThisLevel lev (Node a l r) = collectNodesAtThisLevel (lev-1) l ++ collectNodesAtThisLevel (lev-1) r

tree = foldr insert Empty [1,5,3,9]
