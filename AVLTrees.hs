data AVLTree a = EmptyTree | Nodee a (AVLTree a) (AVLTree a) deriving (Show, Read, Eq, Ord)


height:: AVLTree a -> Int
height EmptyTree = 0
height (Nodee _ left right) = 1 + max (height left) (height right)

balanceFactor:: AVLTree a -> Int
balanceFactor EmptyTree = 0
balanceFactor (Nodee _ left right) = height(left) - height(right)

rotateLeftLeft:: AVLTree a -> AVLTree a
rotateLeftLeft (Nodee a (Nodee b ll lr) r) = Nodee b ll (Nodee a lr r)

rotateRightRight:: AVLTree a -> AVLTree a
rotateRightRight (Nodee a l (Nodee b rl rr)) = Nodee b (Nodee a l rl) rr

rotateLeftRight:: AVLTree a -> AVLTree a
rotateLeftRight (Nodee a l r) = rotateLeftLeft(Nodee a (rotateRightRight l) r)

rotateRightLeft:: AVLTree a -> AVLTree a
rotateRightLeft (Nodee a l r) = rotateRightRight(Nodee a l (rotateLeftLeft r))

insertAVL::(Ord a) => a -> AVLTree a -> AVLTree a
insertAVL x EmptyTree = Nodee x EmptyTree EmptyTree
insertAVL x (Nodee a left right)
  | x < a = rebalance(Nodee a (insertAVL x left) right)
  | x > a =  rebalance(Nodee a left (insertAVL x right))
  | otherwise = Nodee a left right

rebalance::(Ord a) => AVLTree a -> AVLTree a
rebalance tree
    | balanceFactor tree > 1 = rebalanceLeft tree
    | balanceFactor tree < -1 = rebalanceRight tree
    | otherwise = tree

rebalanceLeft::(Ord a) => AVLTree a -> AVLTree a
rebalanceLeft (Nodee a l r)
    | balanceFactor l >= 0 = rotateLeftLeft (Nodee a l r)
    | otherwise = rotateLeftRight (Nodee a l r)

rebalanceRight:: (Ord a) => AVLTree a -> AVLTree a
rebalanceRight (Nodee a l r)
    | balanceFactor r <= 0 = rotateRightRight (Nodee a l r)
    | otherwise = rotateRightLeft (Nodee a l r)

preorder:: AVLTree a -> [a]
preorder EmptyTree = []
preorder (Nodee a l r) = a : (preorder l ++ preorder r)

inorder:: AVLTree a -> [a]
inorder EmptyTree = []
inorder (Nodee a l r) = inorder l ++ [a] ++ inorder r

postorder:: AVLTree a -> [a]
postorder EmptyTree = []
postorder (Nodee a l r) = postorder l ++ postorder r ++ [a]

searchAVL:: (Ord a )=>a->AVLTree a->Bool
searchAVL _ EmptyTree = False
searchAVL el (Nodee a l r)
    | el == a = True
    | el < a = searchAVL el l
    | el > a = searchAVL el r

tree = insertAVL 4 EmptyTree
tree1 = insertAVL 3 tree
tree2 = insertAVL 2 tree1
tree3 = insertAVL 1 tree2
tree4 = insertAVL 0 tree3
tree5 = insertAVL 7 tree4