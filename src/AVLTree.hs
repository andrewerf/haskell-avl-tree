module AVLTree where


data AVLTree k = AVLTree {
    cLeft :: AVLTree k,
    cRight :: AVLTree k,
    value :: k
} | NoTree


height :: (Ord k) => AVLTree k -> Integer
height NoTree = -1
height (AVLTree left right _) = 1 + max (height left) (height right)


isBalanced :: (Ord k) => AVLTree k -> Bool
isBalanced NoTree = True
isBalanced (AVLTree left right _) = 
    foldl (&&) True [isBalanced left, isBalanced right, (abs (height left - height right)) <= 1]


factor :: (Ord k) => AVLTree k -> Integer
factor NoTree = 0
factor (AVLTree left right _) = height right - height left


insert :: (Ord k) => AVLTree k -> k -> AVLTree k
insert NoTree new_val = AVLTree NoTree NoTree new_val
insert (AVLTree left right val) new_val 
    | new_val < val = rotate (AVLTree (insert left new_val) right val)
    | otherwise = rotate (AVLTree left (insert right new_val) val)


rotate :: (Ord k) => (AVLTree k) -> (AVLTree k)
rotate NoTree = NoTree
rotate (AVLTree left right val)
    | not (isBalanced left) = (AVLTree (rotate left) right val)
    | not (isBalanced right) = (AVLTree left (rotate right) val)

    | h_l + 1 < h_r = -- right is higher (and tree is unbalanced)
        if (height l_r) <= (height r_r) -- right child of r (r_r) is higher
        then AVLTree (AVLTree left l_r val) r_r (value right)
        else AVLTree (AVLTree left (cLeft l_r) val) (AVLTree (cRight l_r) r_r (value right)) (value l_r)

    | h_r + 1 < h_l = -- symmetric
        if (height r_l) <= (height l_l)
        then AVLTree l_l (AVLTree r_l right val) (value left)
        else AVLTree (AVLTree l_l (cLeft r_l) (value left)) (AVLTree (cRight r_l) right val) (value r_l)

    | otherwise = tree
    
    where tree = (AVLTree left right val)
          h_r = height right
          h_l = height left
          r_l = cRight left
          r_r = cRight right
          l_l = cLeft left
          l_r = cLeft right

buildAVLTree :: (Ord k) => [k] -> (AVLTree k)
buildAVLTree xs = foldl insert NoTree xs
