-- AVL trees

{- resources:
   - https://visualgo.net/bst  nice visualisation of all operations, recommended!

   the English wiki article on AVL trees is very poorly written; the article on
   rotations is worthwhile
   - https://de.wikipedia.org/wiki/AVL-Baum  brush up on your German! or use
     google translate
   - https://en.wikipedia.org/wiki/Tree_rotation
-}


data AVL a = Leaf | Node Int (AVL a) a (AVL a) deriving Show

-- here's a drawing function, which, combined with [putStrLn], lets you visualise a tree
data S = L | R
draw :: Show a => AVL a -> String
draw t = "\n" ++ draw' Nothing t 0 ++ "\n"
  where
    draw' _ Leaf _ = []
    draw' dir (Node _ l v r) d =
      draw' (Just R) r (d+1) ++ node dir ++ draw' (Just L) l (d+1)
      where
        node dir' = padding d ++
          case dir' of
            Nothing -> ""
            Just L -> "\\- "
            Just R -> "/- "
          ++ show v ++ "\n"
        padding n = replicate (n*4) ' '


empty :: AVL a
empty = Leaf

height :: AVL a -> Int
height Leaf = 0
height (Node h _ _ _) = h

-- [avl left a right] is a smart constructor that fills in the height of the
-- new tree. The height of [left] and [right] may differ by at most 1
avl :: AVL a -> a -> AVL a -> AVL a
avl l a d = Node h l a d
  where h = 1 + max (height l) (height d)

-- check if an element appears in a tree
element :: Ord a => a -> AVL a -> Bool
element x Leaf = False
element x (Node _ l y d)
  |x > y = element x d
  |x < y = element x l
  |otherwise = True

-- [skew t] computes how "disbalanced" [t] is, ie. how its subtrees compare in
-- height. A search tree is an AVL tree is this factor is never greater than 1.
skew :: AVL a -> Int
skew Leaf = 0
skew (Node _ l x d) = height l - height d

-- Perform a right rotation
rotR :: AVL a -> AVL a
rotR (Node _ (Node _ l1 y d1) x d2) = Node h l1 y (Node h1 d1 x d2)
  where h1 = 1 + max (height d1) (height d2) 
        h = 1 + max (height l1) h1


-- Perform a left rotation
rotL :: AVL a -> AVL a
rotL (Node _ l1 x (Node _ l2 y d2)) = Node h (Node h1 l1 x l2) y d2
  where h1 = 1 + max (height l1) (height l2)
        h = 1 + max (height d2) h1
        

-- [rebalance t] performs the rotations necessary to re-balance an almost-AVL
-- tree which is skewed by at most 2.
rebalance :: AVL a -> AVL a
rebalance Leaf = Leaf
rebalance a@(Node h l x r) 
  |abs (skew a) < 2 = avl (rebalance l) x (rebalance r) 
  |skew a == 2 && skew l >= 0 = rotR a
  |skew a == -2 && skew r <= 0 = rotL a
  |skew a == -2 && skew l < 0 = rotR (avl (rotL l) x r)
  |skew a == 2 && skew r > 0 = rotL (avl l x (rotR r))

-- [add t a] inserts an element into an AVL tree, ensuring that the invariants
-- are respected.
add :: (Ord a) => AVL a -> a -> AVL a
add Leaf y = avl Leaf y Leaf
add a@(Node _ l x r) y
  |y < x = rebalance $ avl (add l y) x r
  |y > x = rebalance $ avl l x (add r y)
  |otherwise = a

-- With [add] implemented, we can easily construct an AVL tree from a list
fromList :: (Ord a) => [a] -> AVL a
fromList xs = foldl add Leaf xs

-- [minAVL t] finds the minimum element of [t]
minAVL :: (Ord a) => AVL a -> Maybe a
minAVL Leaf = Nothing
minAVL (Node _ Leaf x _) = Just x
minAVL (Node _ l x r) = minAVL l

-- [delete t a] returns an AVL tree [t] with [a] removed
delete :: Ord a => AVL a -> a -> AVL a
delete Leaf x = Leaf
delete (Node _ l y r) x 
  |x > y = rebalance $ avl l y (delete r x) 
  |x < y = rebalance $ avl (delete l x) y r
  |otherwise = case minAVL r of
                    Just a -> avl l a (delete r a)
                    Nothing -> Leaf 







