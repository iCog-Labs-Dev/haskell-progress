module Party where

import Employee ( Employee(Emp), GuestList(..), testCompany )
import Data.Tree(Tree(Node))
import Data.Monoid

instance Semigroup GuestList where
    (<>) :: GuestList -> GuestList -> GuestList
    (GL gl1 fun1) <> (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

instance Monoid GuestList where
    mempty :: GuestList
    mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL guestList cachedFun) = GL (emp:guestList) (cachedFun + fun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) | fun1 >= fun2 = gl1 | otherwise =  gl2

--This Function works perfectly
treeFold ::(Monoid b) => (a -> b -> b) -> b -> Tree a -> b
treeFold f acc (Node x []) = f x acc
treeFold f acc (Node x treeList) = mconcat $ map (treeFold f acc) treeList ++ [f x acc]

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp guestList= (bestListWithBoss, bestListWithNoBoss)
    where newLists = map (fmap' emp) guestList
          bestListWithBoss   = bestGuestList newLists
          bestListWithNoBoss = bestGuestList guestList
          bestGuestList = foldl (\acc (withBoss, withNoBoss) -> moreFun acc $ moreFun withBoss withNoBoss) mempty

nextLevel' :: Employee -> [(GuestList, GuestList)] -> [(GuestList, GuestList)]
nextLevel' emp guestLists = [nextLevel emp guestLists]

maxFun :: Tree Employee -> GuestList
maxFun campTree = moreFun withBoss withNoBoss
    where (withBoss, withNoBoss)                    = mconcat $ treeFold nextLevel' [mempty, mempty] campTree
          nextLevel' emp guestLists = [nextLevel emp guestLists]

fmap' :: Employee -> (GuestList, GuestList) -> (GuestList, GuestList)
fmap' emp (withBoss, withNoBoss) = (glCons emp withBoss, glCons emp withNoBoss)