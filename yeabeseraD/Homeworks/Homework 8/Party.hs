module Party where

import Employee
import Data.Tree(Tree(Node))

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL guestList cachedFun) = GL (emp:guestList) (cachedFun + fun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) | fun1 >= fun2 = gl1 | otherwise =  gl2

instance Semigroup GuestList where
    (<>) :: GuestList -> GuestList -> GuestList
    (GL gl1 fun1) <> (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

instance Monoid GuestList where
    mempty :: GuestList
    mempty = GL [] 0

treeFold :: b -> (a -> b -> b) -> Tree a -> b
treeFold acc f (Node a []) = f acc a
treeFold acc f (Node a (x:xs)) = acc
    where foldTreeList :: [Tree a] -> b
          foldTreeList = foldl (`treeFold` f) acc

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [(listWithBoss, listWithNoBoss)] = (newListWithBoss, newListWithNoBoss)
    where newListWithBoss = glCons emp bestList
          newListWithNoBoss = glCons emp bestList
          bestList 
            | listWithBoss >= listWithNoBoss = listWithBoss 
            | otherwise = listWithNoBoss