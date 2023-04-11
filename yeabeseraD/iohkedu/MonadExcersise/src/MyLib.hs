{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module MyLib where

import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad

newtype Address = MkAddress Int
    deriving (Eq, Ord, Show)

addressMapping :: M.Map Address Address
addressMapping = 
    M.fromList 
    [ (MkAddress 3, MkAddress 7),
      (MkAddress 4, MkAddress 20),
      (MkAddress 5, MkAddress 3),
      (MkAddress 7, MkAddress 14),
      (MkAddress 9, MkAddress 5),
      (MkAddress 16, MkAddress 9)
    ]
newtype ErrorMessage = AddressNotFound Address deriving Show

lookup' :: Address -> Map Address a -> Either ErrorMessage a
lookup' address mapping=
    case M.lookup address  mapping of
        Nothing -> Left (AddressNotFound address)
        Just value -> Right value

threeHopes' :: Address -> Either ErrorMessage String
threeHopes' address0 = do
    address1 <- lookup' address0 addressMapping
    address2 <- lookup' address1 addressMapping
    address3 <- lookup' address2 addressMapping
    Right $ show address3

threeHopes :: Address -> Maybe String
threeHopes address0 = do
    address1 <- M.lookup address0 addressMapping
    address2 <- M.lookup address1 addressMapping
    address3 <- M.lookup address2 addressMapping
    Just $ show address3

data Tree a = 
      Node (Tree a) a (Tree a)
    | Leaf
    deriving Show
    
labelTree :: Tree a -> Tree (Int, a)
labelTree tree = newTree
    where (newTree, _) =runWithCounter (labelTree' tree) 0

newtype WithCounter a = MkWithCounter {runWithCounter :: Int -> (a, Int)}

labelTree'Org :: Tree a -> WithCounter (Tree (Int, a))
labelTree'Org Leaf = MkWithCounter (\currentLabel -> (Leaf, currentLabel))
labelTree'Org (Node l x r) = 
    MkWithCounter (\currentLabel -> case runWithCounter (labelTree'Org l) currentLabel of
        (l', currentLabel') ->
            case runWithCounter tick currentLabel' of
                (labelForX, nextLabel) ->
                    case runWithCounter(labelTree'Org r) nextLabel of
                        (r', currentLabel'') ->
                            (Node l' (labelForX, x) r', currentLabel'')
    )

instance Functor WithCounter where
    fmap = liftM


instance Applicative WithCounter where
    pure = returnWithCounter
    (<*>) = ap

instance Monad WithCounter where
    return = pure
    (>>=) = bindWithCounter


labelTree' :: Tree a -> WithCounter (Tree (Int, a))
labelTree' Leaf = returnWithCounter Leaf
labelTree' (Node l x r) = do
    l' <-labelTree' l
    labelForX <- tick
    r' <-labelTree' r
    return (Node l' (labelForX, x) r')

tick :: WithCounter Int
tick = MkWithCounter (\current -> (current, current + 1))

bindWithCounter :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
bindWithCounter computation continuation = 
    MkWithCounter (\currentCounter -> case runWithCounter computation currentCounter of
        (result, currentCounter') -> runWithCounter (continuation result) currentCounter'
    )

returnWithCounter :: a -> WithCounter a
returnWithCounter x = MkWithCounter (\currentCounter-> (x, currentCounter))

buildTree :: a -> Int -> Tree a
buildTree val h = 
    if h <=0
        then Leaf
        else
            let subTree = buildTree val (h-1)
            in Node subTree val subTree