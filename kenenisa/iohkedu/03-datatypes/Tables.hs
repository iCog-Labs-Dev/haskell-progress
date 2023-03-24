module Tables (
  Table,
  empty,
  insert,
  delete,
  lookup,
  mapValues,
  mapKeys,
  alter
) where

import Prelude hiding (lookup)
import Data.Maybe

-- START HERE AFTER reaching the pointer in Datatypes.hs

newtype Table k v = Table [(k, v)]
  deriving (Show)

-- In the following, we first reimplement the functions
-- from the slides, but with the @newtype@-based version
-- of the 'Table' type.

-- Task Tables-1.
--
-- Re-implement 'empty'.

empty :: Table k v
empty = Table []

-- Task Tables-2.
--
-- Re-implement 'insert'.

insert :: (Eq k) => k -> v -> Table k v -> Table k v
insert key value t@(Table table)
  | isNothing (lookup key t) = Table ((key,value) : table)
  | otherwise = insert key value (delete key t)

-- Task Tables-3.
--
-- Re-implement 'delete'.

delete :: Eq k => k -> Table k v -> Table k v
delete key (Table table) = Table $ remove table
  where
    remove [] = []
    remove (x:xs)
      | fst x == key = remove xs
      | otherwise = x : remove xs

-- Task Tables-4.
--
-- Re-implement 'lookup'.

lookup :: Eq k => k -> Table k v -> Maybe v
lookup _ (Table []) = Nothing
lookup key (Table (x:xs))
  | fst x == key = Just $ snd x
  | otherwise = lookup key (Table xs)

-- Task Tables-5.
--
-- Implement a map function on the table values.

mapValues :: (v1 -> v2) -> Table k v1 -> Table k v2
mapValues f (Table table) = Table $ values table
  where
    values [] = []
    values (x:xs) = (fst x, f $ snd x) : values xs

-- Task Tables-6.
--
-- Implement a map function on the table keys.
--
-- Tricky additional question:
-- Can you identify a potential problem with
-- this function?

mapKeys :: (Eq k1,Eq k2) => (k1 -> k2) -> Table k1 v -> Table k2 v
mapKeys f (Table table) = Table $ keys table
  where
    keys [] = []
    keys (x:xs) = (f $ fst x , snd x) : keys xs

-- Task Tables-7.
--
-- Implement a more general table update function.
-- The function 'alter' takes a function and a key.

alter :: Eq k => (Maybe v -> Maybe v) -> k -> Table k v -> Table k v
alter f key table
  | isNothing value = delete key table
  | otherwise = insert key (justValue value) (delete key table)
    where
      value = f $ lookup key table
      justValue (Just v) = v

-- Task Tables-8.
--
-- Add an export list to the module, exporting
-- all the functions, and the 'Table' type, but
-- no constructors. The syntax
--
--   Table()
--
-- can be used in the export list to export a
-- datatype or newtype without any of its
-- constructors.

-- GO TO Transactions.hs