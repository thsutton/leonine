module Data.Leonine.Sparse (
    Bit,
    Chunk,
    test,
    set,
    clear,
    -- Internal functions
    v_elem, v_insert, v_sort
) where

import qualified Data.List                        as L
import           Data.Monoid
import qualified Data.Vector.Algorithms.Insertion as VA
import           Data.Vector.Unboxed              (Vector)
import qualified Data.Vector.Unboxed              as V
import           Data.Word

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> import qualified Data.List as L
-- >>> instance Arbitrary Chunk where arbitrary = Chunk . V.fromList . L.nub . L.sort <$> arbitrary
-- >>> instance (V.Unbox a, Ord a, Arbitrary a) => Arbitrary (Vector a) where arbitrary = V.fromList<$> arbitrary

-- | A bit in a bitmap.
type Bit = Word16

-- | A bitmap of 2^16 bits.
newtype Chunk = Chunk { sparse :: Vector Bit }
  deriving (Show, Eq, Ord)

instance Monoid Chunk where
    mempty = Chunk (V.empty)

    (Chunk as) `mappend` (Chunk bs) = Chunk (v_merge as bs)

-- * Queries

-- | Check whether a 'Bit' is set in a 'Chunk'.
test :: Chunk -> Bit -> Bool
test (Chunk bs) b = bs `v_elem` b

-- | Population count
--
-- prop> pop mempty == 0
-- prop> \b -> pop (set mempty b) == 1
pop :: Chunk -> Int
pop (Chunk bs) = V.length bs

-- | Set a bit in a chunk.
--
-- prop> \(c, b) -> test (set c b) b
set :: Chunk -> Bit -> Chunk
set c@(Chunk bs) b
    | bs `v_elem` b = c
    | otherwise     = Chunk(bs `v_insert` b)

-- | Clear a bit in a chunk.
--
-- prop> \(c, b) -> not (test (clear c b) b)
clear :: Chunk -> Bit -> Chunk
clear c@(Chunk bs) b
    | bs `v_elem` b = Chunk(bs `v_remove` b)
    | otherwise     = c

-- * Helpers functions

-- | Test whether an element occurs in a sorted vector.
v_elem :: (V.Unbox a, Ord a) => Vector a -> a -> Bool
v_elem bs b = V.elem b bs

-- | Insert an element into a sorted vector.
--
-- prop> \(Chunk bs, b) -> v_elem (v_insert bs b) b
-- prop> \(Chunk bs, b) -> v_sort (v_insert bs b) == (v_insert bs b)
v_insert :: (V.Unbox a, Ord a) => Vector a -> a -> Vector a
v_insert bs b = v_sort (V.cons b bs)

-- | Remove an element from a vector.
--
-- prop> \(bs, b::Word16) -> not (v_elem (v_remove bs b) b) || (v_elem bs b)
-- prop> \(Chunk bs, b) -> v_remove (v_insert bs b) b == bs
v_remove :: (V.Unbox a, Ord a) => Vector a -> a -> Vector a
v_remove bs b =
    let (h,t) = V.break (== b) bs
    in if V.null t then h else h V.++ (V.tail t)

-- | Sort the elements of a vector.
--
-- prop> \(bs :: Vector Bit) -> V.toList (v_sort bs) == L.sort (V.toList bs)
v_sort :: (V.Unbox a, Ord a) => Vector a -> Vector a
v_sort = V.modify VA.sort

-- | Merge two sorted arrays.
v_merge :: (V.Unbox a, Ord a) => Vector a -> Vector a -> Vector a
v_merge as bs
    | V.null as = bs
    | V.null bs = as
    | otherwise =
        let a   = V.head as
            as' = V.tail as
            b   = V.head bs
            bs' = V.tail bs
        in case compare a b of
             LT -> V.cons a (V.cons b (v_merge as' bs'))
             EQ -> V.cons a (v_merge as' bs')
             GT -> V.cons b (V.cons a (v_merge as' bs'))
