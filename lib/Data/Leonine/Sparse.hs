module Data.Leonine.Sparse (
    Bit,
    Chunk,
    test,
    set,
    clear,
    bits,
    pop,
    -- Internal functions
    v_elem, v_insert, v_sort
) where

import qualified Data.List                        as L
import           Data.Maybe                       (isJust)
import           Data.Monoid
import qualified Data.Vector.Algorithms.Insertion as VA
import           Data.Vector.Unboxed              (Vector)
import qualified Data.Vector.Unboxed              as V
import           Data.Word

import Data.Leonine.Utils

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> import qualified Data.List as L
-- >>> instance Arbitrary Chunk where arbitrary = Chunk . V.fromList . L.nub . L.sort <$> arbitrary
-- >>> instance (V.Unbox a, Ord a, Arbitrary a) => Arbitrary (Vector a) where arbitrary = V.fromList<$> arbitrary

-- | A sparse bitmap of 2^16 bits.
--
-- The representation is an unboxed vector of unique 16-bit words
-- stored in ascending order.
newtype Chunk = Chunk { sparse :: Vector Bit }
  deriving (Show, Eq, Ord)

instance Monoid Chunk where
    mempty = Chunk (V.empty)
    (Chunk as) `mappend` (Chunk bs) = Chunk (merge as bs)

-- * Queries

-- | Check whether a 'Bit' is set in a 'Chunk'.
test :: Chunk -> Bit -> Bool
test (Chunk bs) b = isJust (search id b bs)

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

bits :: Chunk -> [Bit]
bits = V.toList . sparse

-- * Helpers functions

-- | Test whether an element occurs in a sorted vector.
v_elem :: (V.Unbox a, Ord a) => Vector a -> a -> Bool
v_elem bs b = isJust (search id b bs)

-- | Insert an element into a sorted vector.
--
-- prop> \(inp :: Vector Int, b) -> let bs = v_sort inp in v_elem (v_insert bs b) b
-- prop> \(inp :: Vector Int, b) -> let bs = v_sort inp in v_sort (v_insert bs b) == (v_insert bs b)
v_insert :: (V.Unbox a, Ord a) => Vector a -> a -> Vector a
v_insert bs b = v_sort (V.cons b bs)
