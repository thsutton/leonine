{-# LANGUAGE InstanceSigs #-}
module Data.Leonine.Sparse (
  Sparse,
  empty, set, clear,
  test
) where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (return)
import qualified Data.List                        as L
import           Data.Monoid
import qualified Data.Vector.Algorithms.Insertion as VA
import qualified Data.Vector.Algorithms.Search    as VA
import           Data.Vector.Unboxed              (Unbox, Vector)
import qualified Data.Vector.Unboxed              as V
import qualified Data.Vector.Unboxed.Mutable      as VM
import           Data.Word

import           Prelude                          (Bool (..), Eq, Int, Ord (..),
                                                   Ordering (..), Read, Show,
                                                   error, flip, foldl,
                                                   otherwise, ($), (.), (/=),
                                                   (==))

-- $setup
-- >>> :set -XFlexibleInstances -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> import qualified Data.List as L
-- >>> instance Arbitrary Sparse where arbitrary = Sparse . (V.fromList . L.nub . L.sort) <$> arbitrary
-- >>> instance Arbitrary (Vector Word16) where arbitrary = (V.fromList . L.sort) <$> arbitrary
-- >>> let is_v_set = \bs -> bs == (V.fromList . L.nub . L.sort . V.toList $ bs)
-- >>> let is_sorted = \bs -> bs == (V.fromList . L.sort . V.toList $ bs)

-- | One of the 2^16 bits which can be set in a 'Sparse' chunk.
type Bit = Word16

-- | A sparse representation of sets of up to 2^16 bits.
newtype Sparse = Sparse { sparse :: Vector Word16 }
  deriving (Eq, Show, Read)

-- * Construction

-- | An empty 'Sparse' chunk.
empty :: Sparse
empty = Sparse mempty

-- | Insert a bit into a sparse chunk.
insert :: Bit -> Sparse -> Sparse
insert b (Sparse bs) =
    let pos = binarySearch b bs
        len = V.length bs
    in case compare pos len of
         -- Insert, if not already present, at pos.
         LT ->
         -- Append to end of vector.
         EQ -> 
         -- Impossible case: insert past the end of the vector.
         GT -> error "Cannot insert to a position *after* the end of a vector."
    = Sparse (V.modify ins bs)
  where
    ins :: 
    ins v = do
      -- Find position to insert.
      ix <- VA.binarySearchL v b
      -- Append to the end
      case compare ix (V.length v) of
        LT -> return ()
        -- Append the element at the end.
        EQ -> do
              v' <- VM.grow v 1
              VM.write v' ix b
        -- Impossible case: insert past the end of the vector.
        GT -> 

