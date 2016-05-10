module Data.Leonine.Dense where

import           Data.Bits
import           Data.Monoid
import qualified Data.Vector.Algorithms.Insertion as VA
import           Data.Vector.Unboxed              (Unbox, Vector)
import qualified Data.Vector.Unboxed              as V
import qualified Data.Vector.Unboxed.Mutable      as V (read, write)
import           Data.Word

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> import qualified Data.List as L
-- >>> instance Arbitrary Chunk where arbitrary = foldl set mempty . L.nub . L.sort <$> arbitrary
-- >>> instance (V.Unbox a, Ord a, Arbitrary a) => Arbitrary (Vector a) where arbitrary = V.fromList<$> arbitrary

-- | The identifier of a single bit in a 'Chunk'.
type Bit = Word16

-- | A bitmap of 2^16 bits.
--
-- The contained vector always contains 2^16 / 26 = 4096 'Word16' values.
newtype Chunk = Chunk { dense :: Vector Word16 }
  deriving (Show, Eq, Ord)

instance Monoid Chunk where
    mempty = Chunk (V.replicate 4096 0)
    (Chunk c1) `mappend` (Chunk c2) = Chunk (V.zipWith (.|.) c1 c2)

-- * Queries

-- | Check whether a 'Bit' is set in a 'Chunk'.
test :: Chunk -> Bit -> Bool
test (Chunk bs) b = False

-- | Count the number of bits set.
--
-- prop> pop mempty == 0
-- prop> \b -> pop (set mempty b) == 1
-- prop> \c b -> pop (set c b) `elem` [0,1]
pop :: Chunk -> Int
pop (Chunk bs) = V.foldl' (\c w -> c + popCount w) 0 bs


-- *

-- | Set a 'Bit' in a 'Chunk'.
--
-- prop> \b1 -> let c = mempty in 1 + pop c == pop (set c b1)
-- prop> \b1 -> let c = mempty in 1 + pop c == pop (set (set c b1) b1)
set :: Chunk -> Bit -> Chunk
set c@(Chunk bs) b
    | test c b = c
    | otherwise =
        let (wi, bi) = address b
        in Chunk (v_alter wi (\w -> setBit w bi) bs)

-- * Utilities

-- | Calculate the word and bit indices for a 'Bit' within a 'Chunk'.
address :: Bit -> (Int, Int)
address b = (0,0)

-- | Alter an element in a vector.
--
-- Precondition: The index is within the bounds.
v_alter
    :: Unbox a
    => Int -- ^ Index in 'Vector'
    -> (a -> a) -- ^ Modify
    -> Vector a -- ^ Vector
    -> Vector a
v_alter i f v = V.modify act v
  where
    act v = do
      w <- V.read v i
      V.write v i (f w)
