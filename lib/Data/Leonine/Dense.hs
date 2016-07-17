module Data.Leonine.Dense where

import           Data.Bits
import           Data.Convertible
import           Data.Monoid
import qualified Data.Vector.Algorithms.Insertion as VA
import           Data.Vector.Unboxed              (Unbox, Vector)
import qualified Data.Vector.Unboxed              as V
import qualified Data.Vector.Unboxed.Mutable      as V (read, write)
import           Data.Word

import Data.Leonine.Utils

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> import qualified Data.List as L
-- >>> instance Arbitrary Chunk where arbitrary = foldl set mempty . L.nub . L.sort <$> arbitrary
-- >>> instance (V.Unbox a, Ord a, Arbitrary a) => Arbitrary (Vector a) where arbitrary = V.fromList<$> arbitrary

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
--
-- prop> \(b :: Word16) -> test (set mempty b) b
-- prop> \(b :: Word16) -> not (test mempty b)
test :: Chunk -> Bit -> Bool
test (Chunk bs) b =
  let (w, i) = address b
  in testBit (bs V.! w) i

-- | Count the number of bits set.
--
-- prop> pop mempty == 0
-- prop> \b -> pop (set mempty b) == 1
-- prop> \c b -> (pop (set c b) - pop c) `elem` [0,1]
pop :: Chunk -> Int
pop (Chunk bs) = V.foldl' (\c w -> c + popCount w) 0 bs

-- | Produce a list of 'Bit's set in a 'Chunk'.
--
-- prop> \b -> pop b == length (bits b)
-- prop> \b -> b == (foldl (set) mempty $ bits b)
bits :: Chunk -> [Bit]
bits (Chunk bs) = concatMap bts (zip [0..] $ V.toList bs)
  where
    rng :: [Int]
    rng = [0..15]
    bts :: (Int, Word16) -> [Bit]
    bts (i, w) =
      let ix = convert (i * 16)
      in [ ix + (convert b)
         | b <- rng
         , testBit w b
         ]

-- *

-- | Set a 'Bit' in a 'Chunk'.
--
-- prop> \b1 -> let c = mempty in 1 + pop c >= pop (set c b1)
-- prop> \c b1 -> pop (set c b1) == pop (set (set c b1) b1)
set :: Chunk -> Bit -> Chunk
set c@(Chunk bs) b
    | test c b = c
    | otherwise =
        let (wi, bi) = address b
        in Chunk (v_alter wi (\w -> setBit w bi) bs)

-- | Clear a 'Bit' in a 'Chunk'.
--
-- prop> \c b -> not (test (clear c b) b)
-- prop> \b -> not (test (clear (set mempty b) b) b)
clear :: Chunk -> Bit -> Chunk
clear c@(Chunk bs) b
  | test c b =
    let (w,i) = address b
    in Chunk (v_alter w (\b -> clearBit b i) bs)
  | otherwise = c

-- * Utilities

-- | Calculate the word and bit indices for a 'Bit' within a 'Chunk'.
--
-- >>> address 0
-- (0,0)
-- >>> address 16
-- (1,0)
-- >>> address 4096
-- (256,0)
-- >>> address 0xffff
-- (4095,15)
address :: Bit -> (Int, Int)
address b =
  let (w, i) = b `divMod` 16
  in (convert w, convert i)

-- | Combine word and bit indices into a 'Bit' within a 'Chunk'.
--
-- prop> \b -> b == combine (address b)
combine :: (Int, Int) -> Bit
combine (w,i) = convert $ (w * 16) + i

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
