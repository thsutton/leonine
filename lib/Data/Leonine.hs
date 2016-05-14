module Data.Leonine where

import           Data.Monoid
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word

import qualified Data.Leonine.Dense  as Dense
import qualified Data.Leonine.Sparse as Sparse
import           Data.Leonine.Utils

-- | A roaring bitmap.
--
-- The bitmap is a 'Vector' of 'Chunk's indexed by the high 16-bits of
-- the words stored in that chunk.
newtype Bitmap = Bitmap { chunks :: Vector Chunk }
  deriving (Eq, Ord)

instance Monoid Bitmap where
  mempty = Bitmap mempty
  (Bitmap cs1) `mappend` (Bitmap cs2) = Bitmap (mergeWith chunkIndex mergeChunks cs1 cs2)

-- | A chunk of a bitmap.
data Chunk
    = Dense { chunkIndex :: Index, denseChunk :: Dense.Chunk }
    | Sparse { chunkIndex :: Index, sparseChunk :: Sparse.Chunk }
  deriving (Eq)

-- | Merge two 'Chunk's.
mergeChunks :: Chunk -> Chunk -> Chunk
mergeChunks c1 c2
    | chunkIndex c1 /= chunkIndex c2 = error "Tried to merge chunks with different indices."
    | otherwise = go c1 c2
  where
    go (Dense ix v1) (Dense _ v2) = Dense ix (v1 <> v2)
    go (Dense ix v1) (Sparse _ v2) = Dense ix (foldl Dense.set v1 (Sparse.bits v2))
    go (Sparse ix v1) (Dense _ v2) = Dense ix (foldl Dense.set v2 (Sparse.bits v1))
    go (Sparse ix v1) (Sparse _ v2) =
      let v = v1 <> v2
      in if Sparse.pop v >= 4096
         then Dense ix (foldl Dense.set mempty (Sparse.bits v))
         else Sparse ix v

-- | Order chunks by their index.
instance Ord Chunk where
  c1 `compare` c2 = (chunkIndex c1) `compare` (chunkIndex c2)

testBit :: Word32 -> Bitmap -> Bool
testBit w (Bitmap m) =
  let (ix, bt) = splitWord w
  in case search chunkIndex ix m of
    Nothing -> False
    Just (Dense _ c) -> Dense.test c bt
    Just (Sparse _ c) -> Sparse.test c bt
