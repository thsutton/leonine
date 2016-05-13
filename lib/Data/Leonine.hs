module Data.Leonine where


import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word

import qualified Data.Leonine.Dense  as Dense
import qualified Data.Leonine.Sparse as Sparse
import           Data.Leonine.Utils

data Chunk
    = Dense { chunkIndex :: Index, denseChunk :: Dense.Chunk }
    | Sparse { chunkIndex :: Index, sparseChunk :: Sparse.Chunk }
  deriving (Eq)

-- | Order chunks by their index.
instance Ord Chunk where
  c1 `compare` c2 = (chunkIndex c1) `compare` (chunkIndex c2)

newtype Bitmap = Bitmap { chunks :: Vector Chunk }
  deriving (Eq, Ord)

testBit :: Word32 -> Bitmap -> Bool
testBit w (Bitmap m) =
  let (ix, bt) = splitWord w
  in False
