-- |

module Data.Leonine where

import           Data.Function
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Word

import           Data.Leonine.Dense  (Dense)
import qualified Data.Leonine.Dense  as Dense
import           Data.Leonine.Sparse (Sparse)
import qualified Data.Leonine.Sparse as Sparse

data Chunk
    = ChunkSparse { index :: Word16, sparseBits :: Sparse }
    | ChunkDense  { index :: Word16, denseBits :: Dense }

orderChunks :: Chunk -> Chunk -> Ordering
orderChunks = compare `on` index

newtype Bitmap = Bitmap { chunks :: Vector Chunk }
