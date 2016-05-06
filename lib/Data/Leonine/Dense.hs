-- |

module Data.Leonine.Dense (
  Dense
) where

import           Data.Bits           as B
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word

import           Data.Leonine.Chunk

newtype Dense = Dense { dense :: Vector Word16 }
  deriving (Show)

-- | Size of the dense vector to allocate.
size :: Int
size = 2^16

empty :: Dense
empty = Dense (V.replicate size 0)

instance Chunk Dense where
    new c = set c empty

    null = V.all (\w -> B.popCount w == 0) . dense

    popCount = V.foldr' (\w c -> B.popCount w + c) 0 . dense

    toList _ = []

    fromList _ = empty

    set w c = c
