module Data.Leonine.Chunk where


import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Word


-- | The interface to a bitmap chunk of 2^16 bits.
class Chunk c where
    -- | Create a new chunk with a single bit set.
    new :: Word16 -> c

    null :: c -> Bool

    popCount :: c -> Int

    toList :: c -> [Word16]

    fromList :: [Word16] -> c

    set :: Word16 -> c -> c
