module Data.Leonine where

import           Control.Arrow
import           Data.Monoid
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Data.Word

import qualified Data.Leonine.Dense  as Dense
import qualified Data.Leonine.Sparse as Sparse
import           Data.Leonine.Utils

-- $setup
-- >>> :set -XStandaloneDeriving
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> import qualified Data.List as L
-- >>> deriving instance Show Chunk
-- >>> deriving instance Show Bitmap
-- >>> instance Arbitrary Bitmap where arbitrary = foldl (flip setBit) mempty . L.nub . L.sort <$> arbitrary

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

-- | Test whether a 'Bit' is set in a 'Bitmap'.
--
-- prop> \i -> not (testBit i mempty)
-- prop> \i -> testBit i (setBit i mempty)
testBit :: Word32 -> Bitmap -> Bool
testBit w (Bitmap m) =
  let (ix, bt) = splitWord w
  in case search chunkIndex ix m of
    Nothing -> False
    Just (Dense _ c) -> Dense.test c bt
    Just (Sparse _ c) -> Sparse.test c bt

-- | Count the number of set 'Bit's in a 'Bitmap'.
pop :: Bitmap -> Int
pop (Bitmap cs) = V.foldl' (\a c -> a + (cpop c)) 0 cs
  where
    cpop (Dense _ c) = Dense.pop c
    cpop (Sparse _ c) = Sparse.pop c

-- | List the 'Bit's set in a 'Bitmap'.
--
-- prop> \b -> (pop b) == length (bits b)
-- prop> \b -> b == (foldl (flip setBit) mempty $ bits b)
bits :: Bitmap -> [Word32]
bits (Bitmap cs) =
  concatMap chunkToList (V.toList cs)
  where
    chunkToList (Sparse i c) = fmap (joinWord i) (Sparse.bits c)
    chunkToList (Dense i c) = fmap (joinWord i) (Dense.bits c)

-- * Modify bitmaps

-- | Set a 'Bit' in a 'Bitmap'.
--
-- prop> \i -> pop (setBit i mempty) == 1
-- prop> \m i -> testBit i (setBit i m)
setBit :: Word32 -> Bitmap -> Bitmap
setBit w bm =
    let (ix, bt) = splitWord w
    in v_alter ix (set ix bt) bm
  where
    set :: Index -> Bit -> Maybe Chunk -> Maybe Chunk
    set ix b c =
      Just $ case c of
        Nothing -> Sparse ix (Sparse.set mempty b)
        Just (Dense i c) -> Dense i (Dense.set c b)
        Just (Sparse i c) -> Sparse i (Sparse.set c b)

-- | Clear a bit in a 'Bitmap'.
--
-- prop> \b m -> not (testBit b (clearBit b m))
-- prop> \b -> mempty == (clearBit b (setBit b mempty))
clearBit :: Word32 -> Bitmap -> Bitmap
clearBit w bm =
    let (ix, bt) = splitWord w
    in (v_alter ix (clear ix bt) bm)
  where
    clear ix b c =
      case c of
        Nothing -> Nothing
        Just (Dense i c) -> Just (Dense i (Dense.clear c b))
        Just (Sparse i c) ->
          let c' = Sparse.clear c b
          in if Sparse.pop c' == 0
             then Nothing
             else Just (Sparse i c')

-- * Utility

-- | Modify a 'Chunk' in a 'Bitmap'.
v_alter :: Index -> (Maybe Chunk -> Maybe Chunk) -> Bitmap -> Bitmap
v_alter ix f bm@(Bitmap m) =
    let r = searchIndex chunkIndex ix m
        (c, p) = either (\p -> (Nothing, p)) (first Just) r
        c' = f c
    in case (c, c') of
      (Nothing, Nothing) -> bm
      (Nothing, Just c) -> insert c bm
      (Just _, Nothing) -> delete p bm
      (Just c, Just c') -> replace p c' bm
  where
    insert c (Bitmap m) = Bitmap (v_sort (V.cons c m))
    replace p c (Bitmap m) = Bitmap (m V.// [(p, c)])
    delete p (Bitmap m) =
      let l = V.take p m
          r = V.drop (p+1) m
      in Bitmap (l V.++ r)
