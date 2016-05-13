{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Data.Leonine.Utils where

import           Data.Bits
import           Data.Either                 (either)
import           Data.Function               (on)
import           Data.Monoid
import qualified Data.Vector.Algorithms.Heap as VA
import           Data.Vector.Generic         (Vector)
import qualified Data.Vector.Generic         as V
import qualified Data.Vector.Generic.Mutable as V (write)
import           Data.Word

-- * Words

type Index = Word16

type Bit = Word16

-- | Split a 'Word32' into a 'Chunk' 'Index' and a 'Bit'.
--
-- >>> splitWord 0xFF000011
-- (65280,17)
-- >>> splitWord 0x000FF000
-- (15,61440)
splitWord :: Word32 -> (Index, Bit)
splitWord w = ( fromIntegral $ (w .&. 0xFFFF0000) `shiftR` 16
              , fromIntegral $  w .&. 0xFFFF
              )

-- * Vectors

-- | Search a 'Vector', sorted according to some index type 'b'.
--
-- >>> search length 3 (Data.Vector.fromList ["","-","---","-----"])
-- Just "---"
-- >>> search length 2 (Data.Vector.fromList ["","-","---","-----"])
-- Nothing
search :: (Ord ix, Vector v a) => (a -> ix) -> ix -> v a -> Maybe a
search index ix v =
  either (const Nothing) (Just . fst) (searchBoundsOn index v ix 0 (V.length v))

-- | Search a 'Vector', sorted according to some index, within the bounds [l,u].
--
-- The element and index are returned when a match is found, otherwise
-- the index at which it could be inserted.
--
-- >>> searchBoundsOn head (Data.Vector.fromList ["as","bs","cs","ds"]) 'c' 0 4
-- Right ("cs",2)
-- >>> searchBoundsOn head (Data.Vector.fromList ["as","bs","cs","ds"]) 'b' 0 4
-- Right ("bs",1)
-- >>> searchBoundsOn head (Data.Vector.fromList ["as","bs","cs","ds"]) 'Z' 0 4
-- Left 0
-- >>> searchBoundsOn head (Data.Vector.fromList ["as","bs","cs","ds"]) 'e' 0 4
-- Left 4
searchBoundsOn :: (Ord ix, Vector v a) => (a -> ix) -> v a -> ix -> Int -> Int -> Either Int (a, Int)
searchBoundsOn index v ix = loop
  where
    loop !l !u
      | u <= l = Left l
      | otherwise = let e' = v V.! k
                    in case compare (index e') ix of
                      EQ -> Right (e', k)
                      LT -> loop (k+1) u
                      GT -> loop l     k
      where k = (u + l) `shiftR` 1

-- | Modify a /sorted/ 'Vector', indexed by 'b'.
--
-- The supplied function can insert, modify, or delete the element
-- which corresponds to the supplied index:
--
-- >>> Data.Vector.toList $ modify fst 'c' (Data.Vector.fromList [('a',1),('c',3),('d',4)]) (fmap (fmap (10+)))
-- [('a',1),('c',13),('d',4)]
--
-- >>> Data.Vector.toList $ modify fst 'b' (Data.Vector.fromList [('a',1),('c',3),('d',4)]) (const $ Just ('b',9))
-- [('a',1),('b',9),('c',3),('d',4)]
--
-- >>> Data.Vector.toList $ modify fst 'a' (Data.Vector.fromList [('a',1),('c',3),('d',4)]) (const Nothing)
-- [('c',3),('d',4)]
-- >>> Data.Vector.toList $ modify fst 'c' (Data.Vector.fromList [('a',1),('c',3),('d',4)]) (const Nothing)
-- [('a',1),('d',4)]
-- >>> Data.Vector.toList $ modify fst 'd' (Data.Vector.fromList [('a',1),('c',3),('d',4)]) (const Nothing)
-- [('a',1),('c',3)]
--
-- >>> modify fst 'b' (Data.Vector.fromList [('a',1),('c',3),('d',4)]) (const Nothing)
-- [('a',1),('c',3),('d',4)]
modify :: (Ord b, Vector v a)
       => (a -> b) -- ^ Index an element.
       -> b -- ^ The index to search for.
       -> v a -- ^ The vector to modify.
       -> (Maybe a -> Maybe a)
       -> v a
modify index ix v f =
    let r = searchBoundsOn index v ix 0 len
    in go r
  where
    cmp = compare `on` index
    len = V.length v
    go (Left i) = case f Nothing of
      Just e' -> V.modify (VA.sortBy cmp) (e' `V.cons` v)
      Nothing -> v
    go (Right (r,i)) = case f (Just r) of
      Just e' -> V.modify (\v -> V.write v i e') v
      Nothing -> let i' = i + 1
                in (V.slice 0 i v) V.++ (V.slice i' (len - i') v)
