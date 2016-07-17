{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Set        as S
import           Data.Word
import           System.Exit
import           Test.QuickCheck

import Data.Leonine

prop_population :: [Word32] -> Bool
prop_population input =
  bits (foldl (flip setBit) mempty input) == nub (sort input)

--
-- Use Template Haskell to automatically run all of the properties above.
--

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
    result <- runTests
    unless result exitFailure
