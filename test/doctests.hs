import           Test.DocTest

main :: IO ()
main =
  doctest [ "-ilib"
          , "lib/Data/Leonine/Dense.hs"
          , "lib/Data/Leonine/Sparse.hs"
          , "lib/Data/Leonine.hs"
          ]
