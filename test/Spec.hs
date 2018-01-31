{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Lib

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- | It is required (and true) that the boundary of a boundary is empty.
boundOfBoundIsEmpty :: Simplex 4 -> Bool
boundOfBoundIsEmpty simplex =
  0 == chainLength (boundary (boundary (SimplexC @ Z2 simplex)))
