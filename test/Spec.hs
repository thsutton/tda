{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad
import qualified Data.Vector    as V
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           System.Exit

import           Lib

genSimplex :: Gen (Simplex 4)
genSimplex = Simplex . V.fromList <$> Gen.list (Range.linear 1 50) (Gen.word64 (Range.linear 0 100))

-- | It is required (and true) that the boundary of a boundary is empty.
prop_boundOfBoundIsEmpty :: Property
prop_boundOfBoundIsEmpty = property $ do
  simplex <- forAll genSimplex
  0 === chainLength (boundary (boundary (SimplexC @ Z2 simplex)))

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main = do
  r <- tests
  unless r $ exitFailure
