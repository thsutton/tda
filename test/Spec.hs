{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad
import           Data.Monoid
import qualified Data.Set       as S
import qualified Data.Vector    as V
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           System.Exit

import           Lib

genSimplex :: Gen (Simplex 4)
genSimplex = Simplex . V.fromList . S.toList <$> Gen.set (Range.linear 1 50) (Gen.word64 (Range.linear 0 100))

prop_boundOfBoundIsEmptyZ2 :: Property
prop_boundOfBoundIsEmptyZ2 = property $ do
  simplex <- forAll genSimplex
  0 === chainLength (boundary (boundary (SimplexC @ Z2 simplex)))

prop_boundOfBoundIsEmptyFloat :: Property
prop_boundOfBoundIsEmptyFloat = property $ do
  simplex <- forAll genSimplex
  0 === chainLength (boundary (boundary (SimplexC @ (Sum Float) simplex)))

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main = do
  r <- tests
  unless r $ exitFailure
