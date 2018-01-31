{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Lib where

import           Data.Group
import           Data.List
import           Data.Modular
import           Data.Monoid
import           Data.Vector  (Vector)
import qualified Data.Vector  as V
import           GHC.TypeLits

-- | Z/2
data Z2 = Z | I deriving (Eq)

instance Num Z2 where
  fromInteger i = if even i then Z else I

  Z + I = I
  I + Z = I
  _ + _ = Z

  I * I = I
  _ * _ = Z

  (-) = (+)
  abs = id
  signum = id

instance Show Z2 where
  show Z = "0"
  show I = "1"

instance Monoid Z2 where
  mempty = I
  mappend = (*)

instance Group Z2 where
  invert = id

instance Abelian Z2

type Point = Int

-- | A k-simplex is an ordered set of k points.
data Simplex (k :: Nat) = Simplex (Vector Point)
  deriving (Eq, Ord)

deriving instance Show (Simplex k)

-- | A k-chain is the formal sum of a finite number of oriented
-- k-simplices with coefficients from some Abelian group.
--
-- \( c^{(k)} = \sum_i a_i \sigma_i^{(k)} \)
data Chain c k where
  Chain :: (Num c, Abelian c) => Vector (c, Simplex k) -> Chain c k

deriving instance Show c => Show (Chain c k)

-- | The boundary operator maps a k-simplex onto the sum of the
-- \(k-1\)-simplices in its boundary.
--
-- \(\partial^{(k)}(\sigma^{(k)}) = \sum_{i=0}^{k}(−1)^{i}[x_0,\ldots,\hat{x_i},\ldots,x_k] \)
simplexBoundary :: (Eq c, Num c, Abelian c) => Simplex (k + 1) -> Chain c k
simplexBoundary (Simplex ps) =
    Chain . V.filter ((/= 0) . fst) . V.imap (\i _ -> elem i) $ ps
  where
    elem :: (Num c, Abelian c) => Int -> (c, Simplex k)
    elem i = (pow (-1) i, Simplex (delete i ps))
    delete = \i v -> (V.slice 0 i v) <> (V.drop (i + 1) v)

-- | The boundary of a k-chain is a \(k-1\)-chain obtained linear
-- extension from its action on the k-simplices.
--
-- \( \partial^{(k)}(\sum_i a_i \sigma_i^{(k)}) = \sum_i a_i \partial^{(k)}(\sigma_i^{(k)}) \)
chainBoundary :: Chain c (k + 1) -> Chain c k
chainBoundary (Chain es) = error "Nope"

someFunc :: IO ()
someFunc = putStrLn "lol"
