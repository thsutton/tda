{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import           Data.Function
import           Data.Group
import           Data.List
import           Data.Modular
import           Data.Monoid
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Data.Word
import           GHC.TypeLits

class Field n where
  -- | Addition; associative, commutative
  fplus :: n -> n -> n
  -- | Additive identity.
  fzero :: n
  -- | Additive inverse.
  fnegate :: n -> n

  -- | Multiplication: associative, commutative
  fmult :: n -> n -> n
  -- | Multiplicative identity
  fone :: n
  -- | Multiplicative inverse.
  frecip :: n -> n

instance (Num n, Fractional n) => Field n where
  fplus = (+)
  fzero = 0
  fnegate = negate

  fmult = (*)
  fone = 1
  frecip = recip

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

instance Ord Z2 where
  compare Z I = LT
  compare I Z = GT
  compare _ _ = EQ

instance Show Z2 where
  show Z = "0"
  show I = "1"

instance Monoid Z2 where
  mempty = I
  mappend = (*)

instance Group Z2 where
  invert = id

instance Abelian Z2

-- * Dimension

class Dimension o where
  dim :: o -> Integer

-- * Simplices

type Point = Word64

-- | A k-simplex is an ordered set of k points.
data Simplex (k :: Nat) = Simplex { points :: Vector Point }
  deriving (Eq, Ord)

instance KnownNat k => Dimension (Simplex k) where
  dim s = natVal s

-- | Smart constructor
simplex :: Vector Point -> Maybe (Simplex k)
simplex v =
  let v' = V.fromList . nub . V.toList $ v
  in if v' == v then Just (Simplex v') else Nothing

isFaceOf :: Simplex k -> Simplex (k + 1) -> Bool
isFaceOf (Simplex as) (Simplex bs) = V.all (\a -> V.elem a bs) as

deriving instance Show (Simplex k)

-- * Chains

-- | A k-chain is the formal sum of a finite number of oriented
-- k-simplices with coefficients from some Abelian group.
--
-- \( c^{(k)} = \sum_i a_i \sigma_i^{(k)} \)
data Chain c k where
  Chain :: (Eq c, Ord c, Num c, Abelian c) => Vector (c, Simplex k) -> Chain c k

chainLength :: Chain c k -> Int
chainLength = V.length . links

instance KnownNat k => Dimension (Chain c k) where
  dim chain = natVal chain

links :: Chain c k -> Vector (c, Simplex k)
links (Chain es) = es

multiplyChain :: (Num c, Abelian c) => c -> Chain c k -> Chain c k
multiplyChain c (Chain es) =
  let es' = es
  in simplifyChain $ Chain es'

deriving instance Show c => Show (Chain c k)

-- | Canonicalise a 'Chain'.
--
-- This is pretty much the same as simplifying a polynomial with
-- simplicies instead of variables.
--
-- TODO: Avoid implementing this with lists.
simplifyChain :: (Num c, Abelian c) => Chain c k -> Chain c k
simplifyChain (Chain es) =
  let el = sortOn snd . V.toList $ es
      el' = groupBy ((==) `on` snd) el
      el'' = map (\l -> (sum (map fst l), snd $ head l) ) el'
  in Chain (V.fromList . filter ( (/= 0) . fst) $ el'')

-- * Boundaries

-- | Objects can be mapped to a chain of their boundary faces
--
-- For a simplex this is constructing a chain of the faces:
--
-- \[ \partial^{(k)}(\sigma^{(k)}) = \sum_{i=0}^{k}(−1)^{i}[x_0,\ldots,\hat{x_i},\ldots,x_k] \]
--
-- And the linear extension defines the boundary for a chain:
--
-- \[ \partial^{(k)}(\sum_i a_i \sigma_i^{(k)}) = \sum_i a_i \partial^{(k)}(\sigma_i^{(k)}) \]
class Boundary (s :: * -> Nat -> *) where
  boundary :: forall c k. (Ord c, Num c, Abelian c) => s c (k + 1) -> Chain c k

-- | We need a wrapper that can ignore the coefficient type parameter.
newtype SimplexC c k = SimplexC { unwrap :: Simplex k }

-- | Take the faces of a 'Simplex'.
instance Boundary SimplexC where
  boundary :: (Ord c, Num c, Abelian c) => SimplexC c (k + 1) -> Chain c k
  boundary (SimplexC (Simplex ps)) =
      simplifyChain . Chain . V.imap (\i _ -> elem i) $ ps
    where
      elem :: (Abelian c, Num c) => Int -> (c, Simplex k)
      elem i = (pow (-1) i, Simplex (delete i ps))
      delete = \i v -> (V.slice 0 i v) <> (V.drop (i + 1) v)

-- | Extend the simplex case linearly.
instance Boundary Chain where
  boundary (Chain es) =
    let
      es' = es >>= (\(c,s) -> links . multiplyChain c $ boundary (SimplexC s))
    in simplifyChain (Chain es')

