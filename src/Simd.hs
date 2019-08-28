{-# language
      BangPatterns
    , MagicHash
    , ScopedTypeVariables
    , TypeApplications
  #-}

module Simd
  ( xor
  ) where

import Control.Monad.ST
import Data.Primitive
import Data.Primitive.Unlifted.Array
import Data.Word
import GHC.Exts
import Simd.Internal

lengthBytes :: forall a. Prim a => PrimArray a -> Int
lengthBytes p = sizeofPrimArray p * sizeOf @a undefined

unInt :: Int -> Int#
unInt (I# i#) = i#

xor :: forall a. (Prim a)
  => PrimArray a -- ^ source a
  -> PrimArray a -- ^ source b
  -> PrimArray a -- ^ target
xor a@(PrimArray a#) b@(PrimArray b#) = runST $ if lenA == lenB
  then do
    m@(MutableByteArray target#) <- newByteArray lenA
    avx2_xor_bits target# (unInt lenA) a# b#
    ByteArray b# <- unsafeFreezeByteArray m
    pure (PrimArray b#)
  else error "xor: length mismatch!"
  where
    lenA = sizeofPrimArray a
    lenB = sizeofPrimArray b
