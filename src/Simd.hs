{-# language
      BangPatterns
    , MagicHash
    , ScopedTypeVariables
    , TypeApplications
  #-}

module Simd
  ( xor
  , xorMutable
  ) where

import Control.Monad.ST
import Data.Primitive
import Data.Primitive.Unlifted.Array
import Data.Word
import GHC.Exts
import Simd.Internal

import qualified Data.Bits as Bits

lengthBytes :: forall a. Prim a => PrimArray a -> Int
lengthBytes p = sizeofPrimArray p * sizeOf @a undefined

unInt :: Int -> Int#
unInt (I# i#) = i#

unByteArray :: ByteArray -> ByteArray#
unByteArray (ByteArray b#) = b#

xor :: ()
  => ByteArray
  -> ByteArray
  -> ByteArray
xor a b = runST (unsafeFreezeByteArray =<< xorMutable a b)
{-# inline xor #-}

xorMutable :: ()
  => ByteArray
  -> ByteArray
  -> ST s (MutableByteArray s)
xorMutable a b = do
  let lenA = sizeofByteArray a
  let lenB = sizeofByteArray b
  if lenA == lenB
    then do
      m@(MutableByteArray target#) <- newByteArray lenA
      avx2_xor_bits target# (unInt lenA) (unByteArray a) (unByteArray b)
      pure m
    else error $ "xorMutable: length mismatch! " ++ show lenA ++ " vs " ++ show lenB
{-# inline xorMutable #-}
