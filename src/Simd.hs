{-# language
      BangPatterns
    , MagicHash
    , ScopedTypeVariables
    , TypeApplications
  #-}

module Simd
  ( Simd.xor
  , Simd.xorMutable
  , Simd.or
  , Simd.orMutable
  ) where

import Control.Monad.ST
import Data.Primitive
--import Data.Primitive.Unlifted.Array
import GHC.Exts
import Simd.Internal

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
    else error $ lengthMismatch "xorMutable" lenA lenB
{-# inline xorMutable #-}

or :: ()
  => ByteArray
  -> ByteArray
  -> ByteArray
or a b = runST (unsafeFreezeByteArray =<< orMutable a b)
{-# inline or #-}

orMutable :: ()
  => ByteArray
  -> ByteArray
  -> ST s (MutableByteArray s)
orMutable a b = do
  let lenA = sizeofByteArray a
  let lenB = sizeofByteArray b
  if lenA == lenB
    then do
      m@(MutableByteArray target#) <- newByteArray lenA
      avx2_or_bits target# (unInt lenA) (unByteArray a) (unByteArray b)
      pure m
    else error $ lengthMismatch "orMutable" lenA lenB
{-# inline orMutable #-}

lengthMismatch :: String -> Int -> Int -> String
lengthMismatch fun lenA lenB = fun
  ++ ": length mismatch! "
  ++ show lenA
  ++ show lenB

