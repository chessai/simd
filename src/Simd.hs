{-# language
      BangPatterns
    , MagicHash
    , RankNTypes
    , ScopedTypeVariables
    , TypeApplications
  #-}

module Simd
  ( Simd.xor
  , Simd.xorMutable
  , Simd.xorInto
  , Simd.or
  , Simd.orMutable
  , Simd.and
  , Simd.andMutable
  , Simd.nand
  , Simd.nandMutable

  , Simd.equal
  , Simd.equalMutable
  ) where

import Control.Monad.ST
import Data.Primitive
import Data.Word
--import Data.Primitive.Unlifted.Array
import GHC.Exts
import Simd.Internal

purify :: ()
  => (forall s. ByteArray -> ByteArray -> ST s (MutableByteArray s))
  -> ByteArray
  -> ByteArray
  -> ByteArray
purify s b0 b1 = runST (unsafeFreezeByteArray =<< s b0 b1)
{-# inline purify #-}

unInt :: Int -> Int#
unInt (I# i#) = i#

unByteArray :: ByteArray -> ByteArray#
unByteArray (ByteArray b#) = b#

xor :: ()
  => ByteArray
  -> ByteArray
  -> ByteArray
xor = purify xorMutable
{-# inline xor #-}

xorMutable :: ()
  => ByteArray
  -> ByteArray
  -> ST s (MutableByteArray s)
xorMutable = binop "xorMutable" avx2_xor_bits
{-# inline xorMutable #-}

xorInto :: ()
  => MutableByteArray s
  -> ByteArray
  -> ByteArray
  -> Int
  -> ST s ()
xorInto (MutableByteArray dst) (ByteArray a) (ByteArray b) (I# len) =
  avx2_or_bits dst len a b

or :: ()
  => ByteArray
  -> ByteArray
  -> ByteArray
or = purify orMutable
{-# inline or #-}

orMutable :: ()
  => ByteArray
  -> ByteArray
  -> ST s (MutableByteArray s)
orMutable = binop "orMutable" avx2_or_bits
{-# inline orMutable #-}

and :: ()
  => ByteArray
  -> ByteArray
  -> ByteArray
and = purify andMutable
{-# inline and #-}

andMutable :: ()
  => ByteArray
  -> ByteArray
  -> ST s (MutableByteArray s)
andMutable = binop "andMutable" avx2_and_bits
{-# inline andMutable #-}

nand :: ()
  => ByteArray
  -> ByteArray
  -> ByteArray
nand = purify nandMutable
{-# inline nand #-}

nandMutable :: ()
  => ByteArray
  -> ByteArray
  -> ST s (MutableByteArray s)
nandMutable = binop "nandMutable" avx2_nand_bits
{-# inline nandMutable #-}

binop :: ()
  => String -- ^ name of function, for error msg
  -> (MutableByteArray# s -> Int# -> ByteArray# -> ByteArray# -> ST s ())
  -> ByteArray
  -> ByteArray
  -> ST s (MutableByteArray s)
binop err avx2 = \a b -> do
  let lenA = sizeofByteArray a
  let lenB = sizeofByteArray b
  if lenA == lenB
    then do
      m@(MutableByteArray target#) <- newByteArray lenA
      avx2 target# (unInt lenA) (unByteArray a) (unByteArray b)
      pure m
    else error $ lengthMismatch err lenA lenB
{-# inline binop #-}

lengthMismatch :: String -> Int -> Int -> String
lengthMismatch fun lenA lenB = fun
  ++ ": length mismatch! "
  ++ show lenA
  ++ " vs "
  ++ show lenB

equal :: ()
  => Word8
  -> ByteArray
  -> ByteArray
equal = \byte b -> runST (unsafeFreezeByteArray =<< equalMutable byte b)
{-# inline equal #-}

equalMutable :: ()
  => Word8
  -> ByteArray
  -> ST s (MutableByteArray s)
equalMutable = \byte b -> do
  let len = sizeofByteArray b
  m@(MutableByteArray target#) <- newByteArray len
  avx2_cmpeq8 byte target# (unInt len) (unByteArray b)
  pure m
{-# inline equalMutable #-}

