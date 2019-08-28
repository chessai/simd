{-# language CPP #-}
{-# language MagicHash #-}
{-# language UnliftedFFITypes #-}

module Simd.Internal
  ( -- avx2_memcpy
    -- avx2_cmpeq8
    -- avx2_cmpeq8_para
    avx2_and_bits
  , avx2_nand_bits
  , avx2_not_bits
  , avx2_or_bits
  , avx2_xor_bits
  , length_bytearray
  ) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeIOToST)
import GHC.Exts

-- ghc's NCG memcpy compiles to Int#el's specialised memcpy
-- instruction that should be much faster than AVX2's 128-bit
-- memcpy.

--foreign import ccall unsafe "simd.h avx2_mempcy"
--  avx2_memcpy :: ByteArray# -> ByteArray# -> Int# -> IO ()

{-
foreign import ccall unsafe "simd.h avx2_cmpeq8"
  avx2_cmpeq8 :: ()
    => Word8 -- ^ byte
    -> ByteArray# -- ^ target array
    -> Int# -- ^ target length
    -> ByteArray# -- ^ source array
    -> IO ()

foreign import ccall unsafe "simd.h avx2_cmpeq8_para"
  avx2_cmpeq8_para :: ()
    => ByteArray#
    -> Int#
    -> Ptr (Ptr Word8)
    -> Int#
    -> Ptr Word8
    -> IO ()
-}

foreign import ccall "simd_foo.h length_bytearray"
  length_bytearray :: ()
    => ByteArray#
    -> IO Int

foreign import ccall unsafe "simd.h avx2_and_bits"
  avx2_and_bits_internal :: ()
    => MutableByteArray# s -- ^ target
    -> Int# -- ^ target length
    -> ByteArray# -- ^ source a
    -> ByteArray# -- ^ source b
    -> IO ()

foreign import ccall unsafe "simd.h avx2_and_not_bits"
  avx2_nand_bits_internal :: ()
    => MutableByteArray# s -- ^ target
    -> Int# -- ^ target length
    -> ByteArray# -- ^ source a
    -> ByteArray# -- ^ source b
    -> IO ()

foreign import ccall unsafe "simd.h avx2_not_bits"
  avx2_not_bits_internal :: ()
    => MutableByteArray# s -- ^ target
    -> Int# -- ^ target length
    -> ByteArray# -- ^ source
    -> IO ()

foreign import ccall unsafe "simd.h avx2_or_bits"
  avx2_or_bits_internal :: ()
    => MutableByteArray# s -- ^ target
    -> Int# -- ^ target length
    -> ByteArray# -- ^ source a
    -> ByteArray# -- ^ source b
    -> IO ()

foreign import ccall unsafe "simd_foo.h avx2_xor_bits_foo"
  avx2_xor_bits_internal :: ()
    => MutableByteArray# s -- ^ target
    -> Int# -- ^ target length
    -> ByteArray# -- ^ source a
    -> ByteArray# -- ^ source b
    -> IO ()

avx2_and_bits :: ()
  => MutableByteArray# s -- ^ target
  -> Int# -- ^ target length
  -> ByteArray# -- ^ source a
  -> ByteArray# -- ^ source b
  -> ST s ()
avx2_and_bits a b c d = unsafeIOToST (avx2_and_bits_internal a b c d)
{-# inline avx2_and_bits #-}

avx2_nand_bits :: ()
  => MutableByteArray# s -- ^ target
  -> Int# -- ^ target length
  -> ByteArray# -- ^ source a
  -> ByteArray# -- ^ source b
  -> ST s ()
avx2_nand_bits a b c d = unsafeIOToST (avx2_nand_bits_internal a b c d)
{-# inline avx2_nand_bits #-}

avx2_not_bits :: ()
  => MutableByteArray# s
  -> Int#
  -> ByteArray#
  -> ST s ()
avx2_not_bits a b c = unsafeIOToST (avx2_not_bits_internal a b c)
{-# inline avx2_not_bits #-}

avx2_or_bits :: ()
  => MutableByteArray# s -- ^ target
  -> Int# -- ^ target length
  -> ByteArray# -- ^ source a
  -> ByteArray# -- ^ source b
  -> ST s ()
avx2_or_bits a b c d = unsafeIOToST (avx2_or_bits_internal a b c d)
{-# inline avx2_or_bits #-}

avx2_xor_bits :: ()
  => MutableByteArray# s
  -> Int#
  -> ByteArray#
  -> ByteArray#
  -> ST s ()
avx2_xor_bits a b c d = unsafeIOToST (avx2_xor_bits_internal a b c d)
{-# inline avx2_xor_bits #-}

