{-# language
        BangPatterns
      , MagicHash
      , RankNTypes
      , ScopedTypeVariables
  #-}

module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.ST (ST, runST,stToIO)
import Data.Primitive.ByteArray
import Data.Primitive.Contiguous
import Data.Primitive.PrimArray
import Data.Word
import Gauge.Main
import System.Random
import System.Random.Shuffle

import qualified Data.Bits as Bits
import qualified Data.Primitive.Contiguous as C
import qualified Gauge.Main.Options as G
import qualified Simd

import Prelude hiding (or)

main :: IO ()
main = do
  !arr0_1000 <- randomByteArray 1000
  !arr1_1000 <- randomByteArray 1000
  !arr0_10000 <- randomByteArray 10000
  !arr1_10000 <- randomByteArray 10000
  !arr0_25600 <- randomByteArray 25600
  !arr1_25600 <- randomByteArray 25600

  let !arr0_1000_aligned = align32 arr0_1000
      !arr1_1000_aligned = align32 arr1_1000
      !arr0_10000_aligned = align32 arr0_10000
      !arr1_10000_aligned = align32 arr1_10000
      !arr0_25600_aligned = align32 arr0_25600
      !arr1_25600_aligned = align32 arr1_25600

  !aligned_buf_1000 <- newAlignedPinnedByteArray 1000 32
  !aligned_buf_10000 <- newAlignedPinnedByteArray 10000 32
  !aligned_buf_25600 <- newAlignedPinnedByteArray 25600 32

  let !test_byte = 20

  defaultMainWith gaugeCfg $
    [ bgroup "xor: simd-accelerated"
        [ bench "1,000" $ whnf xor (arr0_1000, arr1_1000)
        , bench "10,000" $ whnf xor (arr0_10000, arr1_10000)
        , bench "25,600" $ whnf xor (arr0_25600, arr1_25600)
        ]
    , bgroup "xor: simd-accelerated-aligned"
        [ bench "1,000" $ whnfIO
            (stToIO $ Simd.xorInto aligned_buf_1000 arr0_1000_aligned arr1_1000_aligned 1000)
        , bench "10,000" $ whnfIO
            (stToIO $ Simd.xorInto aligned_buf_10000 arr0_10000_aligned arr1_10000_aligned 10000)
        , bench "25,600" $ whnfIO
            (stToIO $ Simd.xorInto aligned_buf_25600 arr0_25600_aligned arr1_25600_aligned 25600)
        ]
    , bgroup "xor: naive"
        [ bench "1,000" $ whnf xorNaive (arr0_1000, arr1_1000)
        , bench "10,000" $ whnf xorNaive (arr0_10000, arr1_10000)
        , bench "25,600" $ whnf xorNaive (arr0_25600, arr1_25600)
        ]
    , bgroup "or: simd-accelerated"
        [ bench "1,000" $ whnf or (arr0_1000, arr1_1000)
        , bench "10,000" $ whnf or (arr0_10000, arr1_10000)
        , bench "25,600" $ whnf or (arr0_25600, arr1_25600)
        ]
    , bgroup "or: naive"
        [ bench "1,000" $ whnf orNaive (arr0_1000, arr1_1000)
        , bench "10,000" $ whnf orNaive (arr0_10000, arr1_10000)
        , bench "25,600" $ whnf orNaive (arr0_25600, arr1_25600)
        ]
    , bgroup "equal: simd-accelerated"
        [ bench "1,000" $ whnf (equal test_byte) arr0_1000
        , bench "10,000" $ whnf (equal test_byte) arr0_10000
        , bench "25,600" $ whnf (equal test_byte) arr0_25600
        ]
    , bgroup "equal: naive"
        [ bench "1,000" $ whnf (equalNaive test_byte) arr0_1000
        , bench "10,000" $ whnf (equalNaive test_byte) arr0_10000
        , bench "25,600" $ whnf (equalNaive test_byte) arr0_25600
        ]
    ]

align32 :: ByteArray -> ByteArray
align32 src = runST $ do
  dst <- newAlignedPinnedByteArray (sizeofByteArray src) 32
  copyByteArray dst 0 src 0 (sizeofByteArray src)
  unsafeFreezeByteArray dst

gaugeCfg :: G.Config
gaugeCfg = G.defaultConfig

randomList :: Int -> IO [Word8]
randomList sz = shuffleM
  =<< replicateM sz (randomRIO (minBound,maxBound))

randomByteArray :: Int -> IO ByteArray
randomByteArray sz = byteArrayFromListN sz <$> randomList sz

xor :: (ByteArray,ByteArray) -> ByteArray
xor = uncurry Simd.xor
{-# noinline xor #-}

xorNaive :: (ByteArray,ByteArray) -> ByteArray
xorNaive (ByteArray b0#, ByteArray b1#) =
  let arr0 = PrimArray b0# :: PrimArray Word8
      arr1 = PrimArray b1# :: PrimArray Word8
  in case C.zipWith Bits.xor arr0 arr1 of
    PrimArray b2# -> ByteArray b2#
{-# noinline xorNaive #-}

or :: (ByteArray,ByteArray) -> ByteArray
or = uncurry Simd.or
{-# noinline or #-}

orNaive :: (ByteArray,ByteArray) -> ByteArray
orNaive (ByteArray b0#, ByteArray b1#) =
  let arr0 = PrimArray b0# :: PrimArray Word8
      arr1 = PrimArray b1# :: PrimArray Word8
  in case C.zipWith (Bits..|.) arr0 arr1 of
    PrimArray b2# -> ByteArray b2#
{-# noinline orNaive #-}

equal :: Word8 -> ByteArray -> ByteArray
equal = Simd.equal
{-# noinline equal #-}

equalNaive :: Word8 -> ByteArray -> ByteArray
equalNaive byte arr = runST run where
  run :: forall s. ST s ByteArray
  run = do
    let len = sizeofByteArray arr
    m :: MutableByteArray s <- newByteArray len
    let go :: Int -> ST s ()
        go !ix = if ix < len
          then do
            let val = indexByteArray arr ix
            writeByteArray m ix (if val == byte then 1 else 0 :: Word8)
            go (ix + 1)
          else pure ()
    go 0
    unsafeFreezeByteArray m
{-# noinline equalNaive #-}

