{-# language
        BangPatterns
      , MagicHash
      , TypeApplications
  #-}

module Main (main) where

import Control.Monad (replicateM)
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

main :: IO ()
main = do
  !arr0_1000 <- randomByteArray 1000
  !arr1_1000 <- randomByteArray 1000
  !arr0_10000 <- randomByteArray 10000
  !arr1_10000 <- randomByteArray 10000

  defaultMainWith gaugeCfg $
    [ bgroup "xor: simd-accelerated"
        [ bench "1000" $ whnf xor (arr0_1000, arr1_1000)
        , bench "10000" $ whnf xor (arr0_10000, arr1_10000)
        ]
    , bgroup "xor: naive"
        [ bench "1000" $ whnf xorNaive (arr0_1000, arr1_1000)
        , bench "10000" $ whnf xorNaive (arr0_10000, arr1_10000)
        ]
    ]

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

{-
filterV :: Parity -> Vector.Vector Int -> ()
filterV p v = let !v' = Vector.filter (par p) v in ()
{-# noinline filterV #-}

filterVU :: Parity -> VectorUnboxed.Vector Int -> ()
filterVU p v = let !v' = VectorUnboxed.filter (par p) v in ()
{-# noinline filterVU #-}

filterVS :: Parity -> VectorStorable.Vector Int -> ()
filterVS p v = let !v' = VectorStorable.filter (par p) v in ()
{-# noinline filterVS #-}

filterVP :: Parity -> VectorPrimitive.Vector Int -> ()
filterVP p v = let !v' = VectorPrimitive.filter (par p) v in ()
{-# noinline filterVP #-}

filterArray :: Parity -> Array Int -> ()
filterArray p a = let !a' = C.filter (par p) a in ()
{-# noinline filterArray #-}

filterSmallArray :: Parity -> SmallArray Int -> ()
filterSmallArray p a = let !a' = C.filter (par p) a in ()
{-# noinline filterSmallArray #-}

filterPrimArray :: Parity -> PrimArray Int -> ()
filterPrimArray p a = let !a' = C.filter (par p) a in ()
{-# noinline filterPrimArray #-}
-}
