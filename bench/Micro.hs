{-# language
        BangPatterns
      , TypeApplications
  #-}

module Main (main) where

import Control.Monad ((>=>), replicateM)
import Data.Primitive.Contiguous
import Gauge.Main
import System.Random
import System.Random.Shuffle

import qualified Data.List as List
import qualified Data.Primitive.Contiguous as C
import qualified Gauge.Main.Options as G

main :: IO ()
main = do
  arr1000 <-
  v1000 <- randomV 1000
  v10000 <- randomV 10000

  vu1000 <- randomVU 1000
  vu10000 <- randomVU 10000
  vu100000 <- randomVU 100000

  vs1000 <- randomVS 1000
  vs10000 <- randomVS 10000
  vs100000 <- randomVS 100000

  vp1000 <- randomVP 1000
  vp10000 <- randomVP 10000
  vp100000 <- randomVP 100000

  array1000 <- randomC @Array 1000
  array10000 <- randomC @Array 10000
  array100000 <- randomC @Array 100000

  smallArray1000 <- randomC @SmallArray 1000
  smallArray10000 <- randomC @SmallArray 10000
  smallArray100000 <- randomC @SmallArray 100000

  primArray1000 <- randomC @PrimArray 1000
  primArray10000 <- randomC @PrimArray 10000
  primArray100000 <- randomC @PrimArray 100000

  let ev1000    = evenV 1000
      ev10000   = evenV 10000
      ev100000  = evenV 100000

      evu1000   = evenVU 1000
      evu10000  = evenVU 10000
      evu100000 = evenVU 100000

      evs1000   = evenVS 1000
      evs10000  = evenVS 10000
      evs100000 = evenVS 100000

      evp1000   = evenVP 1000
      evp10000  = evenVP 10000
      evp100000 = evenVP 100000

  let earray1000   = evenC @Array 1000
      earray10000  = evenC @Array 10000
      earray100000 = evenC @Array 100000

      esmallArray1000 = evenC @SmallArray 1000
      esmallArray10000 = evenC @SmallArray 10000
      esmallArray100000 = evenC @SmallArray 100000

      eprimArray1000 = evenC @PrimArray 1000
      eprimArray10000 = evenC @PrimArray 10000
      eprimArray100000 = evenC @PrimArray 100000

  defaultMainWith gaugeCfg $
    [ bgroup "filter/random"
      [ bgroup "Vector/filter"
          [ bench "1000" $ whnf (filterV Na) v1000
          , bench "10000" $ whnf (filterV Na) v10000
          , bench "100000" $ whnf (filterV Na) v100000
          ]
       , bgroup "Vector.Unboxed/filter"
           [ bench "1000" $ whnf (filterVU Na) vu1000
           , bench "10000" $ whnf (filterVU Na) vu10000
           , bench "100000" $ whnf (filterVU Na) vu100000
           ]
       , bgroup "Vector.Storable/filter"
           [ bench "1000" $ whnf (filterVS Na) vs1000
           , bench "10000" $ whnf (filterVS Na) vs10000
           , bench "100000" $ whnf (filterVS Na) vs100000
           ]
       , bgroup "Vector.Primitive/filter"
           [ bench "1000" $ whnf (filterVP Na) vp1000
           , bench "10000" $ whnf (filterVP Na) vp10000
           , bench "100000" $ whnf (filterVP Na) vp100000
           ]
       , bgroup "Array/filter"
           [ bench "1000" $ whnf (filterArray Na) array1000
           , bench "10000" $ whnf (filterArray Na) array10000
           , bench "100000" $ whnf (filterArray Na) array100000
           ]
       , bgroup "SmallArray/filter"
           [ bench "1000" $ whnf (filterSmallArray Na) smallArray1000
           , bench "10000" $ whnf (filterSmallArray Na) smallArray10000
           , bench "100000" $ whnf (filterSmallArray Na) smallArray100000
           ]
      , bgroup "PrimArray/filter"
          [ bench "1000" $ whnf (filterPrimArray Na) primArray1000
          , bench "10000" $ whnf (filterPrimArray Na) primArray10000
          , bench "100000" $ whnf (filterPrimArray Na) primArray100000
          ]
      ]
    , bgroup "filter/even_data/even_filter"
      [ bgroup "Vector/filter"
          [ bench "1000" $ whnf (filterV Even) ev1000
          , bench "10000" $ whnf (filterV Even) ev10000
          , bench "100000" $ whnf (filterV Even) ev100000
          ]
      , bgroup "Vector.Unboxed/filter"
          [ bench "1000" $ whnf (filterVU Even) evu1000
          , bench "10000" $ whnf (filterVU Even) evu10000
          , bench "100000" $ whnf (filterVU Even) evu100000
          ]
      , bgroup "Vector.Storable/filter"
          [ bench "1000" $ whnf (filterVS Even) evs1000
          , bench "10000" $ whnf (filterVS Even) evs10000
          , bench "100000" $ whnf (filterVS Even) evs100000
          ]
      , bgroup "Vector.Primitive/filter"
          [ bench "1000" $ whnf (filterVP Even) evp1000
          , bench "10000" $ whnf (filterVP Even) evp10000
          , bench "100000" $ whnf (filterVP Even) evp100000
          ]
      , bgroup "Array/filter"
          [ bench "1000" $ whnf (filterArray Even) earray1000
          , bench "10000" $ whnf (filterArray Even) earray10000
          , bench "100000" $ whnf (filterArray Even) earray100000
          ]
      , bgroup "SmallArray/filter"
          [ bench "1000" $ whnf (filterSmallArray Even) esmallArray1000
          , bench "10000" $ whnf (filterSmallArray Even) esmallArray10000
          , bench "100000" $ whnf (filterSmallArray Even) esmallArray100000
          ]
      , bgroup "PrimArray/filter"
          [ bench "1000" $ whnf (filterPrimArray Even) eprimArray1000
          , bench "10000" $ whnf (filterPrimArray Even) eprimArray10000
          , bench "100000" $ whnf (filterPrimArray Even) eprimArray100000
          ]
      ]
    , bgroup "filter/even_data/odd_filter"
      [ bgroup "Vector/filter"
          [ bench "1000" $ whnf (filterV Odd) ev1000
          , bench "10000" $ whnf (filterV Odd) ev10000
          , bench "100000" $ whnf (filterV Odd) ev100000
          ]
      , bgroup "Vector.Unboxed/filter"
          [ bench "1000" $ whnf (filterVU Odd) evu1000
          , bench "10000" $ whnf (filterVU Odd) evu10000
          , bench "100000" $ whnf (filterVU Odd) evu100000
          ]
      , bgroup "Vector.Storable/filter"
          [ bench "1000" $ whnf (filterVS Odd) evs1000
          , bench "10000" $ whnf (filterVS Odd) evs10000
          , bench "100000" $ whnf (filterVS Odd) evs100000
          ]
      , bgroup "Vector.Primitive/filter"
          [ bench "1000" $ whnf (filterVP Odd) evp1000
          , bench "10000" $ whnf (filterVP Odd) evp10000
          , bench "100000" $ whnf (filterVP Odd) evp100000
          ]
      , bgroup "Array/filter"
          [ bench "1000" $ whnf (filterArray Odd) earray1000
          , bench "10000" $ whnf (filterArray Odd) earray10000
          , bench "100000" $ whnf (filterArray Odd) earray100000
          ]
      , bgroup "SmallArray/filter"
          [ bench "1000" $ whnf (filterSmallArray Odd) esmallArray1000
          , bench "10000" $ whnf (filterSmallArray Odd) esmallArray10000
          , bench "100000" $ whnf (filterSmallArray Odd) esmallArray100000
          ]
      , bgroup "PrimArray/filter"
          [ bench "1000" $ whnf (filterPrimArray Odd) eprimArray1000
          , bench "10000" $ whnf (filterPrimArray Odd) eprimArray10000
          , bench "100000" $ whnf (filterPrimArray Odd) eprimArray100000
          ]
      ]
    ]

  pure ()

gaugeCfg :: G.Config
gaugeCfg = G.defaultConfig {
    G.csvRawFile = Just "bench/data/csvs/filter.csv"
  }

graph :: IO ()
graph = B.bgraph "bench/data/csvs/filter.csv" "Filters" "filter"
  (B.defaultConfig {
    B.outputDir = Just "bench/data/graphs/"
  })

evenList :: Int -> [Int]
evenList sz = List.replicate sz 2

randomList :: Int -> IO [Int]
randomList sz = shuffleM
  =<< replicateM sz (randomRIO (minBound,maxBound))

randomC :: (Contiguous arr, Element arr Int)
  => Int
  -> IO (arr Int)
randomC sz = fromListN sz <$> randomList sz

evenC :: (Contiguous arr, Element arr Int)
  => Int
  -> arr Int
evenC sz = fromListN sz (evenList sz)

evenV :: Int -> Vector.Vector Int
evenV sz = Vector.fromListN sz (evenList sz)

evenVU :: Int -> VectorUnboxed.Vector Int
evenVU sz = VectorUnboxed.fromListN sz (evenList sz)

evenVS :: Int -> VectorStorable.Vector Int
evenVS sz = VectorStorable.fromListN sz (evenList sz)

evenVP :: Int -> VectorPrimitive.Vector Int
evenVP sz = VectorPrimitive.fromListN sz (evenList sz)

randomV :: Int -> IO (Vector.Vector Int)
randomV sz = Vector.fromListN sz <$> randomList sz

randomVU :: Int -> IO (VectorUnboxed.Vector Int)
randomVU sz = VectorUnboxed.fromListN sz <$> randomList sz

randomVS :: Int -> IO (VectorStorable.Vector Int)
randomVS sz = VectorStorable.fromListN sz <$> randomList sz

randomVP :: Int -> IO (VectorPrimitive.Vector Int)
randomVP sz = VectorPrimitive.fromListN sz <$> randomList sz

data Parity
  = Even -- ^ even
  | Odd -- ^ odd
  | Na -- ^ n/a

par :: Parity -> Int -> Bool
par pr = case pr of { Even -> p'; Odd -> p; Na -> p; }
{-# inline par #-}

p, p' :: Int -> Bool
p = odd
p' = even
{-# inline p #-}
{-# inline p' #-}

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
