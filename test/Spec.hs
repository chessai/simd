{-# language BangPatterns, MagicHash, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified GHC.Exts as Exts
import qualified Data.Bits as Bits
import qualified Data.Primitive.Contiguous as C

import Control.Monad.ST
import Data.Bits (Bits)
import Data.Primitive
import Data.Word

import qualified Simd

main :: IO Bool
main = checkSequential $$(discover)

binary :: ()
  => (ByteArray -> ByteArray -> ByteArray)
  -> (PrimArray Word8 -> PrimArray Word8 -> PrimArray Word8)
  -> Property
binary simd naive = property $ do
  (xs,ys) <- forAll $ do
    len <- Gen.int (Range.linear 100 1000)
    xs <- genPrimArray len genWord8
    ys <- genPrimArray len genWord8
    pure (xs, ys)
  let lhs = simd (primArrayToByteArray xs) (primArrayToByteArray ys)
  let rhs = primArrayToByteArray (naive xs ys)
  lhs === rhs

prop_xor :: Property
prop_xor = binary Simd.xor Main.naiveXor

prop_or :: Property
prop_or = binary Simd.or Main.naiveOr

prop_and :: Property
prop_and = binary Simd.and Main.naiveAnd

prop_nand :: Property
prop_nand = binary Simd.nand Main.naiveNand

prop_equal8 :: Property
prop_equal8 = property $ do
  byte <- forAll genWord8
  arr <- forAll $ primArrayToByteArray <$> genPrimArray 1000 genWord8
  Simd.equal byte arr === Main.naiveEqual byte arr

prop_equal16 :: Property
prop_equal16 = property $ do
  byte <- forAll genWord16
  arr <- forAll $ primArrayToByteArray <$> genPrimArray 1000 genWord16
  Simd.equal byte arr === Main.naiveEqual byte arr

primArrayToByteArray :: PrimArray a -> ByteArray
primArrayToByteArray (PrimArray b#) = ByteArray b#

genPrimArray :: Prim a => Int -> Gen a -> Gen (PrimArray a)
genPrimArray sz gen = Exts.fromList <$> Gen.list (Range.singleton sz) gen

genWord64 :: Gen Word64
genWord64 = Gen.word64 Range.constantBounded

genWord16 :: Gen Word16
genWord16 = Gen.word16 Range.constantBounded

genWord8 :: Gen Word8
genWord8 = Gen.word8 Range.constantBounded

type Bin a
  = (Prim a, Bits a) => PrimArray a -> PrimArray a -> PrimArray a

naiveXor :: Bin a
naiveXor = C.zipWith Bits.xor

naiveOr :: Bin a
naiveOr = C.zipWith (Bits..|.)

naiveAnd :: Bin a
naiveAnd = C.zipWith (Bits..&.)

naiveNand :: Bin a
naiveNand = C.zipWith (\x y -> Bits.complement (x Bits..&. y))

naiveEqual :: (Prim a, Eq a) => a -> ByteArray -> ByteArray
naiveEqual byte arr = runST run where
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
