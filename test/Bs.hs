{-# language MagicHash, TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified GHC.Exts as Exts
import qualified Data.Bits as Bits
import qualified Data.Primitive.Contiguous as C

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

primArrayToByteArray :: PrimArray a -> ByteArray
primArrayToByteArray (PrimArray b#) = ByteArray b#

genPrimArray :: Prim a => Int -> Gen a -> Gen (PrimArray a)
genPrimArray sz gen = Exts.fromList <$> Gen.list (Range.singleton sz) gen

genWord64 :: Gen Word64
genWord64 = Gen.word64 Range.constantBounded

genWord8 :: Gen Word8
genWord8 = Gen.word8 Range.constantBounded

naiveXor :: (Prim a, Bits a) => PrimArray a -> PrimArray a -> PrimArray a
naiveXor = C.zipWith Bits.xor

naiveOr :: (Prim a, Bits a) => PrimArray a -> PrimArray a -> PrimArray a
naiveOr = C.zipWith (Bits..|.)

