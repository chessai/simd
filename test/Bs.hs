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

prop_xor :: Property
prop_xor = property $ do
  len <- forAll $ Gen.int (Range.linear 100 1000)
  xs <- forAll $ genPrimArray len genWord8
  ys <- forAll $ genPrimArray len genWord8
  let lhs = Simd.xor (primArrayToByteArray xs) (primArrayToByteArray ys)
  let rhs = primArrayToByteArray (Main.naiveXor xs ys)
  lhs === rhs

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
