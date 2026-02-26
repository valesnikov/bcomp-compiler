module Optimize.Int16
  ( i16Neg,
    i16Asl,
    i16Asr,
    i16Not,
    i16Add,
    i16Sub,
    i16And,
    i16Or,
    i16Cmp,
  )
where

import Data.Bits (Bits (complement, (.&.), (.|.)))
import Data.Int (Int16)

toI16 :: Integer -> Int16
toI16 = fromInteger

fromI16 :: Int16 -> Integer
fromI16 = toInteger

i16Neg :: Integer -> Integer
i16Neg value = fromI16 $ negate $ toI16 value

i16Asl :: Integer -> Integer
i16Asl value = fromI16 $ toI16 value * 2

i16Asr :: Integer -> Integer
i16Asr value = fromI16 $ toI16 value `div` 2

i16Not :: Integer -> Integer
i16Not value = fromI16 $ complement $ toI16 value

i16Add :: Integer -> Integer -> Integer
i16Add lhs rhs = fromI16 $ toI16 lhs + toI16 rhs

i16Sub :: Integer -> Integer -> Integer
i16Sub lhs rhs = fromI16 $ toI16 lhs - toI16 rhs

i16And :: Integer -> Integer -> Integer
i16And lhs rhs = fromI16 $ toI16 lhs .&. toI16 rhs

i16Or :: Integer -> Integer -> Integer
i16Or lhs rhs = fromI16 $ toI16 lhs .|. toI16 rhs

i16Cmp :: (Int16 -> Int16 -> Bool) -> Integer -> Integer -> Bool
i16Cmp cmp lhs rhs = cmp (toI16 lhs) (toI16 rhs)
