module CPU.Util (Nybble, Byte, TwoByte, Word, SignedWord, HzToPeriod, rateCountdown, rateCountdownBool, rateEnable) where

import Clash.Prelude hiding (Word)

type Nybble = Unsigned 4
type Byte = Unsigned 8
type TwoByte = Unsigned 16
type Word = Unsigned 64
type SignedWord = Signed 64

type HzToPeriod (baud :: Nat) = (10 ^ 12) `Div` baud

rateCountdown :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Signal dom Bool -> Signal dom (Index (HzToPeriod baud `Div` period))
rateCountdown _ set = r where
  r = register maxBound (f <$> set <*> r)
  f s n
    | s = maxBound
    | n == 0 = 0
    | otherwise = n-1

rateCountdownBool :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Signal dom Bool -> Signal dom Bool
rateCountdownBool baud set = (0 ==) <$> rateCountdown baud set

rateEnable :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Enable dom
rateEnable baud = toEnable sig where sig = rateCountdownBool baud sig
