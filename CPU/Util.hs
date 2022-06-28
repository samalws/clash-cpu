module CPU.Util (Nybble, Byte, TwoByte, Word, SignedWord, HzToPeriod, rateCountdown, rateCountdownBool, rateEnable, fifoBuffer) where

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

fifoBuffer :: (HiddenClockResetEnable dom, KnownNat fifoDepth, fifoDepth ~ (decDep + 1), NFDataX a) => SNat fifoDepth -> Signal dom (Maybe a) -> Signal dom Bool -> Signal dom (Maybe a)
fifoBuffer depth inpDat inpAck = otpDat where
  otpDat = moore transFn otpFn s0 $ bundle (inpDat, inpAck)
  s0 = (replicate depth Nothing, _0 depth)
  otpFn (b, n) = head b
  transFn s (dat, ack) = pushDat dat $ popWhenAck ack s
  pushDat (Just a) (b, n) | n == maxBound = (b, n)
  pushDat (Just a) (b, n) = (replace n (Just a) b, n+1)
  pushDat Nothing s = s
  popWhenAck True (b, n) = (b <<+ Nothing, if n == 0 then 0 else n-1)
  popWhenAck False s = s
  _0 :: (KnownNat n) => SNat n -> Index (n+1)
  _0 _ = 0
