import qualified Prelude as P
import Clash.Prelude

type HzToPeriod (baud :: Nat) = (10 ^ 12) `Div` baud

uartCountdown :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Signal dom Bool -> Signal dom (Index (HzToPeriod baud `Div` period))
uartCountdown _ set = r where
  r = register maxBound (f <$> set <*> r)
  f s n
    | s = maxBound
    | n == 0 = 0
    | otherwise = n-1

uartCountdownBool :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Signal dom Bool -> Signal dom Bool
uartCountdownBool baud set = (== 0) <$> uartCountdown baud set

data UARTstage = BeforeStart (Index 100) | WaitingInput | StartBit (Vec 8 Bit) | DataBits (Vec 8 Bit) (Index 8) | StopBit deriving (Generic, NFDataX)

uart :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Signal dom (Maybe (Vec 8 Bit)) -> (Signal dom Bool, Signal dom Bit)
uart baud dat = (ack, otp) where
  (set, ack, otp) = unbundle $ mealy trans (BeforeStart maxBound) $ bundle (dat,countdown)
  countdown = uartCountdownBool baud set

  trans (BeforeStart n) (_, True)
    | n == 0 = (WaitingInput, (False, False, 1))
    | otherwise = (BeforeStart (n-1), (True, False, 1))
  trans WaitingInput (Just vec, True) = (StartBit (reverse vec), (True, True, 0))
  trans (StartBit vec) (_, True) = (DataBits vec 0, (True, False, vec !! 0))
  trans (DataBits vec n) (_, True)
    | n == 7 = (StopBit, (True, False, 1))
    | otherwise = (DataBits vec (n+1), (True, False, vec !! (n+1)))
  trans StopBit (_, True) = (BeforeStart maxBound, (False, False, 1))
  trans s _ = (s, (False, False, defaultOtp s))

  defaultOtp (BeforeStart _) = 1
  defaultOtp WaitingInput = 1
  defaultOtp (StartBit _) = 0
  defaultOtp (DataBits vec n) = vec !! n
  defaultOtp StopBit = 1

charToMsg :: Char -> Vec 8 Bit
charToMsg c = bitCoerce (fromIntegral (fromEnum c) :: Unsigned 8)

repeatMsg :: (HiddenClockResetEnable dom, KnownNat n) => Vec n Char -> Signal dom Bool -> Signal dom (Maybe (Vec 8 Bit))
repeatMsg vec ack = (Just . charToMsg . (vec !!)) <$> counter where
  s0 :: (KnownNat n) => SNat n -> Index n
  s0 _ = 0
  counter = regEn (s0 (lengthS vec)) ack (suc <$> counter)
  suc m = if m == maxBound then 0 else m+1

repeatMsgUart :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud, KnownNat n) =>
  SNat baud -> Vec n Char -> Signal dom Bit
repeatMsgUart baud vec = otp where
  (ack, otp) = uart baud msg
  msg = repeatMsg vec ack

createDomain vSystem{vName="DomMain", vPeriod=20000}

topEntity :: Clock DomMain -> Signal DomMain Bit
topEntity clk = withClockResetEnable clk resetGen enableGen $ repeatMsgUart (SNat @9600) $(listToVecTH "Hello world\n")
