module CPU.UART (HzToPeriod, uartTx, uartRx, charToMsg, repeatMsgUartTx, uartEchoFn) where

import qualified Prelude as P
import Clash.Prelude

import CPU.Util (HzToPeriod, rateCountdownBool, fifoBuffer)

data TxUARTstage = TxWaitingInput | TxStartBit (Vec 8 Bit) | TxDataBits (Vec 8 Bit) (Index 8) | TxStopBit deriving (Generic, NFDataX)
uartTx :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Signal dom (Maybe (Vec 8 Bit)) -> (Signal dom Bool, Signal dom Bit)
uartTx baud dat = (ack, otp) where
  (set, ack, otp) = unbundle $ mealy trans TxWaitingInput $ bundle (countdown,dat)
  countdown = rateCountdownBool baud set

  trans TxWaitingInput (True, Just vec) = (TxStartBit (reverse vec), (True, True, 0))
  trans (TxStartBit vec) (True, _) = (TxDataBits vec 0, (True, False, vec !! 0))
  trans (TxDataBits vec n) (True, _)
    | n == 7 = (TxStopBit, (True, False, 1))
    | otherwise = (TxDataBits vec (n+1), (True, False, vec !! (n+1)))
  trans TxStopBit (True, _) = (TxWaitingInput, (False, False, 1))
  trans s _ = (s, (False, False, defaultOtp s))

  defaultOtp TxWaitingInput = 1
  defaultOtp (TxStartBit _) = 0
  defaultOtp (TxDataBits vec n) = vec !! n
  defaultOtp TxStopBit = 1

uartTxBuffered :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud, KnownNat fifoDepth, fifoDepth ~ (decDep + 1)) =>
  SNat fifoDepth -> SNat baud -> Signal dom (Maybe (Vec 8 Bit)) -> Signal dom Bit
uartTxBuffered depth baud dat = otp where
  (ack, otp) = uartTx baud dat'
  dat' = fifoBuffer depth dat ack

data RxUARTstage = RxBeforeStart | RxStartBit | RxDataBits (Vec 8 Bit) (Index 8) | RxStopBit deriving (Generic, NFDataX)

uartRx :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> Signal dom Bool -> Signal dom Bit -> Signal dom (Maybe (Vec 8 Bit))
uartRx baud ack inp = dat where
  (setFast, setSlow, dat) = unbundle $ mealy trans (RxBeforeStart, Nothing) $ bundle (countdownFast,countdownSlow,ack,inp)
  countdownFast = rateCountdownBool (addSNat baud baud) setFast
  countdownSlow = rateCountdownBool baud setSlow

  trans (rxState,datState) (fastIn,slowIn,ackIn,inpIn) = ((rxState', datRx <|> datAck), (setFast,setSlow,datState)) where
    datAck = transAck datState ackIn
    (rxState',datRx,setFast,setSlow) = transRx rxState fastIn slowIn inpIn

  transAck _ True = Nothing
  transAck a False = a

  transRx RxBeforeStart _ _ 0 = (RxStartBit,Nothing,True,False)
  transRx RxStartBit True _ _ = (RxDataBits (repeat 0) 0,Nothing,False,True)
  transRx (RxDataBits vec idx) _ True i | idx /= maxBound = (RxDataBits (replace idx i vec) (idx+1),Nothing,False,True)
  transRx (RxDataBits vec idx) _ True _ = (RxStopBit,Just (reverse vec),False,True)
  transRx RxStopBit _ True _ = (RxBeforeStart,Nothing,False,False)
  transRx s _ _ _ = (s,Nothing,False,False)

charToMsg :: Char -> Vec 8 Bit
charToMsg c = bitCoerce (fromIntegral (fromEnum c) :: Unsigned 8)

repeatMsg :: (HiddenClockResetEnable dom, KnownNat n) => Vec n Char -> Signal dom Bool -> Signal dom (Maybe (Vec 8 Bit))
repeatMsg vec ack = (Just . charToMsg . (vec !!)) <$> counter where
  s0 :: (KnownNat n) => SNat n -> Index n
  s0 _ = 0
  counter = regEn (s0 (lengthS vec)) ack (suc <$> counter)
  suc m = if m == maxBound then 0 else m+1

repeatMsgUartTx :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud, KnownNat n) =>
  SNat baud -> Vec n Char -> Signal dom Bit
repeatMsgUartTx baud vec = otp where
  (ack, otp) = uartTx baud msg
  msg = repeatMsg vec ack

uartEchoFn :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, KnownNat baud, 1 <= period, 1 <= baud) =>
  SNat baud -> (Vec 8 Bit -> Vec 8 Bit) -> Signal dom Bit -> Signal dom Bit
uartEchoFn baud f inp = otp where
  (ack, otp) = uartTx baud (fmap f <$> dat)
  dat = uartRx baud ack inp
