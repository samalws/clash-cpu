module CPU.SDSPI (spi) where

import Clash.Prelude

import Control.Monad.State (runState, get, put)
import Data.Maybe (fromMaybe)

-- spi interface
-- due to the way it reads, this is only for use with SD cards
-- ss is active low
spi :: (HiddenClockResetEnable dom, KnownNat n) => Signal dom Bit -> Signal dom (Maybe (Vec 8 Bit)) -> Signal dom (Index n) -> (Signal dom Bit, Signal dom Bit, Signal dom Bool, Signal dom (Maybe (Maybe (Vec n Bit))))
spi readWire writeReq readReq = unbundle $ mealy transFunc s0 $ bundle (readWire, writeReq, readReq) where

  -- (data writing, data reading, bits left to write, bits left to read, read countdown timer)
  s0 = (repeat 0, Nothing, 0 :: Index 8, 0, 0 :: Index 100)

  transFunc s i = (s',o) where (o,s') = runState (trans i) s

  trans (readWire, writeReq, readReq) = do
    (_, _, writeLeft, readLeft, readCountdown) <- get
    let active = writeLeft > 0 || readLeft > 0
    let ssWire = bitCoerce (not active)
    let shouldTakeReqs = not active
    writeReqAck <- whenBool shouldTakeReqs (takeWriteReq writeReq)
    readReqAck  <- whenBool shouldTakeReqs (takeReadReq readReq)
    let reqAck = writeReqAck || readReqAck
    writeWire <- fromMaybe 1 <$> whenMaybe (writeLeft > 0) doWrite
    readData <- whenMaybe (readLeft > 0) (doRead readWire)
    pure (ssWire, writeWire, reqAck, readData)

  takeWriteReq Nothing = pure False
  takeWriteReq (Just dat) = do
    (_, dataReading, _, readLeft, readCountdown) <- get
    put (dat, dataReading, maxBound, readLeft, readCountdown)
    pure True

  takeReadReq 0 = pure False
  takeReadReq n = do
    (dataWriting, dataReading, writeLeft, _, _) <- get
    put (dataWriting, dataReading, writeLeft, n, maxBound)
    pure True

  doWrite = do
    (dataWriting, dataReading, writeLeft, readLeft, readCountdown) <- get
    put (0 +>> dataWriting, dataReading, writeLeft-1, readLeft, readCountdown)
    pure (Just (last dataWriting))

  doRead readWire = do
    (dataWriting, dataReading, writeLeft, readLeft, readCountdown) <- get
    let shifted = maybe (if readWire == 0 then Just (repeat 0) else Nothing) (Just . (<<+ readWire)) dataReading
    case (readLeft == 1, shifted, readCountdown == 1) of
      (True, _, _) -> put (dataWriting, Nothing, writeLeft, 0, 0) >> pure (Just shifted)
      (False, Just _, _) -> put (dataWriting, shifted, writeLeft, readLeft-1, 0) >> pure Nothing
      (False, Nothing, True) -> put (dataWriting, Nothing, writeLeft, 0, 0) >> pure (Just Nothing)
      (False, Nothing, False) -> put (dataWriting, Nothing, writeLeft, maxBound, readCountdown-1) >> pure Nothing

  whenBool False f = pure False
  whenBool True  f = f

  whenMaybe False f = pure Nothing
  whenMaybe True  f = f
