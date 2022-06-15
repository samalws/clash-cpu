module CPU.SPI where

import Clash.Prelude

import Control.Monad.State (runState, get, put)
import Data.Maybe (fromMaybe)

-- ss is active low
spi :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom (Maybe (Vec 8 Bit)) -> Signal dom Bool -> (Signal dom Bit, Signal dom Bit, Signal dom Bool, Signal dom (Maybe (Vec 8 Bit)))
spi readWire writeReq readReq = unbundle $ mealy transFunc s0 $ bundle (readWire, writeReq, readReq) where

  -- (data writing, data reading, bits left to write, bits left to read)
  s0 = (repeat 0, repeat 0, 0 :: Index 8, 0 :: Index 8)

  transFunc s i = (s',o) where (o,s') = runState (trans i) s

  trans (readWire, writeReq, readReq) = do
    (_, _, writeLeft, readLeft) <- get
    let active = writeLeft > 0 || readLeft > 0
    let ssWire = bitCoerce (not active)
    let shouldTakeReqs = not active
    writeReqAck <- whenBool shouldTakeReqs (takeWriteReq writeReq)
    readReqAck  <- whenBool (shouldTakeReqs && readReq) takeReadReq
    let reqAck = writeReqAck || readReqAck
    writeWire <- fromMaybe 0 <$> whenMaybe (writeLeft > 0) doWrite
    readData <- whenMaybe (readLeft > 0) (doRead readWire)
    pure (ssWire, writeWire, reqAck, readData)

  takeWriteReq Nothing = pure False
  takeWriteReq (Just dat) = do
    (_, dataReading, _, readLeft) <- get
    put (dat, dataReading, maxBound, readLeft)
    pure True

  takeReadReq = do
    (dataWriting, dataReading, writeLeft, _) <- get
    put (dataWriting, dataReading, maxBound, maxBound)
    pure True

  doWrite = do
    (dataWriting, dataReading, writeLeft, readLeft) <- get
    put (0 +>> dataWriting, dataReading, writeLeft-1, readLeft)
    pure (last dataWriting)

  doRead readWire = do
    (dataWriting, dataReading, writeLeft, readLeft) <- get
    let shifted = dataReading <<+ readWire
    put (dataWriting, shifted, writeLeft, readLeft-1)
    pure shifted

  whenBool False f = pure False
  whenBool True  f = f

  whenMaybe False f = pure Nothing
  whenMaybe True  f = Just <$> f
