module CPU.SD where

import Clash.Prelude

import CPU.SDSPI (spi)
import CPU.Types (Byte)

data InitState = WaitingBegin (Index 74) | ISendingSPI ICMD (Index 6) | ReadingStatusByte | WaitingStatusByte | WaitingEnd (Index 8) | Done deriving (Generic, NFDataX)
data ICMD = CMD0 | CMD55 | ACMD41 deriving (Generic, NFDataX)

icmdToByte :: ICMD -> Unsigned 6
icmdToByte CMD0 = 0
icmdToByte CMD55 = 55
icmdToByte ACMD41 = 41

sdInit :: (HiddenClockResetEnable dom, KnownNat n, 48 <= n) =>
  Signal dom (Maybe (Maybe (Vec n Bit))) -> Signal dom Bool -> (Signal dom (Maybe (Vec 8 Bit)), Signal dom (Index n), Signal dom Bool)
sdInit a b = unbundle $ mealy transFunc s0 $ bundle (a,b) where

  s0 = WaitingBegin maxBound

  transFunc s i = (stateFunc s i, otpFunc s) -- TODO moore? I think it's off-by-one

  stateFunc (WaitingBegin 0) _ = ISendingSPI CMD0 0
  stateFunc (WaitingBegin n) _ = WaitingBegin (n-1)
  stateFunc (ISendingSPI CMD0 n) (_, True) | n == maxBound = ISendingSPI CMD55 0
  stateFunc (ISendingSPI CMD55 n) (_, True) | n == maxBound = ISendingSPI ACMD41 0
  stateFunc (ISendingSPI ACMD41 n) (_, True) | n == maxBound = ReadingStatusByte
  stateFunc (ISendingSPI cmd n) (_, True) = ISendingSPI cmd (n+1)
  stateFunc ReadingStatusByte (_, True) = WaitingStatusByte
  stateFunc WaitingStatusByte (Just (Just b), _) = if statusBit b then WaitingEnd maxBound else ISendingSPI CMD55 0
  stateFunc WaitingStatusByte (Just _, _) = ISendingSPI CMD55 0
  stateFunc (WaitingEnd 0) _ = Done
  stateFunc (WaitingEnd n) _ = WaitingEnd (n-1)
  stateFunc s _ = s

  otpFunc (WaitingBegin _) = (Nothing, 0, False)
  otpFunc (ISendingSPI cmd 0) = (Just (((0 :: Bit) :> 1 :> Nil) ++ bitCoerce (icmdToByte cmd)), 0, False)
  otpFunc (ISendingSPI _ 5) = (Just (bitCoerce (0x95 :: Unsigned 8)), 0, False)
  otpFunc (ISendingSPI _ _) = (Just (bitCoerce (0 :: Unsigned 8)), 0, False)
  otpFunc ReadingStatusByte = (Nothing, 48, False)
  otpFunc WaitingStatusByte = (Nothing, 0, False)
  otpFunc (WaitingEnd _) = (Nothing, 0, True)

  statusBit _ = True -- TODO

data OperationStateMain maxReadBits maxWriteBurstLen
  = OSendingSPI OCMD (Unsigned 32) (Index 6) (OperationState maxReadBits maxWriteBurstLen)
  | OperationState (OperationState maxReadBits maxWriteBurstLen)
  deriving (Generic, NFDataX)
data OperationState maxReadBits maxWriteBurstLen
  = AwaitingCmd
  | ReadBits (Index maxReadBits)
  | ReadingBits
  | SendingBytes (Vec maxWriteBurstLen Byte) (Index maxWriteBurstLen)
  deriving (Generic, NFDataX)
data OCMD = CMD16 | CMD17 | CMD24 deriving (Generic, NFDataX)

ocmdToByte :: OCMD -> Unsigned 6
ocmdToByte CMD16 = 16
ocmdToByte CMD17 = 17
ocmdToByte CMD24 = 24

sdOperation :: (HiddenClockResetEnable dom, KnownNat maxReadBits, KnownNat maxWriteBurstLen, KnownNat maxReadBurstLen, KnownNat maxBurstLen, KnownNat maxAddr, (maxReadBurstLen * 8) ~ maxReadBits, 1 <= maxBurstLen, maxWriteBurstLen ~ (1+decMwbl), maxWriteBurstLen <= maxBurstLen, maxReadBurstLen <= maxBurstLen) =>
  Signal dom (Maybe (Maybe (Vec maxReadBits Bit))) -> Signal dom Bool -> Signal dom (Maybe (Index maxBurstLen)) -> Signal dom (Maybe (Index maxAddr, Vec maxWriteBurstLen Byte)) -> Signal dom (Maybe (Index maxAddr))
  -> (Signal dom (Maybe (Vec 8 Bit)), Signal dom (Index maxReadBits), Signal dom Bool, Signal dom Bool, Signal dom (Maybe (Vec maxReadBurstLen Byte)))
sdOperation a b c d e = unbundle $ mealy transFunc s0 $ bundle (a,b,c,d,e) where

  -- input: spiByteRead, spiReqAck, sdBurstLenReq, sdWriteReq, sdReadReq
  -- output: spiWriteReq, spiReadReq, sdBurstLenAck, sdWriteAck, sdReadAck

  s0 = (0, OperationState AwaitingCmd)

  transFunc s i = (stateFunc s i, otpFunc (snd s) i) -- TODO moore? I think it's off-by-one

  stateFunc (blen, OperationState AwaitingCmd) (_,_,Just blen',_,_)      = (blen', OSendingSPI CMD16 (fromIntegral blen) 0 AwaitingCmd)
  stateFunc (blen, OperationState AwaitingCmd) (_,_,_,Just (addr,vec),_) = (blen,  OSendingSPI CMD17 (fromIntegral addr) 0 (SendingBytes vec (fromIntegral blen)))
  stateFunc (blen, OperationState AwaitingCmd) (_,_,_,_,Just addr)       = (blen,  OSendingSPI CMD24 (fromIntegral addr) 0 (ReadBits (fromIntegral blen * 8)))
  stateFunc (blen, OperationState (ReadBits _)) (_,True,_,_,_)           = (blen,  OperationState ReadingBits)
  stateFunc (blen, OperationState ReadingBits) (Just _,_,_,_,_)          = (blen,  OperationState AwaitingCmd)
  stateFunc (blen, OperationState (SendingBytes vec n)) (_,True,_,_,_)
    | n == 0    = (blen, OperationState AwaitingCmd)
    | otherwise = (blen, OperationState (SendingBytes (0 +>> vec) (n-1)))
  stateFunc (blen, OSendingSPI cmd args n after) (_,True,_,_,_)
    | n == maxBound = (blen, OperationState after)
    | otherwise     = (blen, OSendingSPI cmd args (n+1) after)
  stateFunc s _ = s

  otpFunc (OperationState AwaitingCmd)     (_,_,Just _,_,_)         = (Nothing, 0,    True, False, Nothing)
  otpFunc (OperationState AwaitingCmd)     (_,_,_,Just _,_)         = (Nothing, 0,    False, True, Nothing)
  otpFunc (OperationState AwaitingCmd)     _                        = (Nothing, 0,    False, False, Nothing)
  otpFunc (OperationState (ReadBits blen)) _                        = (Nothing, blen, False, False, Nothing)
  otpFunc (OperationState ReadingBits)     (Just Nothing,_,_,_,_)   = (Nothing, 0,    False, False, Nothing)
  otpFunc (OperationState ReadingBits)     (Just (Just bs),_,_,_,_) = (Nothing, 0,    False, False, Just (bitCoerce bs))
  otpFunc (OperationState ReadingBits)     _                        = (Nothing, 0,    False, False, Nothing)
  otpFunc (OperationState (SendingBytes bs _)) _                    = (Just (bitCoerce (last bs)),                                    0, False, False, Nothing)
  otpFunc (OSendingSPI cmd args 0 _)       _                        = (Just (((0 :: Bit) :> 1 :> Nil) ++ bitCoerce (ocmdToByte cmd)), 0, False, False, Nothing)
  otpFunc (OSendingSPI cmd args 5 _)       _                        = (Just (bitCoerce (0x95 :: Unsigned 8)),                         0, False, False, Nothing)
  otpFunc (OSendingSPI cmd args n _)       _                        = (Just ((bitCoerce args :: Vec 4 (Vec 8 Bit)) !! (n-1)),         0, False, False, Nothing)
