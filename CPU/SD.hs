module CPU.SD where

import Prelude hiding ((++))
import Clash.Prelude

data InitState = WaitingBegin (Index 74) | SendingSPI CMD (Index 6) | ReadingStatusByte | WaitingStatusByte | WaitingEnd (Index 8) | Done deriving (Generic, NFDataX)
data CMD = CMD0 | CMD55 | ACMD41 deriving (Generic, NFDataX)

cmdToByte :: CMD -> Unsigned 6
cmdToByte CMD0 = 0
cmdToByte CMD55 = 55
cmdToByte ACMD41 = 41

sdInit :: (HiddenClockResetEnable dom, KnownNat n, 48 <= n) => Signal dom (Maybe (Maybe (Vec n Bit))) -> Signal dom Bool -> (Signal dom (Maybe (Vec 8 Bit)), Signal dom (Index n), Signal dom Bool)
sdInit a b = unbundle $ mealy transFunc s0 $ bundle (a,b) where

  s0 = WaitingBegin maxBound

  transFunc s i = (stateFunc s i, otpFunc s)

  stateFunc (WaitingBegin 0) _ = SendingSPI CMD0 0
  stateFunc (WaitingBegin n) _ = WaitingBegin (n-1)
  stateFunc (SendingSPI CMD0 n) (_, True) | n == maxBound = SendingSPI CMD55 0
  stateFunc (SendingSPI CMD55 n) (_, True) | n == maxBound = SendingSPI ACMD41 0
  stateFunc (SendingSPI ACMD41 n) (_, True) | n == maxBound = ReadingStatusByte
  stateFunc (SendingSPI cmd n) (_, True) = SendingSPI cmd (n+1)
  stateFunc ReadingStatusByte (_, True) = WaitingStatusByte
  stateFunc WaitingStatusByte (Just (Just b), _) = if statusBit b then WaitingEnd maxBound else SendingSPI CMD55 0
  stateFunc WaitingStatusByte (Just _, _) = SendingSPI CMD55 0
  stateFunc (WaitingEnd 0) _ = Done
  stateFunc (WaitingEnd n) _ = WaitingEnd (n-1)
  stateFunc s _ = s

  otpFunc (WaitingBegin _) = (Nothing, 0, False)
  otpFunc (SendingSPI cmd 0) = (Just (((0 :: Bit) :> 1 :> Nil) ++ bitCoerce (cmdToByte cmd)), 0, False)
  otpFunc (SendingSPI _ 5) = (Just (bitCoerce (0x95 :: Unsigned 8)), 0, False)
  otpFunc (SendingSPI _ _) = (Just (bitCoerce (0 :: Unsigned 8)), 0, False)
  otpFunc ReadingStatusByte = (Nothing, 48, False)
  otpFunc WaitingStatusByte = (Nothing, 0, False)
  otpFunc (WaitingEnd _) = (Nothing, 0, True)

  statusBit _ = True -- TODO
