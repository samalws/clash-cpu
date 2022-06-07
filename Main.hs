import qualified Prelude as P
import Clash.Prelude hiding (read, Word)

import Data.Maybe (isJust, fromMaybe)

import CPU.Core
import CPU.UART
import CPU.Assembler

resizeCoerce :: (BitPack a, BitPack b) => a -> b
resizeCoerce = unpack . resize . pack

topEntity' :: (HiddenClockResetEnable dom, DomainPeriod dom ~ period, KnownNat period, 1 <= period) =>
  Signal dom Bit -> Signal dom (Vec 4 Bool) -> (Signal dom Bit, Signal dom (Vec 4 Bool))
topEntity' rx inp = (tx, otpLeds) where
  (readAddr, maybeWrite) = unbundle $ cpuRamRom
    (SNat @100)
    (SNat @1 {- @100000 -})
    "meminit.bin"
    $(memBlobTH Nothing (P.replicate 1 {- 100000 -} (0 :: BitVector 8)))
    cpuInput

  cpuInput = register (Nothing, False) $ genCpuInp <$> readAddr <*> rxDat <*> inp

  genCpuInp 0xFFF7 _ i = (Just $ resizeCoerce $ not <$> i, False)
  genCpuInp 0xFFFE r _ = (Just $ resizeCoerce $ isJust r, False)
  genCpuInp 0xFFFF r _ = (Just $ bitCoerce $ fromMaybe (repeat 0) r, False)
  genCpuInp _ _ _ = (Nothing, False)

  otpLeds = regMaybe (repeat True) (genOtpLedRegVal <$> maybeWrite)

  genOtpLedRegVal (Just (0xFFF7, v)) = Just $ fmap not $ resizeCoerce v
  genOtpLedRegVal _ = Nothing

  txDat = regMaybe Nothing $ genTxDatRegVal <$> txAck <*> maybeWrite

  genTxDatRegVal _ (Just (0xFFFF, v)) = Just $ Just $ bitCoerce v
  genTxDatRegVal True _ = Just Nothing
  genTxDatRegVal _ _ = Nothing

  (txAck, tx) = uartTx (SNat @9600) txDat
  rxDat = uartRx (SNat @9600) ((0xFFFF ==) <$> readAddr) rx

createDomain vSystem{vName="DomMain", vPeriod=20000}

topEntity :: Clock DomMain -> Signal DomMain Bit -> Signal DomMain (Vec 4 Bool) -> (Signal DomMain Bit, Signal DomMain (Vec 4 Bool))
topEntity clk rx btns = withClockResetEnable clk resetGen enableGen topEntity' rx btns

repeatBtnsLeds :: Assembler ()
repeatBtnsLeds = do
  lit A 8 0xFFF0
  lit A 9 0xF
  beginning <- label
  read A 2 8
  bxor A 2 9
  write A 8 2
  jmpK A beginning

uartEcho :: Assembler ()
uartEcho = do
  lit A 0 1
  lit A 8 0xFFF8
  lit A 9 0x100
  beginning <- label
  read A 2 8
  mov A 1 2
  band A 1 9
  write LE 8 2
  jmpK A beginning

otpAssembly :: IO ()
otpAssembly = writeFile "meminit.bin" $ assemblyToFile $ padAssembly 100 $ runAssembler uartEcho
