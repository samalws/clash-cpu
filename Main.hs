import qualified Prelude as P
import Clash.Prelude hiding (read, Word)

import CPU.Core
import CPU.Assembler

topEntity' :: HiddenClockResetEnable dom => (Signal dom Bool, Signal dom Bool) -> (Signal dom Bool, Signal dom Bool)
topEntity' inp = unbundle otp where
  (readAddr, maybeWrite) = unbundle $ cpuRamRom (SNat :: SNat 100) (SNat :: SNat 100000) "meminit.bin" $(memBlobTH Nothing (P.replicate 100000 (0 :: BitVector 8))) cpuInput
  cpuInput = register (Nothing, False) $ genCpuInp <$> readAddr <*> bundle inp
  genCpuInp 0xFFF3 (i,_) = (Just (if i then 1 else 0), False)
  genCpuInp 0xFFF7 (_,i) = (Just (if i then 1 else 0), False)
  genCpuInp _ _ = (Nothing, False)
  otp = regMaybe (True,True) (genOtpRegVal <$> otp <*> maybeWrite)
  genOtpRegVal (a,b) (Just (0xFFF3, v)) = Just (v==0,b)
  genOtpRegVal (a,b) (Just (0xFFF7, v)) = Just (a,v==0)
  genOtpRegVal _ _ = Nothing

createDomain vSystem{vName="DomMain", vPeriod=20000}

topEntity :: Clock DomMain -> (Signal DomMain Bool, Signal DomMain Bool) -> (Signal DomMain Bool, Signal DomMain Bool)
topEntity clk btns = withClockResetEnable clk resetGen enableGen topEntity' btns

toAssemble :: Assembler ()
toAssemble = do
  lit A 6 0
  lit A 7 1
  lit A 8 0xFFF0
  lit A 9 0xFFF4
  beginning <- label
  write A 8 6
  write A 9 6
  read A 0 8
  read A 1 9
  write LE 8 7
  swap A 0 1 2
  write LE 9 7
  jmpK A beginning

otpAssembly :: IO ()
otpAssembly = writeFile "meminit.bin" $ assemblyToFile $ padAssembly 100 $ runAssembler toAssemble
